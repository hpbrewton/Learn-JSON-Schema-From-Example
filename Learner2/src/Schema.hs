{-# LANGUAGE OverloadedStrings #-}
module Schema (
    Schema (..),
    arbitraryFromSchema,
    schemaFromValue,
    order,
    getFromDefinitionMap,
    member,
    schemaFromDefinition,
    rootLevel,
    refUpdater
    ) where 

import qualified Data.HashMap.Strict as HM 
import qualified Data.HashSet as HS
import Data.Aeson
import Data.Text 
import qualified Data.Vector as Vec
import Data.Scientific
import Test.QuickCheck as Gen
import Test.QuickCheck.Arbitrary
import Util
import Text.Printf
import Data.Maybe
import Control.Monad

-- import Debug.Trace

type Oracle = Value -> IO Bool
type Definitions = HM.HashMap Text Schema

data Schema = StringSchema -- to be replace with regular expresion
    | SingStringSchema Text 
    | BooleanSchema -- a stub 
    | NullSchema -- a stub
    | NumberSchema (Maybe Scientific) (Maybe Scientific) -- min max 
    | ArraySchema Schema Int (Maybe Int) -- body, minimum len maximum len 
    | TupleSchema (Vec.Vector Schema) -- schema must match in this order
    | ObjectSchema (HM.HashMap Text Schema) (HS.HashSet Text) -- properies, required 
    | RefSchema Text 
    | AnyOf [Schema]
    | DefinitionSchema Text Definitions 
    | SchemaWithOracle Schema Oracle  -- not available as a producable oracle, but used as an internal step
    | Any 

instance Show Schema where 
    show (SingStringSchema string) = printf "<%s>" (show string)
    show StringSchema = "StringSchema"
    show BooleanSchema = "BooleanSchema"
    show NullSchema = "NullSchema"
    show (NumberSchema a b) = printf "NumberSchema %s %s" (show a) (show b) 
    show (ArraySchema sch lb ub) = printf "ArraySchema (%s) %s %s"  (show sch) (show lb) (show ub)
    show (TupleSchema vec) = printf "TupleSchema %s" (show vec)
    show (ObjectSchema object set) = printf "ObjectSchema %s %s" (show object) (show set) 
    show (RefSchema text) = printf "RefSchema %s" (show text) 
    show (AnyOf schemas) = printf "AnyOf %s" (show schemas)
    show (DefinitionSchema top rest) = printf "Definitions %s %s" (show top) (show rest)
    show (SchemaWithOracle schema _) = printf "~%s" (show schema)
    show (Any) = printf "any"

type RootSchema = (HM.HashMap Text Schema) 

schemaFromValue :: Value -> Schema
schemaFromValue Null = NullSchema
schemaFromValue (Bool _) = BooleanSchema
schemaFromValue (String string) = SingStringSchema string
schemaFromValue (Number n) = NumberSchema (Just n) (Just n)
schemaFromValue (Object obj) = ObjectSchema (HM.map schemaFromValue obj) (HS.fromList $ HM.keys obj)
schemaFromValue (Array vec) = TupleSchema $ Vec.map schemaFromValue vec

{-
StringSchema -- to be replace with regular expresion
BooleanSchema -- a stub 
NullSchema -- a stub
NumberSchema (Maybe Scientific) (Maybe Scientific) -- min max 
ArraySchema Schema Int (Maybe Int) -- body, minimum len maximum len 
TupleSchema (Vec.Vector Schema) -- schema must match in this order
ObjectSchema (HM.HashMap Text Schema) (HS.HashSet Text) -- properies, required 
RefSchema Text 
AnyOf [Schema]
DefinitionSchema Text Definitions 
SchemaWithOracle Schema Oracle 
-}

rootLevel :: Value -> Schema 
rootLevel (Object obj) = (DefinitionSchema "." definitionsWithTop)
    where 
        top = schemaFromDefinition $ Object obj
        definitions = case HM.lookup "definitions" obj of 
            Just (Object obj) -> mapTextKeys ("#/definitions/" <>) $ HM.map schemaFromDefinition obj 
            Nothing -> HM.empty
        definitionsWithTop = HM.insert "." top definitions

schemaFromDefinition :: Value -> Schema
schemaFromDefinition Null = NullSchema
schemaFromDefinition (Object obj) = case HM.lookup "type" obj of 
    Just "string" -> case HM.lookup "enum" obj of 
        Just (Array vs) -> if (1 == Vec.length vs) 
            then case (Vec.head vs) of 
                String v -> SingStringSchema v 
                otherwise -> (error "this should have been a string")
            else (error "don't know how to handle non-unit enum... ermm... um... yet")
        Nothing -> StringSchema
    Just "null" -> NullSchema
    Just "boolean" -> BooleanSchema
    Just "number" ->  NumberSchema (fmap toNumber (HM.lookup "minimum" obj)) (fmap toNumber (HM.lookup "maximum" obj))
    Just "array" -> case HM.lookup "items" obj of 
        (Just v) -> ArraySchema (schemaFromDefinition v) (fromJust $ toInt (HM.lookupDefault (Number 0) "minItems" obj)) (join $ fmap toInt (HM.lookup "maxItems" obj)) 
        Nothing -> ArraySchema Any (fromJust $ toInt (HM.lookupDefault (Number 0) "minItems" obj)) (join $ fmap toInt (HM.lookup "maxItems" obj))  
    Just "object" -> case (HM.lookup "properties" obj, HM.lookup "required" obj) of 
        (Nothing, Nothing) -> ObjectSchema HM.empty HS.empty
        (Just (Object properties), Nothing) -> ObjectSchema (HM.map schemaFromDefinition properties) HS.empty 
        (Nothing, Just (Array vector)) -> ObjectSchema HM.empty $ HS.fromList $ Vec.toList $ Vec.map (\(String txt) -> txt) vector
        (Just (Object properties), Just (Array vector)) -> ObjectSchema (HM.map schemaFromDefinition properties) (HS.fromList $ Vec.toList  $ Vec.map (\(String txt) -> txt) vector)
        _ -> error "ill formed schema"
    Just (Array tuple) -> TupleSchema $ fmap schemaFromDefinition tuple
    Nothing -> case HM.lookup "$ref" obj of 
        Just (String ref) -> RefSchema ref 
        Just _ -> error "ill formed ref"
        Nothing -> case HM.lookup "anyOf" obj of 
            Just (Array examples) -> AnyOf $ Vec.toList $ fmap schemaFromDefinition examples
            Nothing -> case HM.lookup "oneOf" obj of 
                Just (Array examples) -> AnyOf $ Vec.toList $ fmap schemaFromDefinition examples
                Nothing -> error "we had had a problem"
    Just v -> error $ show v
    where 
        toInt :: Value -> Maybe Int 
        toInt (Number n) = toBoundedInteger n 
        toInt otw = error "could not conver to integer"

        toNumber :: Value -> Scientific
        toNumber (Number n) = n 
        toNumber _ = error "could not convert to number" 

order :: Int -> Schema -> Schema -> IO (Maybe Ordering) 
order n = \(SchemaWithOracle sch1 orc1) (SchemaWithOracle sch2 orc2) -> do 
    exs1 <- generate $ Gen.resize n $ Gen.listOf $ arbitraryFromSchema sch1
    exs2 <- generate $ Gen.resize n $ Gen.listOf $ arbitraryFromSchema sch2 
    r2lt1 <- fmap Prelude.and $ sequence $ fmap orc1 exs2
    r1lt2 <- fmap Prelude.and $ sequence $ fmap orc2 exs1 
    case (r1lt2, r2lt1) of 
        (True, True) -> return $ Just EQ
        (True, False) -> return $ Just LT 
        (False, True) -> return $ Just GT
        (False, False) -> return $ Nothing 

member :: Schema -> Value -> Bool
member (SingStringSchema a) (String b) = a == b
member StringSchema (String _) = True 
member BooleanSchema (Bool _) = True
member NullSchema Null = True 
member (NumberSchema lb ub) (Number num) = lbOk && ubOk
    where 
        lbOk = case lb of (Just v) -> num >= v ; _ -> True  
        ubOk = case ub of (Just v) -> num <= v ; _ -> True 
member (ArraySchema schema lb ub) (Array vector) = allMembers && lbOk && ubOk
    where 
        allMembers = and $ fmap (member schema) vector
        lbOk = lb <= Vec.length vector
        ubOk = case ub of 
            Just v -> v >= Vec.length vector
            Nothing -> True 
member (TupleSchema schemas) (Array vector) = sameSize && elemsMatch
    where 
        sameSize = (Vec.length schemas) == (Vec.length vector)
        elemsMatch = and $ Vec.zipWith member schemas vector
member (ObjectSchema object required) (Object m) = requiredKeysIn && childrenMatch
    where 
        requiredKeysIn = and $ HS.map (flip HM.member m) required
        childrenMatch = and $ HM.elems $ HM.intersectionWith member object m 
member (AnyOf schemas) v = result
    where 
        result = or $ fmap (flip member v) schemas
member sch@(DefinitionSchema top rest) v = member (getFromDefinitionMap sch top) v
member Any _ = True 
member schema value = False

arbitraryFromSchema :: Schema -> Gen.Gen Value
arbitraryFromSchema (SingStringSchema str) = return $ String str 
arbitraryFromSchema StringSchema = return $ String "??"
arbitraryFromSchema (BooleanSchema) = do 
    bool <- Gen.elements [True, False]
    return $ Bool bool 
arbitraryFromSchema (NullSchema) = return Null 
arbitraryFromSchema (NumberSchema lb ub) = do 
    let jlb = Util.orElse lb (-1000)
    let jub = Util.orElse ub (1000)
    fmap Number $ genScientificInRange jlb jub
    where 
        genScientificInRange :: Scientific -> Scientific -> Gen.Gen Scientific
        genScientificInRange a b 
            | (b-a) < epsilon = return $ averge a b 
            | otherwise = do 
                let m = averge a b 
                low <- arbitrary
                if low
                    then genScientificInRange a m 
                    else genScientificInRange m b
arbitraryFromSchema (ArraySchema schema jlb ub) = do 
    let jub = Util.orElse ub 10
    n <- choose (jlb, jub)
    fmap Array $ fmap Vec.fromList $ sized (\size -> Gen.vectorOf (max jlb (min n size)) $ scale scaler $ arbitraryFromSchema schema)
    where 
        scaler = (`quot` 2)
arbitraryFromSchema (TupleSchema schema) = fmap Array $ sequence $ fmap arbitraryFromSchema schema
arbitraryFromSchema (ObjectSchema object required) = do 
    let optional = (HM.keysSet object) `HS.difference` required
    rejected <- Gen.sublistOf $ HS.toList optional
    let selectedSubObject = Prelude.foldr (HM.delete) object rejected
    fmap Object $ sequence $ HM.map arbitraryFromSchema selectedSubObject 
arbitraryFromSchema (RefSchema text) = error "can not generate an arbitrary member from a ref"
arbitraryFromSchema (AnyOf schemas) = Gen.oneof $ fmap arbitraryFromSchema schemas
arbitraryFromSchema (SchemaWithOracle schema _) = arbitraryFromSchema schema
arbitraryFromSchema Any = arbitraryFromSchema NullSchema
arbitraryFromSchema other = error $ show other

getFromDefinitionMap :: Schema -> Text -> Schema
getFromDefinitionMap (DefinitionSchema _ m) = getWithDefinitionMap
    where 
        getWithDefinitionMap :: Text -> Schema 
        getWithDefinitionMap = continuer . (\schema -> case schema of (SchemaWithOracle subSchema _) -> subSchema ; _ -> schema) . (m `errorLookup`)
            where 
                continuer (SingStringSchema str) = (SingStringSchema str)
                continuer StringSchema = StringSchema 
                continuer BooleanSchema = BooleanSchema
                continuer NullSchema = NullSchema
                continuer s@(NumberSchema _ _) = s 
                continuer (ArraySchema schema lb ub) = (ArraySchema (continuer schema) lb ub)
                continuer (TupleSchema vector) = TupleSchema $ Vec.map continuer vector
                continuer (ObjectSchema object required) = ObjectSchema (HM.map continuer object) required
                continuer (RefSchema ref) = getWithDefinitionMap ref 
                continuer (AnyOf schemas) = AnyOf $ fmap continuer schemas 

                continuer other = error $ show other

refUpdater :: (Text -> [Text]) -> Schema -> Schema
refUpdater updater (ArraySchema schema lb ub) = ArraySchema (refUpdater updater schema) lb ub 
refUpdater updater (TupleSchema schemas) = TupleSchema $ fmap (refUpdater updater) schemas
refUpdater updater (ObjectSchema schemas req) = ObjectSchema (fmap (refUpdater updater) schemas) req 
refUpdater updater (RefSchema ref) = AnyOf $ fmap RefSchema $ updater ref 
refUpdater updater (AnyOf schemas) = AnyOf $ fmap (refUpdater updater) schemas 
refUpdater updater (DefinitionSchema top defs) = DefinitionSchema top $ HM.filterWithKey (\k _ -> (k `elem` updater k)) $ fmap (refUpdater updater) defs 
refUpdater updater (SchemaWithOracle schema oracle) = SchemaWithOracle (refUpdater updater schema) oracle
refUpdater _ other = other 
