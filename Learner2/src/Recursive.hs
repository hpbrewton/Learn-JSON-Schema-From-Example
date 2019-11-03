{-# LANGUAGE OverloadedStrings #-}
module Recursive (
    adjacency,
    infiniteObject
) where 

import Data.Text
import Data.HashMap.Strict as HM
import Data.HashSet as HS
import Test.QuickCheck as Gen 
import Schema 
import Data.Aeson
import Control.Monad
import Util

infiniteObject :: Schema -> Schema
infiniteObject (DefinitionSchema t m) = getSchema m (RefSchema t)

getSchema :: HM.HashMap Text Schema -> Schema -> Schema
getSchema m (RefSchema schema) = getSchema m (m HM.! schema) 
getSchema m (SchemaWithOracle schema _) = getSchema m schema
getSchema m (ArraySchema schema lb ub) = ArraySchema (getSchema m schema) lb ub 
getSchema m (TupleSchema schemas) = TupleSchema $ (fmap (getSchema m)) schemas
getSchema m (ObjectSchema schemas req) = ObjectSchema (fmap (getSchema m) schemas) req 
getSchema m (AnyOf schemas) = AnyOf (fmap (getSchema m) schemas)
getSchema m other = other

getOracle :: HM.HashMap Text Schema -> Text -> (Value -> IO Bool)
getOracle m t = (\(SchemaWithOracle _ o) -> o) (m HM.! t)

contains :: (Value -> IO Bool)  -> Schema -> IO Bool 
contains o s = do 
    examples <- Gen.generate $ Gen.vectorOf 10 $ arbitraryFromSchema s
    fmap and $ sequence $ fmap o examples

updateRefs :: (Text -> Text) -> Schema -> Schema
updateRefs f (DefinitionSchema t m) = DefinitionSchema (f t) $ HM.map (updateRefs f) m
updateRefs f (SchemaWithOracle s o) = SchemaWithOracle (updateRefs f s) o 
updateRefs f StringSchema = StringSchema
updateRefs f BooleanSchema = BooleanSchema 
updateRefs f NullSchema = NullSchema
updateRefs f (NumberSchema a b) = (NumberSchema a b)
updateRefs f (ArraySchema a lb ub) = ArraySchema (updateRefs f a) lb ub
updateRefs f (AnyOf as) = AnyOf (fmap (updateRefs f) as)
updateRefs f (TupleSchema as) = TupleSchema (fmap (updateRefs f) as)
updateRefs f (ObjectSchema object req) = ObjectSchema (fmap (updateRefs f) object) req 
updateRefs f (RefSchema ref) = RefSchema (f ref)

adjacency :: Schema -> IO Schema
adjacency (DefinitionSchema t m) = do 
    fPairs <- filterM (\(b, l) -> (getOracle m b) `contains` (getSchema m (RefSchema l))) pairs
    let preEdges = (HM.!) $ Prelude.foldr lister HM.empty fPairs
    let postEdges = (HM.!) $ Prelude.foldr lister HM.empty $ fmap swap fPairs
    let bisim a b = ((preEdges a) == (preEdges b)) && ((postEdges a) == (postEdges b))
    let classes = Prelude.zip (fmap showT [1..]) $ equivalenceClass bisim (HM.keys m) 
    let updater = (HM.!) $ HM.fromList [(r, r') | (r', rs) <- classes, r <- rs]
    let switches = HM.map (AnyOf . fmap RefSchema) $ HM.fromList classes
    let (DefinitionSchema t' olds) = updateRefs updater $ DefinitionSchema t m
    let switchesPushed = HM.map (pushRefs olds) switches
    return $ DefinitionSchema t' switchesPushed
    -- return $ DefinitionSchema "" $ HM.map (AnyOf . fmap RefSchema) $ HM.fromList classes
    where 
        pairs = [(a, b) | a <- HM.keys m, b <- HM.keys m]
        lister :: (Text, Text) -> HM.HashMap Text [Text] -> HM.HashMap Text [Text]
        lister (l, r) m 
            | l `HM.member` m = HM.adjust (r:) l m 
            | otherwise = HM.insert l [r] m

        pushRefs defs (DefinitionSchema t m) = DefinitionSchema t $ HM.map (pushRefs defs) m
        pushRefs defs (SchemaWithOracle s o) = SchemaWithOracle (pushRefs defs s) o 
        pushRefs defs StringSchema = StringSchema
        pushRefs defs BooleanSchema = BooleanSchema 
        pushRefs defs NullSchema = NullSchema
        pushRefs defs (NumberSchema a b) = (NumberSchema a b)
        pushRefs defs (ArraySchema a lb ub) = ArraySchema (pushRefs defs a) lb ub
        pushRefs defs (AnyOf as) = AnyOf (fmap (pushRefs defs) as)
        pushRefs defs (TupleSchema as) = TupleSchema (fmap (pushRefs defs) as)
        pushRefs defs (ObjectSchema object req) = ObjectSchema (fmap (pushRefs defs) object) req 
        pushRefs defs (RefSchema ref) = defs HM.! ref







    