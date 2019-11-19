{-# LANGUAGE OverloadedStrings #-}
module Recursive (
    adjacency,
    infiniteObject
) where 

import Data.Text hiding (groupBy)
import Data.HashMap.Strict as HM
import Data.HashSet as HS
import Test.QuickCheck as Gen 
import Schema 
import Data.Aeson
import Control.Monad
import Util
import Data.List (nub, groupBy)

import Debug.Trace

infiniteObject :: Schema -> Schema
infiniteObject (DefinitionSchema t m) = getSchema m (RefSchema t)

getSchema :: HM.HashMap Text Schema -> Schema -> Schema
getSchema m (RefSchema schema) = getSchema m (m `Util.errorLookup` schema) 
getSchema m (SchemaWithOracle schema _) = getSchema m schema
getSchema m (ArraySchema schema lb ub) = ArraySchema (getSchema m schema) lb ub 
getSchema m (TupleSchema schemas) = TupleSchema $ (fmap (getSchema m)) schemas
getSchema m (ObjectSchema schemas req) = ObjectSchema (fmap (getSchema m) schemas) req 
getSchema m (AnyOf schemas) = AnyOf (fmap (getSchema m) schemas)
getSchema m other = other

getOracle :: HM.HashMap Text Schema -> Text -> (Value -> IO Bool)
getOracle m t = (\(SchemaWithOracle _ o) -> o) (m `Util.errorLookup` t)

contains :: (Value -> IO Bool)  -> Schema -> IO Bool 
contains o s = do 
    examples <- Gen.generate $ Gen.vectorOf 10 $ arbitraryFromSchema s
    arrayChecks  <- case s of 
        (ArraySchema sch l Nothing) -> do 
            v <- Gen.generate $ arbitraryFromSchema $ (ArraySchema sch l (Just l))
            return $ return v
        (ArraySchema sch l (Just u)) -> do 
            l <- Gen.generate $ arbitraryFromSchema $ (ArraySchema sch l (Just l))
            b <- Gen.generate $ arbitraryFromSchema $ (ArraySchema sch u (Just u))
            return [l, b]
        otherwise -> return []
    fmap and $ sequence $ fmap o (arrayChecks ++ examples)

updateRefs :: (Text -> Text) -> Schema -> Schema
updateRefs f (DefinitionSchema t m) = DefinitionSchema (f t) $ HM.map (updateRefs f) m
updateRefs f (SchemaWithOracle s o) = SchemaWithOracle (updateRefs f s) o 
updateRefs f (SingStringSchema s) = (SingStringSchema s)
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

    let preEdges = Util.errorLookup $ Prelude.foldr lister HM.empty fPairs
    let postEdges = Util.errorLookup $ Prelude.foldr lister HM.empty $ fmap swap fPairs
    let bisim a b = ((preEdges a) == (preEdges b)) && ((postEdges a) == (postEdges b))
    let classes = Prelude.zip (fmap showT [1..]) $ equivalenceClass bisim (HM.keys m)
    -- putStrLn $ show classes
    -- -- let susim a b = ((b `elem` ))
    -- let supsim a b = (b `subseteq` preEdges (Prelude.head $ HS.toList a)) && (a `subseteq` preEdges (Prelude.head $ HS.toList b))
    -- let tops = groupBy (\a b -> (fst a) == (fst b)) [(n,m) | (n, as) <- classes, (m, bs) <- classes, n /= m, supsim (HS.fromList as) (HS.fromList bs)]
    -- let repPairs = fmap Prelude.head $ Prelude.filter ((==1) . Prelude.length) $ (fixed compressed) tops
    -- putStrLn $ show repMap

    -- let mappedClasses = HM.fromList classes
    -- let revMap = HM.fromList $ fmap swap repPairs
    -- let mergedClasses = fmap (\(k, vs) -> case (HM.lookup k revMap) of (Just r) -> (k, vs ++ (mappedClasses HM.! r)); Nothing -> (k, vs)) classes

    -- let supsimrep v = case (HM.lookup v $ HM.fromList repPairs) of (Just r) -> r ; Nothing -> v 

    let updater = Util.errorLookup $ HM.fromList [(r, r') | (r', rs) <- classes, r <- rs]
    let switches = HM.map (AnyOf . fmap RefSchema) $ HM.fromList classes
    let (DefinitionSchema t' olds) = updateRefs updater $ DefinitionSchema t m
    let switchesPushed = HM.map (pushRefs olds) switches

    -- within :: (Eq a) => [a] -> [a] -> Bool 
    -- let within as = Prelude.foldr (\v -> (&& (v `elem` as))) True 
    -- also, if some set of schemas are exactly equal, 
    -- reveal this
    -- let eqMap = HM.fromList [(k2,k1) | (k1, (AnyOf as)) <- HM.toList switchesPushed, (k2, (AnyOf bs)) <- HM.toList switchesPushed, k1 /= k2, within as bs]
    return $ nubValues (DefinitionSchema t' switchesPushed)
    -- return $ DefinitionSchema "" $ HM.map (AnyOf . fmap RefSchema) $ HM.fromList classes
    where 
        pairs = [(a, b) | a <- HM.keys m, b <- HM.keys m]
        lister :: (Text, Text) -> HM.HashMap Text (HS.HashSet Text) -> HM.HashMap Text (HS.HashSet Text)
        lister (l, r) m 
            | l `HM.member` m = HM.adjust (HS.insert r) l m 
            | otherwise = HM.insert l (HS.singleton r) m

        pushRefs defs (DefinitionSchema t m) = DefinitionSchema t $ HM.map (pushRefs defs) m
        pushRefs defs (SchemaWithOracle s o) = SchemaWithOracle (pushRefs defs s) o 
        pushRefs defs (SingStringSchema s) = (SingStringSchema s)
        pushRefs defs StringSchema = StringSchema
        pushRefs defs BooleanSchema = BooleanSchema 
        pushRefs defs NullSchema = NullSchema
        pushRefs defs (NumberSchema a b) = (NumberSchema a b)
        pushRefs defs (ArraySchema a lb ub) = ArraySchema (pushRefs defs a) lb ub
        pushRefs defs (AnyOf as) = AnyOf (fmap (pushRefs defs) as)
        pushRefs defs (TupleSchema as) = TupleSchema (fmap (pushRefs defs) as)
        pushRefs defs (ObjectSchema object req) = ObjectSchema (fmap (pushRefs defs) object) req 
        pushRefs defs (RefSchema ref) = defs `Util.errorLookup` ref

        nubValues (DefinitionSchema t m) = DefinitionSchema t $ HM.map (nubValues) m
        nubValues (SchemaWithOracle s o) = SchemaWithOracle (nubValues s) o 
        nubValues (SingStringSchema s) = (SingStringSchema s)
        nubValues StringSchema = StringSchema
        nubValues BooleanSchema = BooleanSchema 
        nubValues NullSchema = NullSchema
        nubValues (NumberSchema a b) = (NumberSchema a b)
        nubValues (ArraySchema a lb ub) = ArraySchema (nubValues a) lb ub
        nubValues (AnyOf as) = AnyOf (nub $ fmap (nubValues) as)
        nubValues (TupleSchema as) = TupleSchema (fmap (nubValues) as)
        nubValues (ObjectSchema object req) = ObjectSchema (fmap (nubValues) object) req 
        nubValues (RefSchema ref) = (RefSchema ref)


        compressed xs = xs'
            where 
                replaces = HM.fromList $ fmap Prelude.head $ Prelude.filter ((==1) . Prelude.length) xs 
                xs' = fmap (Prelude.filter (\v -> fst v /= snd v) . fmap ((\(h, v) -> case HM.lookup v replaces of Just r -> (h, r) ; Nothing -> (h, v)))) xs







    