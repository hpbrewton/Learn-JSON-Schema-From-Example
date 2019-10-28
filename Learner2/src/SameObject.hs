{-# LANGUAGE OverloadedStrings #-}
module SameObject (
    replaceWithBisimulations
) where

import Schema
import Util
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Test.QuickCheck as Gen
import qualified Data.Vector as Vec
import Generalizer
import Control.Monad

import Debug.Trace

lessThanOrEqual :: Schema -> T.Text -> T.Text -> IO Bool
lessThanOrEqual defSchema@(DefinitionSchema _ schemaMap) left right = do 
    examples <- Gen.generate $ Gen.vectorOf 10 $ arbitraryFromSchema $ getFromDefinitionMap defSchema left 
    let (SchemaWithOracle _ oracle) = (schemaMap HM.! right)
    fmap and $ sequence $ fmap oracle examples

createSubSchemaGraph :: Schema -> IO (T.Text -> T.Text -> Bool)
createSubSchemaGraph defSchema@(DefinitionSchema _ schemas) = do 
    let schemaMapList = HM.keys schemas
    let pairs = [(a,b) | a <- schemaMapList, b <- schemaMapList]
    pairsSet <- fmap HS.fromList $ filterM (\(a, b) -> lessThanOrEqual defSchema a b) pairs
    return $ \left right -> ((left, right) `HS.member` pairsSet )


bisimulate :: (Foldable f, Eq (f T.Text)) => (T.Text -> T.Text -> Bool) -> f T.Text -> T.Text -> T.Text -> Bool 
bisimulate r exh a b = (preEdgesA == preEdgesB) && (postEdgesA == postEdgesB)
    where 
        toFilteredList :: (Foldable g) => (a -> Bool) -> g a -> [a]
        toFilteredList p = foldr (\a b -> if p a 
            then a:b 
            else b) []
        preEdgesA = toFilteredList (flip r a) exh
        preEdgesB = toFilteredList (flip r b) exh 
        postEdgesA = toFilteredList (r a) exh 
        postEdgesB = toFilteredList (r b) exh 

bisimulateSet :: Schema -> IO (HS.HashSet (T.Text, T.Text))
bisimulateSet defSchema@(DefinitionSchema _ schemas) = do 
    relation <- createSubSchemaGraph defSchema 
    let operation = bisimulate relation (HM.keys schemas)
    let allKeys = HM.keys schemas
    return $ HS.fromList [(a, b) | a <- allKeys, b <- allKeys, bisimulate relation allKeys a b]

replaceWithBisimulations :: Schema -> IO Schema
replaceWithBisimulations defSchema@(DefinitionSchema top schemas) = do 
    pairsSet <- bisimulateSet defSchema
    let classes = equivalenceClass (\a b -> HS.member (a, b) pairsSet) $ HM.keys schemas
    let updater = updateReference classes
    -- return (updater requiredTop, refUpdater updater defSchema)
    let (DefinitionSchema _ newSchemas) = refUpdater updater defSchema
    return $ AnyOf $ fmap (flip DefinitionSchema newSchemas) $ updater top
    where 
        reps :: [[T.Text]] -> HM.HashMap T.Text [T.Text]
        reps cs = HM.fromList $ concat [[(c, traceShow (keepers clss) $ keepers clss) | c <- clss] | clss <- (traceShow cs cs)] 
        updateReference :: [[T.Text]] -> T.Text -> [T.Text]
        updateReference cs = ((reps cs) HM.!)
        keepers :: [T.Text] -> [T.Text]
        keepers xs = (head xs) : others
            where 
                others = filter (\r -> case (schemas HM.! r) of (SchemaWithOracle NullSchema _) -> True ; _ -> False) xs 
