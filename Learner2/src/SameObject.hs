{-# LANGUAGE OverloadedStrings #-}
module SameObject (
    sameObjectGeneralize
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

isObjectOrNull :: Schema -> Bool
isObjectOrNull (SchemaWithOracle (ObjectSchema object required) oracle) = True 
isObjectOrNull (SchemaWithOracle NullSchema oracle) = True
isObjectOrNull _ = False


sameObjectGeneralize :: Schema -> IO Schema
sameObjectGeneralize defs@(DefinitionSchema top schemas) = do 
    let labelMaps = foldr (\l -> HM.insert l HS.empty) HM.empty $ map fst allLabels
    equalPairs <- equalPairsIO 
    let anyOfs = HM.map (AnyOf . (map RefSchema) . HS.toList ) $ foldr (\(l1, l2) m -> HM.adjust (HS.insert l1) l2 m) labelMaps  equalPairs
    return $ DefinitionSchema top $ HM.unionWith (\(AnyOf items) (SchemaWithOracle current oracle) -> (SchemaWithOracle (AnyOf (current:items)) oracle)) anyOfs schemas
    where 
        sim :: Int -> ((T.Text, Schema), (T.Text, Schema)) -> IO Bool 
        sim n ((l1, (SchemaWithOracle _ o1)), (l2, SchemaWithOracle _ o2)) = do 
            let arb1 = arbitraryFromSchema $ getFromDefinitionMap defs l1 
            fmap and $ join $ fmap sequence $ fmap (map o2) $ Gen.generate $ Gen.resize n $ Gen.listOf1 arb1
        allLabels = HM.toList $ HM.filter isObjectOrNull schemas
        pairsOfKVOs = uniquePairs allLabels
        equalPairsIO :: IO [(T.Text, T.Text)]
        equalPairsIO = fmap (filter (\(a, b) ->  a/=b)) $ fmap (fmap (\(a, b) -> (fst a, fst b))) $ filterM (sim 10) pairsOfKVOs
