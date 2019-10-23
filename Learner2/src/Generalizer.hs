{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Generalizer (
    Generalizer,
    generalizeBottomUp,
    combine
) where 

import Debug.Trace
import Data.Aeson 
import Schema
import Util
import qualified Data.Vector as Vec
import qualified Test.QuickCheck as Gen
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type Generalizer = Schema -> (Value -> IO Bool) -> IO Schema

combine :: Generalizer -> Generalizer -> Generalizer
combine genA genB = \schema oracle -> do 
    afterB <- genB schema oracle
    genA afterB oracle

generalizeBottomUp :: Generalizer -> Generalizer
generalizeBottomUp generalizer = \schema oracle -> 
    case schema of 
        StringSchema -> generalizer StringSchema oracle
        BooleanSchema -> generalizer BooleanSchema oracle
        NullSchema -> generalizer NullSchema oracle
        NumberSchema _ _ -> generalizer schema oracle 
        ArraySchema sch lb mub -> do 
            sch' <- (generalizeBottomUp generalizer) sch oracle 
            generalizer (ArraySchema sch' lb mub) oracle
        TupleSchema schemas -> do 
            (Array vector) <- Gen.generate $ arbitraryFromSchema (TupleSchema schemas)
            let oracles = Vec.map (\slotter value -> oracle $ Array $ slotter $ value) $ Util.slotterVec vector 
            childGeneralized <- fmap TupleSchema $ sequence $ Vec.zipWith (generalizeBottomUp generalizer) schemas oracles
            generalizer childGeneralized oracle
        ObjectSchema object required -> do 
            arbObject <- sequence $ HM.map (Gen.generate . arbitraryFromSchema) object
            genObject <- sequence $ HM.mapWithKey (\k v -> (generalizeBottomUp generalizer) v (oracle . Object . (flip (HM.insert k) arbObject))) object  
            generalizer (ObjectSchema genObject required) oracle
        RefSchema ref -> generalizer (RefSchema ref) oracle
        AnyOf schemas -> do 
            childGeneralized <- sequence $ map (flip (generalizeBottomUp generalizer) oracle) schemas
            generalizer (AnyOf childGeneralized) oracle
        SchemaWithOracle schema oracle' -> do 
            -- putStrLn $ "okay we're with dealing schema oracles"
            childGeneralized <- (generalizeBottomUp generalizer) schema oracle
            generalizer (SchemaWithOracle childGeneralized oracle') oracle
        _-> error $ show schema
