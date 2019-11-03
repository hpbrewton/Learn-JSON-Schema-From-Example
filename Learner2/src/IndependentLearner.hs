module IndependentLearner (
    independentLearner
) where

import Schema
import Data.Aeson 
import qualified Test.QuickCheck as Gen 
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HM
import Util


independentLearner :: Schema -> (Value -> IO Bool) -> IO Schema
independentLearner StringSchema oracle = 
    return (SchemaWithOracle StringSchema oracle)
independentLearner BooleanSchema oracle = 
    return (SchemaWithOracle BooleanSchema oracle)
independentLearner NullSchema oracle = 
    return (SchemaWithOracle NullSchema oracle)
independentLearner (NumberSchema a b) oracle = do
    (a', b') <- range (lowerBound, upperBound) (orElse a lowerBound, orElse b upperBound) (oracle . Number)
    return (SchemaWithOracle (NumberSchema (Just a') (Just b')) oracle)
independentLearner (TupleSchema schemas) oracle = do 
    (Array example) <- Gen.generate $ arbitraryFromSchema $ TupleSchema schemas
    -- can this be a list 
    schemasRotate <- (fmap and $ sequence $  fmap (oracle . Array) $ rotations example) 
    let isList = ((not . null) schemas) && schemasRotate

    -- now we learn about the children
    let oracles = Vec.map (oracle.) $ Vec.map (Array.) $ slotterVec example
    newSchemas <- sequence $ fmap (\(s, o) -> independentLearner s o) $ Vec.zip schemas oracles
    if isList
        then do 
            let listSizeOracle = \n -> oracle $ Array $ Vec.fromList $ take n $ repeat $ Vec.head example
            (lb, ub) <- range (lowerBound, upperBound) (length schemas, length schemas) listSizeOracle
            return $ SchemaWithOracle (ArraySchema (AnyOf $ Vec.toList $ newSchemas) lb $ Just ub) oracle
            -- return $ SchemaWithOracle (ArraySchema (SchemaWithOracle (AnyOf $ Vec.toList $ newSchemas) (\v -> oracle $ Array $ (v `Vec.cons` Vec.tail example))) lb $ Just ub) oracle
        else return (SchemaWithOracle (TupleSchema newSchemas) oracle)
independentLearner (ObjectSchema object req) oracle = do 
    (Object example) <- Gen.generate $ arbitraryFromSchema $ ObjectSchema object req 
    
    -- first we learn which of the required are actually required
    req' <- filterIOHSet (\k -> fmap not $ oracle $ Object $ HM.delete k example) req 

    -- now we learn about the children
    let oracles = HM.mapWithKey (\k _ -> (\v -> oracle $ Object $ HM.insert k v example)) object
    newSchemas <- sequence $ HM.mapWithKey (\k v -> independentLearner v (oracles HM.! k)) object 
    return (SchemaWithOracle (ObjectSchema newSchemas req') oracle) 