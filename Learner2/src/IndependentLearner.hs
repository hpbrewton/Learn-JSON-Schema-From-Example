{-# LANGUAGE OverloadedStrings #-}
module IndependentLearner (
    independentLearner
) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as Vec
import           Schema
import qualified Test.QuickCheck     as Gen
import           Util


independentLearner :: Schema -> (Value -> IO Bool) -> IO Schema
independentLearner (SingStringSchema str) oracle = do
    b <- oracle $ String $ "zabadoo" <> str -- change this to a random prefix
    if b
        then return $ SchemaWithOracle StringSchema oracle
        else return $ SchemaWithOracle (SingStringSchema str) oracle
independentLearner StringSchema oracle =
    return (SchemaWithOracle StringSchema oracle)
independentLearner BooleanSchema oracle =
    return (SchemaWithOracle BooleanSchema oracle)
independentLearner NullSchema oracle =
    return (SchemaWithOracle NullSchema oracle)
independentLearner (NumberSchema a b) oracle = do
    (a', b') <- range (lowerBound, upperBound) (orElse a lowerBound, orElse b upperBound) (oracle . Number)
    let ma = if (a' - epsilon < lowerBound) then Nothing else Just a' 
    let mb = if (b' + epsilon > upperBound) then Nothing else Just b'
    return (SchemaWithOracle (NumberSchema ma mb) oracle)
independentLearner (TupleSchema schemas) oracle = do
    (Array example) <- Gen.generate $ arbitraryFromSchema $ TupleSchema schemas
    -- first we learn about the children
    let oracles = Vec.map (oracle.) $ Vec.map (Array.) $ slotterVec example
    newSchemas <- sequence $ fmap (\(s, o) -> independentLearner s o) $ Vec.zip schemas oracles

    -- can this be a list
    {-
    In order to check we rotate the schemas,
    generate a whole bunch of examples, 
    and then we see if we they are all accepted
    -}
    let rotatedTuple = if Vec.null newSchemas 
        then TupleSchema newSchemas
        else TupleSchema ((Vec.tail newSchemas) <> Vec.singleton (Vec.head newSchemas))
    examples <- Gen.generate $ Gen.vectorOf 10 $ arbitraryFromSchema rotatedTuple
    isList <- fmap and $ sequence $ map oracle examples
    if isList && ((not . null) example)
        then do
            let listSizeOracle = \n -> oracle $ Array $ Vec.fromList $ take n $ repeat $ Vec.head example
            (lb, ub) <- range (lowerBound, upperBound) (length schemas, length schemas) listSizeOracle
            -- return (SchemaWithOracle (TupleSchema newSchemas) oracle)
            let mub = if (ub == upperBound - 1) then Nothing else (Just ub)
            return $ SchemaWithOracle (ArraySchema (AnyOf $ Vec.toList $ newSchemas) lb mub) oracle
            -- return $ SchemaWithOracle (ArraySchema (SchemaWithOracle (AnyOf $ Vec.toList $ newSchemas) (\v -> oracle $ Array $ (v `Vec.cons` Vec.tail example))) lb $ Just ub) oracle
        else return (SchemaWithOracle (TupleSchema newSchemas) oracle)
independentLearner (ObjectSchema object req) oracle = do
    (Object example) <- Gen.generate $ arbitraryFromSchema $ ObjectSchema object req

    -- first we learn which of the required are actually required
    req' <- filterIOHSet (\k -> fmap not $ oracle $ Object $ HM.delete k example) req

    -- now we learn about the children
    let oracles = HM.mapWithKey (\k _ -> (\v -> oracle $ Object $ HM.insert k v example)) object
    newSchemas <- sequence $ HM.mapWithKey (\k v -> independentLearner v (oracles `Util.errorLookup` k)) object
    return (SchemaWithOracle (ObjectSchema newSchemas req') oracle)
