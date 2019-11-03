{-# LANGUAGE OverloadedStrings #-}
module ScalarGeneralize (
    scalarGeneralize
) where

import Schema
import Util
import Data.Aeson
import Data.Scientific
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Test.QuickCheck as Gen
import qualified Data.Vector as Vec
import Generalizer

upperBoundGeneralize :: Scientific -> (Value -> IO Bool) -> IO (Maybe Scientific)
upperBoundGeneralize sci _ = return $ return sci

lowerBoundGeneralize :: Scientific -> (Value -> IO Bool) -> IO (Maybe Scientific)
lowerBoundGeneralize sci _ = return $ return sci 

scalarGeneralize :: Generalizer 
scalarGeneralize StringSchema oracle = return $ StringSchema
scalarGeneralize BooleanSchema oracle = return $ BooleanSchema
scalarGeneralize NullSchema oracle = return $ NullSchema
scalarGeneralize (NumberSchema mlb mub) oracle = case (mlb, mub) of 
    (Nothing, Nothing) -> return $ NumberSchema Nothing Nothing
    (Nothing, (Just ub)) -> fmap (NumberSchema Nothing) $ upperBoundGeneralize ub oracle 
    ((Just lb), Nothing) -> fmap (flip NumberSchema Nothing) $ lowerBoundGeneralize lb oracle
    ((Just lb), (Just ub)) -> do 
        glb <- lowerBoundGeneralize lb oracle
        gub <- upperBoundGeneralize ub oracle
        return $ NumberSchema glb gub
-- scalarGeneralize sch@(ArraySchema child lb ub) oracle = if ub == Just 0 
--     then return sch 
--     else do 
--         (Array example) <- Gen.generate $ arbitraryFromSchema sch 
--         scalarGeneralize child (\v -> oracle $ (v:example))
scalarGeneralize sch@(TupleSchema schemas) oracle = do 
    (Array example) <- Gen.generate $  arbitraryFromSchema sch 
    results <- sequence $ map (oracle . Array) $ rotations example
    if and results && (not . null) results
        then return $ ArraySchema (AnyOf $ Vec.toList schemas) 0 (Just $ length schemas)
        else return $ TupleSchema schemas
scalarGeneralize (ObjectSchema object required) oracle = do
    let maybeUnrequired = (HM.keysSet object) `HS.difference` required
    arbObject <- sequence $ HM.map (Gen.generate . arbitraryFromSchema) object
    genRequired <- Util.filterIOHSet (fmap not . oracle . Object . (flip HM.delete arbObject)) maybeUnrequired
    return $ ObjectSchema object genRequired
scalarGeneralize sch@(RefSchema _) oracle = return sch 
scalarGeneralize sch@(AnyOf _) oracle = return sch 
