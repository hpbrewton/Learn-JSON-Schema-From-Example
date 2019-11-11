{-# LANGUAGE OverloadedStrings #-}
module MainDriver (
    main 
) where

import Data.IORef
import Data.Aeson
import Schema
import Flatten
import Test.QuickCheck as Gen
import Control.Monad
import qualified Data.Vector as Vec
import Text.Printf
import IndependentLearner

import Data.Ratio

-- import Debug.Trace
-- import Debug.Trace.LocationTH


import Recursive
import Data.Time
import Teacher
import qualified Data.HashMap.Strict as HM
import Util

precision :: (Value -> IO Bool) -> Int -> Schema -> IO (Ratio Int)
precision oracle n schema = do 
    examples <- Gen.generate $ Gen.vectorOf n $ Gen.resize 10 $ arbitraryFromSchema schema
    results <- sequence $ map oracle examples
    return $ ((length $ filter id results) % (length examples))

recall :: (IO Value) -> Int -> Schema -> IO (Ratio Int) 
recall example n schema = do 
    examples <- sequence $ [example | _ <- [1..n]]
    let results = map (member schema) examples
    return $ ((length $ filter id results) % (length examples))

main :: IO ()
main = do 
    let teacher = PathTeacher "data" 14
    tStart <- start teacher
    let schema = schemaFromValue tStart 
    tOracle <- oracle teacher
    aschema <- independentLearner schema tOracle
    let bschema = flatten "" aschema
    finalStrict <- adjacency bschema
    putStrLn $ show finalStrict
    p <- precision tOracle 10 $ infiniteObject finalStrict
    r <- recall (example teacher) 10 $ infiniteObject finalStrict
    putStrLn $ printf "%s\t%s" (show p) (show r) -- ((length ns) `quot` 2) (show $ sum accumTime)