{-# LANGUAGE OverloadedStrings #-}
module MainDriver (
    main 
) where

import Data.IORef
import Data.Aeson
import Schema
import Test.QuickCheck as Gen
import Control.Monad
import qualified Data.Vector as Vec
import Text.Printf
import Recursive
import Data.Ratio
import ServerTeacher

-- import Debug.Trace
-- import Debug.Trace.LocationTH


import Data.Time
import Teacher
import PathTeacher
import qualified Data.HashMap.Strict as HM
import Util

precision :: (Value -> IO Bool) -> Int -> Schema -> IO (Ratio Int)
precision oracle n schema = do 
    examples <- Gen.generate $ Gen.vectorOf n $ Gen.resize 10 $ arbitraryFromSchema schema
    results <- sequence $ map oracle examples
    -- putStrLn $ show $ encode $ head examples
    return $ ((length $ filter id results) % (length examples))

recall :: (IO Value) -> Int -> Schema -> IO (Ratio Int) 
recall example n schema = do 
    examples <- sequence $ [example | _ <- [1..n]]
    let results = filter (member schema) examples
    -- putStrLn $ show $ encode $ head $ filter (not . member schema) examples
    return $ ((length $ results) % (length examples))

main :: IO ()
main = do 
    -- let teacher = PathTeacher "data" 14
    let teacher = ServerTeacher "http://localhost:8080/"
    finalStrict <- learn teacher
    putStrLn $ show finalStrict
    oracleT <- oracle teacher
    p <- precision oracleT 10 $ infiniteObject finalStrict
    r <- recall (example teacher) 10 $ infiniteObject finalStrict
    putStrLn $ printf "%s\t%s" (show p) (show r) -- ((length ns) `quot` 2) (show $ sum accumTime)