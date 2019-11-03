{-# LANGUAGE OverloadedStrings #-}
module MainDriver (
    main 
) where

import Data.Aeson
import Schema
import Flatten
import Test.QuickCheck as Gen
import Control.Monad
import qualified Data.Vector as Vec
import Text.Printf
import IndependentLearner

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LazyBS
import Data.Ratio

import Debug.Trace

import Recursive
import Network.Wreq
import Control.Lens
import Data.Time
import qualified Data.HashMap.Strict as HM

getStart :: IO Value 
getStart = do 
    response <- get "http://localhost:3000/start"
    case decode $ (response ^. responseBody) of 
        Nothing -> error "could not parse start"
        Just v -> return v

oracle :: Value -> IO Bool 
oracle value = do 
    response <- post "http://localhost:3000/member" value 
    case decode $ (response ^. responseBody) of 
        Nothing -> error "could not parse response of oracle"
        Just (Bool b) -> return b 
        _ -> error "was expecting a boolean from oracle"

example :: IO Value
example = do 
    response <- get "http://localhost:3000/example"
    case decode $ (response ^. responseBody) of 
        Nothing -> error "could not parse example"
        Just v -> return v

precision :: Int -> Schema -> IO Double
precision n schema = do 
    ourExamples <- Gen.generate $ Gen.vectorOf n $ Gen.resize 10 $ arbitraryFromSchema $ schema
    acceptedExamples <- filterM oracle ourExamples
    let p = ((fromIntegral $ length acceptedExamples) / fromIntegral n)
    return p

recall :: Int -> Schema -> IO Double
recall n schema = do 
    theirExamples <- sequence $ take n $ repeat example
    let acceptedExamples = Prelude.filter (Schema.member schema) theirExamples
    putStrLn $ show (Prelude.filter (not . Schema.member schema) theirExamples)
    let r = ((fromIntegral $ length acceptedExamples) / (fromIntegral $ length (theirExamples)))
    return r

main :: IO ()
main = do 
    start <- getStart
    let schema = schemaFromValue start 
    aschema <- independentLearner schema oracle
    let bschema = flatten "" aschema
    finalStrict <- adjacency bschema
    putStrLn $ show finalStrict
    p <- precision 10 $ infiniteObject finalStrict
    r <- recall 10 $ infiniteObject finalStrict
    putStrLn $ printf "%f\t%f" (1.0 :: Double) r