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

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LazyBS
import Data.Ratio

-- import Debug.Trace

import Recursive
import Network.Wreq
import Control.Lens
import Data.Time
import qualified Data.HashMap.Strict as HM
import Util

getStart :: IO Value 
getStart = do 
    response <- get "http://localhost:3000/start"
    case decode $ (response ^. responseBody) of 
        Nothing -> error "could not parse start"
        Just v -> return v

oracle :: IORef [UTCTime] -> Value -> IO Bool 
oracle refTimes value = do 
    times <- readIORef refTimes
    times' <- fmap (:times) getCurrentTime
    response <- post "http://localhost:3000/member" value 
    case decode $ (response ^. responseBody) of 
        Nothing -> error "could not parse response of oracle"
        Just (Bool b) -> do
            times'' <- fmap (:times') getCurrentTime
            b' <- atomicModifyIORef refTimes (\_ -> (times'', b))
            return b'
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
    dummy <- newIORef []
    acceptedExamples <- filterM (oracle dummy) ourExamples
    let p = ((fromIntegral $ length acceptedExamples) / fromIntegral n)
    return p

recall :: Int -> Schema -> IO Double
recall n schema = do 
    theirExamples <- sequence $ take n $ repeat example
    let acceptedExamples = Prelude.filter (Schema.member schema) theirExamples
    -- putStrLn $ show (Prelude.filter (not . Schema.member schema) theirExamples)
    let r = ((fromIntegral $ length acceptedExamples) / (fromIntegral $ length (theirExamples)))
    return r

timeSince :: UTCTime -> IO NominalDiffTime
timeSince timeA = fmap (`diffUTCTime` timeA) getCurrentTime



main :: IO ()
main = do 
    timeA <- getCurrentTime
    start <- getStart
    let schema = schemaFromValue start 
    ref <- newIORef []
    aschema <- independentLearner schema (oracle ref)
    let bschema = flatten "" aschema
    finalStrict <- adjacency bschema
    p <- precision 10 $ infiniteObject finalStrict
    r <- recall 10 $ infiniteObject finalStrict
    ns <- readIORef ref 
    let accumTime = Util.evens $ tail $ zipWith diffUTCTime ns (tail ns)
    putStrLn $ printf "%f\t%f\t%d\t%s" (1.0 :: Double) r ((length ns) `quot` 2) (show $ sum accumTime)