{-# LANGUAGE OverloadedStrings #-}
module FullGeneralizer (
    main
) where

import Data.Aeson 
import Data.HashMap.Strict as HM
import Data.HashSet as HS
import Generalizer
import Schema

import ScalarGeneralize
import TupleGeneralize
import ConsolidateAnyOfGeneralize
import RemoveAnyOfGeneralize
import OracleWrapperGeneralize
import RemoveAnyOfGeneralize
import RefGeneralize
import SimplifyNumbers
import SameObject
import ForwardRefs
import Test.QuickCheck as Gen
import Control.Monad
import qualified Data.Vector as Vec
import Text.Printf

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LazyBS
import Data.Ratio

import Debug.Trace

import Network.Wreq
import Control.Lens

fullGeneralize :: Generalizer 
fullGeneralize = foldr1 (flip combine) $ Prelude.map generalizeBottomUp [
    scalarGeneralize,
    wrapper,
    refer
    -- unwrapper
    ]

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
    ourExamples <- Gen.generate $ Gen.vectorOf n $ arbitraryFromSchema $ schema
    acceptedExamples <- filterM oracle ourExamples
    let p = ((fromIntegral $ length acceptedExamples) / fromIntegral n)
    return p

recall :: Int -> Schema -> IO Double
recall n schema = do 
    theirExamples <- sequence $ take n $ repeat example
    let acceptedExamples = Prelude.filter (Schema.member schema) theirExamples
    let r = ((fromIntegral $ length acceptedExamples) / (fromIntegral $ length (theirExamples)))
    return r

main :: IO ()
main = do 
    startValue <- getStart
    let startSchema = schemaFromValue startValue 
    schema' <- fullGeneralize startSchema oracle
    let simplified = simplifyReferences $ forwardRefs $ simplifyNumbers schema'
    -- checkedSameObject <- sameObjectGeneralize simplified
    (AnyOf reduced) <- replaceWithBisimulations simplified
    let learnedSchema = AnyOf $ fmap (\defs@(DefinitionSchema top schemas) -> (getFromDefinitionMap defs top)) (traceShow reduced reduced)
    -- p <- precision 10 learnedSchema
    r <- recall    10 learnedSchema
    putStrLn $ printf "%f\t%f" (1.0 :: Double) r
