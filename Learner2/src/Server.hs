{-# LANGUAGE OverloadedStrings #-}
module Server (
    main
) where

import Debug.Trace
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.Map as M
import Data.Aeson
import qualified Test.QuickCheck.Gen as Gen

import Schema
import Text.Printf
import qualified Data.Vector as Vec

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LazyBS

import System.Environment
import Control.Lens

-- | "{\"anyOf\":[{\"type\":\"number\"}, null]}"
start :: Int -> IO Value 
start n = do 
    text <- readFile $ printf "data/%d/start.json" n
    case decode $ LazyBS.fromStrict $ BS.pack text of 
        Just value -> return value 
        Nothing -> error "could not parse start file"

schema :: Int -> IO Schema 
schema n = do 
    text <- readFile $ printf "data/%d/schema.json" n
    case decode $ LazyBS.fromStrict $ BS.pack text of 
        Just value -> return $ getFromDefinitionMap (traceShow (rootLevel value) (rootLevel value)) "."
        Nothing -> error ""

example :: Schema -> IO Value 
example schema = Gen.generate $ Gen.resize 10 $ arbitraryFromSchema schema

main :: IO ()
main = do
    (n:_) <- getArgs
    let exampleNumber = read n 
    schemaV <- schema exampleNumber
    startV <- start exampleNumber
    putStrLn $ show $ member schemaV startV
    run 3000 $ \request f -> do 
            let path = pathInfo request
            json <- case path of 
                ["start"] -> return startV
                ["example"] -> example schemaV
                ["member"] -> do 
                    body <- lazyRequestBody request
                    case decode body of 
                        Just json -> do 
                            return $ Bool $ member schemaV json 
                        Nothing -> return $ Bool $ False
                path -> error $ show path
            f $ responseLBS status200 [(hContentType, "application/json")] $ encode json
