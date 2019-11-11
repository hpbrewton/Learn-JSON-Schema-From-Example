{-# LANGUAGE OverloadedStrings #-}
module Teacher (
    example,
    oracle,
    start,
    ServerTeacher (..),
    PathTeacher (..)
) where 

import Control.Lens
import Network.Wreq
import Text.Printf
import Schema
import Data.Aeson
import qualified Test.QuickCheck as Gen
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LazyBS

class Teacher teacher where 
    start   :: teacher -> IO Value
    oracle  :: teacher -> IO (Value -> IO Bool)
    example :: teacher -> IO Value 

data ServerTeacher = ServerTeacher {
    url :: String 
}

instance Teacher ServerTeacher where 
    start teacher = do 
        response <- get ((url teacher) <> "start")
        case decode $ (response ^. responseBody) of 
            Nothing -> error "could not parse start"
            Just v -> return v 
    oracle teacher = return $ \value -> do 
        response <- post ((url teacher) <> "member") value 
        case decode $ (response ^. responseBody) of 
            Nothing -> error "could not parse response of oracle"
            Just (Bool b) -> do 
                return b 
            _ -> error "was expecting a boolean form the oracle"
    example teacher = do 
        response <- get ((url teacher) <> "example")
        case decode $ (response ^. responseBody) of 
            Nothing -> error "could not parse example"
            Just v -> return v

data PathTeacher = PathTeacher String Int

instance Teacher PathTeacher where 
    start (PathTeacher p n) = do 
        text <- readFile $ printf "%s/%d/start.json" p n
        case decode $ LazyBS.fromStrict $ BS.pack text of 
            Just value -> return value 
            Nothing -> error "could not parse start file"
    oracle (PathTeacher p n) = do
        text <- readFile $ printf "%s/%d/schema.json" p n 
        case decode $ LazyBS.fromStrict $ BS.pack text of 
            Just value -> return $ (return . member (getFromDefinitionMap (rootLevel value) "."))
            Nothing -> error ""
    example (PathTeacher p n) = do 
        text <- readFile $ printf "%s/%d/schema.json" p n 
        case decode $ LazyBS.fromStrict $ BS.pack text of 
            Just value -> Gen.generate $ Gen.resize 10 $ arbitraryFromSchema $ getFromDefinitionMap (rootLevel value) "."
            Nothing -> error ""


