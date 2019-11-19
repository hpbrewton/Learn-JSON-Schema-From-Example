{-# LANGUAGE OverloadedStrings #-}
module PathTeacher (
    example,
    oracle,
    start,
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
import Teacher

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
