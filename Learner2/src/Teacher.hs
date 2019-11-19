{-# LANGUAGE OverloadedStrings #-}
module Teacher (
    example,
    oracle,
    start,
    Teacher,
    learn,
) where 

import Util
import Flatten
import IndependentLearner
import Recursive
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

learn :: (Teacher teacher) => teacher -> IO Schema
learn teacher = do
    tStart <- start teacher
    let schema = schemaFromValue tStart 
    tOracle <- oracle teacher
    aschema <- independentLearner schema tOracle
    adjacency $ flatten "" aschema
    -- finalStrict <- adjacency bschema 


