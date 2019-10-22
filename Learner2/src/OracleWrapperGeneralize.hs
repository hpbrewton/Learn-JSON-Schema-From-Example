{-# LANGUAGE OverloadedStrings #-}
module OracleWrapperGeneralize (
    wrapper,
    unwrapper
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

wrapper :: Generalizer
wrapper schema oracle = return (SchemaWithOracle schema oracle)

unwrapper :: Generalizer
unwrapper (SchemaWithOracle schema oracle) _ = return schema
unwrapper schema _ = return schema