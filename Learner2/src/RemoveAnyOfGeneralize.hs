module RemoveAnyOfGeneralize (
    removeAnyOfGeneralize
) where

import Schema
import Util
import Data.Aeson
import Data.Scientific
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Test.QuickCheck as Gen
import qualified Data.Vector as Vec
import Generalizer

removeAnyOfGeneralize :: Generalizer 
removeAnyOfGeneralize = generalizeBottomUp nonRecursiveBoii
    where 
        nonRecursiveBoii (AnyOf _) oracle = return NullSchema 
        nonRecursiveBoii sch oracle = return sch