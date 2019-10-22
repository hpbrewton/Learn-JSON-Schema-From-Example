module ConsolidateAnyOfGeneralize (
    consolidateAnyOfGeneralize
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

combineNumberSchema :: Schema -> Schema -> Schema
combineNumberSchema (NumberSchema lba uba) (NumberSchema lbb ubb) = NumberSchema (maybeMin lba lbb) (maybeMax uba ubb)
    where 
        maybeMin (Just a) (Just b) = Just $ min a b  
        maybeMin Nothing a = a 
        maybeMin a b = maybeMin b a 

        maybeMax (Just a) (Just b) = Just $ max a b 
        maybeMax Nothing a = a 
        maybeMax a b = maybeMax b a

consolidateAnyOfGeneralize :: Generalizer
consolidateAnyOfGeneralize = generalizeBottomUp consolidateAnyOfGeneralizeNonRecursive
    where
        consolidateAnyOfGeneralizeNonRecursive :: Generalizer
        consolidateAnyOfGeneralizeNonRecursive (AnyOf schemae) oracle = do 
            let (numbers, nonNumbers) = L.partition (\s -> case s of (NumberSchema _ _) -> True; _ -> False) schemae
            let allNumberSchema = foldr1 combineNumberSchema (numbers :: [Schema])
            return $ AnyOf (allNumberSchema:nonNumbers)
        consolidateAnyOfGeneralizeNonRecursive sch oracle = return sch 
            