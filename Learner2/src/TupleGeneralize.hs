module TupleGeneralize (
    tupleGeneralize 
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

tupleGeneralize :: Generalizer
tupleGeneralize = generalizeBottomUp $ nonRecursiveTupleGeneralize
    where
        nonRecursiveTupleGeneralize :: Generalizer
        nonRecursiveTupleGeneralize sch@(TupleSchema schemas) oracle = do 
            (Array example) <- Gen.generate $ arbitraryFromSchema sch
            shuffle <- Gen.generate $ Gen.shuffle $ Vec.toList example
            b <- oracle $ Array $ Vec.fromList shuffle
            if b
                then return $ ArraySchema (AnyOf (Vec.toList schemas)) (Vec.length schemas) (Just $ Vec.length schemas)
                else return $ (TupleSchema schemas)
        nonRecursiveTupleGeneralize sch oracle = return sch
