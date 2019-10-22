{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Bool (
    BoolLattice (..),
    grow,
    accepted,
    latticeExample,
    )
    where 

import Lattice 
import qualified Test.QuickCheck.Gen as Gen

data BoolLattice = Singleton Bool 
    | Any
    deriving (Show, Ord, Eq)

growBoolLattice :: Bool -> (Bool -> IO Bool) -> IO BoolLattice
growBoolLattice e o = do 
    return (Singleton e)

acceptedBool :: BoolLattice -> Bool -> Bool 
acceptedBool (Singleton a) b = a == b 
acceptedBool Any _ = True 

latticeExampleBool :: BoolLattice -> Gen.Gen Bool 
latticeExampleBool (Singleton str) = return str 
latticeExampleBool Any = return "aa" -- TODO: uhyeah?

instance Lattice BoolLattice Bool where 
    grow = growBoolLattice
    accepted = acceptedBool
    latticeExample = latticeExampleBool