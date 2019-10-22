{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module String (
    StringLattice (..),
    grow,
    accepted,
    latticeExample,
    )
    where 

import Lattice 
import qualified Test.QuickCheck.Gen as Gen

data StringLattice = Singleton String 
    | Any
    deriving (Show, Ord, Eq)

growStringLattice :: String -> (String -> IO Bool) -> IO StringLattice
growStringLattice e o = do 
    return (Singleton e)

acceptedString :: StringLattice -> String -> Bool 
acceptedString (Singleton a) b = a == b 
acceptedString Any _ = True 

latticeExampleString :: StringLattice -> Gen.Gen String 
latticeExampleString (Singleton str) = return str 
latticeExampleString Any = return "aa" -- TODO: uhyeah?

instance Lattice StringLattice String where 
    grow = growStringLattice
    accepted = acceptedString
    latticeExample = latticeExampleString