{-# LANGUAGE MultiParamTypeClasses #-}
module Float (
    Range (..),
    grow,
    accepted,
    latticeExample,
    )
    where 

import Lattice
import Data.Fixed
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.Arbitrary as Arb

data Range = Interval Float Float
    | Top 
    | Bot 
    deriving (Eq, Ord, Show)

growFractionalRange :: Float -> (Float ->  IO Bool) -> IO Range
growFractionalRange e o = do 
    l <- (findMax (-1000) e (fmap not . o)) 
    r <- (findMax e 1000 o)
    return $ Interval l r
    where
        findMax :: Float -> Float -> (Float -> IO Bool) -> IO Float
        findMax l r o = do 
            if (m == l || m == r)
                then return m 
                else do 
                    b <- o m
                    if b 
                        then findMax m r o 
                        else findMax l m o 
            where 
                m = (l + r)/2

acceptedFloat :: Range -> Float -> Bool 
acceptedFloat (Interval a b) n = (a <= n) && (n <= b)
acceptedFloat Top _ = True 
acceptedFloat Bot _ = False 

latticeExampleRange :: Range -> Gen.Gen Float 
latticeExampleRange (Interval a b) = do 
    f <- Arb.arbitrarySizedFractional 
    return $ a + (f `mod'` (b-a))

instance Lattice Range Float where
    grow = growFractionalRange
    accepted = acceptedFloat
    latticeExample = latticeExampleRange