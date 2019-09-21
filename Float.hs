{-# LANGUAGE MultiParamTypeClasses #-}
module Float (
    Range (..),
    grow
    )
    where 

import Lattice

data Range = Interval Float Float
    | Top 
    | Bot 
    deriving (Eq, Ord, Show)

growFractionalRange :: Float -> (Float -> Bool) -> Range
growFractionalRange e o = Interval (findMax (-1000) e (not . o)) (findMax e 1000 o)
    where
        findMax l r o 
            | m == l || m == r = m 
            | o m = findMax m r o 
            | otherwise = findMax l m o 
            where 
                m = (l + r)/2

instance Lattice Range Float where
    grow = growFractionalRange