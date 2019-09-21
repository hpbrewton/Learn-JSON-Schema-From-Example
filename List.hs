{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module List (
    ListLattice (..),
    grow
    )
    where

import Lattice 
import Float

import qualified Data.Map as M

data ListLattice l = Empty 
    | NonEmpty l
    | Any l
    deriving (Eq, Ord, Show)

growListLattice :: Lattice l v => [v] -> ([v] -> Bool) -> ListLattice l
growListLattice [] _ = Empty
growListLattice (x:xs) o 
    | (not . o) [] = NonEmpty tau 
    | otherwise = Any tau 
    where 
        tau = grow x (\e -> o (e:xs))

instance (Lattice l v) => Lattice (ListLattice l) [v] where 
    grow = growListLattice

exampleOracle :: [Float] -> Bool 
exampleOracle [] = False
exampleOracle xs = all (\v -> v >= 25 && v <= 60) xs

l :: [Float]
l = [30, 35, 40]