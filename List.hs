{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module List (
    ListLattice (..),
    grow,
    exampleOracle,
    accepted,
    latticeExample
    )
    where

import Lattice 
import Float

import qualified Data.Map as M
import qualified Test.QuickCheck.Gen as Gen

data ListLattice l = Empty 
    | NonEmpty l
    | Any l
    deriving (Eq, Ord, Show)

growListLattice :: Lattice l v => [v] -> ([v] -> IO Bool) -> IO (ListLattice l)
growListLattice [] _ = return $ Empty
growListLattice (x:xs) o = do 
    neAccepted <- (fmap not . o) [] 
    if neAccepted
        then fmap NonEmpty tau
        else fmap Any tau 
    where 
        tau = grow x (\e -> o (e:xs))

listAccepted :: (Lattice l v) => ListLattice l -> [v] -> Bool
listAccepted Empty [] = True 
listAccepted (NonEmpty t) [] = False
listAccepted (NonEmpty t) xs = all (accepted t) xs 
listAccepted (Any t) xs = all (accepted t) xs
listAccepted _ _ = False

listLatticeExample :: (Lattice l v) => ListLattice l -> Gen.Gen [v] 
listLatticeExample Empty = return $ [] 
listLatticeExample (NonEmpty t) = Gen.listOf1 (latticeExample t)
listLatticeExample (Any t) = do
    Gen.oneof [return [], Gen.listOf1 (latticeExample t) ]

instance (Lattice l v) => Lattice (ListLattice l) [v] where 
    grow = growListLattice
    accepted = listAccepted
    latticeExample = listLatticeExample

exampleOracle :: [Float] -> IO Bool 
exampleOracle [] = return False
exampleOracle xs = return $ all (\v -> v >= 25 && v <= 60) xs

l :: [Float]
l = [30, 35, 40]

learned :: IO (ListLattice Range)
learned = growListLattice l exampleOracle