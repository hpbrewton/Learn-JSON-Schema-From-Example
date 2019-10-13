{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Object (
    ObjectLattice (..),
    grow,
    exampleOracle,
    latticeExample,
    obj
    )
    where

import Lattice 
import Float
import qualified Test.QuickCheck.Gen as Gen

import qualified Data.Map as M

data ObjectLattice l = ObjectLattice (M.Map String l)
    deriving (Show, Eq, Ord)

growObjectRange :: Lattice l v => (M.Map String v) -> ((M.Map String v) -> IO Bool) -> IO (ObjectLattice l)
growObjectRange m o = fmap ObjectLattice $ sequence $ M.mapWithKey (\k v -> grow v (o . (\v -> M.insert k v m))) m

objectAccepted :: Lattice l v => ObjectLattice l -> M.Map String v -> Bool
objectAccepted (ObjectLattice l) obj = if M.keys l == M.keys obj 
    then all (\(k, v) -> accepted (l M.! k) v) $ M.toList obj 
    else False

objectLatticeExample :: Lattice l v => ObjectLattice l -> Gen.Gen (M.Map String v)
objectLatticeExample (ObjectLattice m) = sequence $ M.map latticeExample m  

instance (Lattice l v) => Lattice (ObjectLattice l) (M.Map String v) where 
    grow = growObjectRange
    accepted = objectAccepted
    latticeExample = objectLatticeExample

exampleOracle :: M.Map String Float -> IO Bool 
exampleOracle m = return ((m M.! "a" <= 30) && (m M.! "b" >= 40))

obj :: M.Map String Float
obj = M.fromList [("a", 25), ("b", 60)]

learned :: IO (ObjectLattice Range)
learned = grow obj exampleOracle