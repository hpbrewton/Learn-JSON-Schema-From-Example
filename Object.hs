{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Object (
    ObjectLattice (..),
    grow
    )
    where

import Lattice 
import Float

import qualified Data.Map as M

data ObjectLattice l = ObjectLattice (M.Map String l)
    deriving (Show, Eq, Ord)

growObjectRange :: Lattice l v => (M.Map String v) -> ((M.Map String v) -> Bool) -> ObjectLattice l
growObjectRange m o = ObjectLattice $ M.mapWithKey (\k v -> grow v (o . (\v -> M.insert k v m))) m

instance (Lattice l v) => Lattice (ObjectLattice l) (M.Map String v) where 
    grow = growObjectRange

exampleOracle :: M.Map String Float -> Bool 
exampleOracle m = (m M.! "a" <= 30) && (m M.! "b" >= 40)

obj :: M.Map String Float
obj = M.fromList [("a", 25), ("b", 60)]