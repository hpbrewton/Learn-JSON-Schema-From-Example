{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Lattice (
    Lattice,
    grow,
    accepted,
    latticeExample
    )
    where

import qualified Test.QuickCheck.Gen as Gen

class Lattice l d where 
    grow :: d -> (d -> IO Bool) -> IO l
    accepted :: l -> d -> Bool
    latticeExample :: l -> Gen.Gen d