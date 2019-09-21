{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Lattice (
    Lattice,
    grow
    )
    where

class Lattice l d where 
    grow :: d -> (d -> Bool) -> l