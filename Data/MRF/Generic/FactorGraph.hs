module Data.MRF.Generic.FactorGraph
( PlainFactor (..)
, Factor (..)
, FactorGraph (..)
, WeightGraph (..)
, FGV (..)
, WGV (..)
) where

import Data.ListLike
import Data.MRF.Base

-- | Factor c connected to nodes represented by a list of node indexes.
class PlainFactor c where
    adjacent    :: c -> NodeIxs         -- ^ Indexes of adjacent nodes

class (Ord x, PlainFactor c) => Factor c x | c -> x where
    prior       :: c -> Values x -> Double
    -- | Default prior is equall to 1 (0 in log scale).
    prior _ = const 0.0

-- | Factor graph g with factor type c and value type x.
class FactorGraph g c v | g -> c v where
    factorsNum      :: g -> Int
    factors         :: g -> [c]
    factor          :: g -> Int -> c
    reverseFactors  :: g -> g
    nodesNum        :: g -> Int
    nodes           :: g -> [v]
    node            :: g -> Int -> v

    factors g = [factor g i | i <- [0 .. factorsNum g - 1]]
    nodes g = [node g i | i <- [0 .. nodesNum g - 1]]

class FactorGraph g c v => WeightGraph g c v where
    weights     :: g -> Int -> Weights

-- | Factor graph with value type specified.
class (ListLike v x, Factor c x, FactorGraph g c v)
    => FGV g c v x | g -> c v x where

instance (ListLike v x, Factor c x, FactorGraph g c v) => FGV g c v x where

-- | Weight graph with value type specified.
class (ListLike v x, Factor c x, WeightGraph g c v)
    => WGV g c v x | g -> c v x where

instance (ListLike v x, Factor c x, WeightGraph g c v) => WGV g c v x where
