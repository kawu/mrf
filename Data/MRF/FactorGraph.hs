module Data.MRF.FactorGraph
( FactorGraph (..)
, mkFactorGraph
, Labeled (..)
, mkLabeled
, Dom
) where

-- TODO: In Data.MRF.FactorGraph.Unboxed implement unboxed version
-- (when values can be stored in unboxed vectors).

import           Prelude hiding (length, reverse, map)
import           Data.ListLike (length, reverse, map, index, singleton,
                                fromList, toList, elemIndex)
import           Data.ListLike.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import           Data.MRF.Generic hiding (FactorGraph, fromList)
import qualified Data.MRF.Generic as G

-- type Dom x = Values x
type Dom x  = V.Vector x

data FactorGraph c x = FactorGraph
    { graphNodes    :: V.Vector (Dom x)
    , graphFactors  :: V.Vector c }

instance Factor c x => G.FactorGraph (FactorGraph c x) c (Dom x) where
    factorsNum              = length . graphFactors
    factors                 = toList . graphFactors
    factor graph            = index $ graphFactors graph
    nodesNum                = length . graphNodes
    nodes                   = toList . graphNodes
    node graph              = index $ graphNodes graph
    reverseFactors graph    = FactorGraph ns $ reverse fs
        where ns = graphNodes graph
              fs = graphFactors graph

mkFactorGraph :: (Ord x, Factor c x) => [[x]] -> [c] -> FactorGraph c x
mkFactorGraph ns fs =
    FactorGraph nodes factors
  where
    nodes = fromList $ map fromList ns
    factors = fromList fs

------------------------------------------------------------------------------

type ChoiceIxs = U.Vector Int

-- | Labeled graph implementation with factors linked to their types --
-- a transformation of factor graph g.
data Labeled g = Labeled
    { labelGraph    :: g
    , labelIxs      :: ChoiceIxs }

instance G.FactorGraph g c v => G.FactorGraph (Labeled g) c v  where
    factorsNum              = factorsNum . labelGraph
    factors                 = factors . labelGraph
    factor                  = factor . labelGraph
    nodesNum                = nodesNum . labelGraph
    nodes                   = nodes . labelGraph
    node                    = node . labelGraph
    reverseFactors graph    =
        Labeled (reverseFactors $ labelGraph graph) (labelIxs graph)

instance G.FactorGraph g c v => WeightGraph (Labeled g) c v where
    weights graph k =
        let labelIx = index (labelIxs graph) k
        -- | TODO: 0.0 in log scale 
        in  singleton (labelIx, 1.0)

mkLabeled :: FGV g c v x => [x] -> g -> Labeled g
mkLabeled labels graph =
    Labeled graph labelIxs
  where
    ns = G.nodes graph
    labelIxs = fromList
        [fromJust $ elemIndex v vs | (vs, v) <- zip ns labels]
    fromJust (Just x) = x
    fromJust Nothing  = error "mkLabeled: index out of bounds"
