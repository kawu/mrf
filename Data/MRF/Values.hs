module Data.MRF.Values
( values
, values'
, valueIxs'
, valuesOn'
) where

import Prelude hiding (length)
import Data.ListLike (ListLike, fromList, toList, index, length)

import Data.MRF.Base
import Data.MRF.Generic hiding (fromList)

-- | Chosen values (cartesian product) of given nodes.
values :: WGV g f v x => g -> NodeIxs -> [(Values x, Double)]
values graph
    = map (proc . unzip) . sequence
    . map weightsFor . toList
  where
    node' = node graph
    weightsFor i =
        let n  = node graph i
            ws = weights graph i
        in  [(index n j, w) | (j, w) <- toList ws]
    -- | TODO: Weights should be also stored in log scale ?
    -- proc (xs, ps) = (fromList xs, sum ps)
    proc (xs, ps) = (fromList xs, product ps)

-- | All potential values (cartesian product) of given nodes.
values' :: FGV g f v x => g -> NodeIxs -> [Values x]
values' graph
    = map fromList . sequence
    . map (toList . node graph)
    . toList

-- | Potential value indexes (cartesian product) for given nodes.
valueIxs' :: FGV g f v x => g -> NodeIxs -> [ValueIxs]
valueIxs' graph
    = map fromList . sequence
    . map (ixs . node graph)
    . toList
  where
    ixs xs = [0 .. length xs - 1]

-- valuesOn' :: FGV g f v x => g -> NodeIxs -> ValueIxs -> Values x
-- valuesOn' graph nodeIxs valueIxs = fromList
--     [ node `index` i
--     | (node, i) <- zip nodes $ toList valueIxs ]
--   where
--     nodes = map (node graph) $ toList nodeIxs

valuesOn' :: FGV g f v x => g -> NodeIxs -> ValueIxs -> Values x
valuesOn' graph nodeIxs =
    doIt
  where
    nodes = map (node graph) nodeIxs
    doIt valueIxs =
        [ node `index` i
        | (node, i) <- zip nodes valueIxs ]
