module Data.MRF.Base
( NodeIxs
, ValueIxs
, Weights
, Values
) where

-- | A list of node indexes.
type NodeIxs    = [Int]

-- | A list of value indexes (usually related to some fixed NodeIxs list).
type ValueIxs   = [Int]

-- | A list of values (usually related to some fixed NodeIxs list).
type Values x   = [x]

-- | Indexes of node values together with assigned weights.
type Weights	= [(Int, Double)]
