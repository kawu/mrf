module Data.MRF.Generic.Feature
( Feature (..)
) where

import Data.MRF.Base
import Data.MRF.Generic.FactorGraph

-- class (Ord f, Factor c x) => Feature f c x | f -> c x where
class Factor c x => Feature f c x | f -> c x where
    -- | Generate list of features appropriate for the specific value list.
    genFeatures :: c -> Values x -> [f]
