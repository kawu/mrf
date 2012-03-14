module Data.MRF.Generic.ParamSet
( ParamCore (..)
, ParamSet (..)
, mkParamSet
) where

import qualified Data.Set as Set
import SGD

import Data.MRF.Generic.Feature

-- | TODO: c, x type variables superfluous.
class (Feature f c x, ParamCore p) => ParamSet p f c x | p -> f where

    -- | Make parameter set from a list of (feature, value) pairs.
    -- There should be no repetitions in the input list.
    fromList        :: [(f, Double)] -> p
    phi             :: p -> f -> Double

    -- | Index of the given feature.  It can be assumed, that
    -- feature f is present in the parameter set.
    featureToIx     :: f -> p -> Int

-- | Make params from a list of features with initial weights set to 0.0.
-- There may be repetitions in the input list.
mkParamSet :: (Ord f, ParamSet p f c x) => [f] -> p
mkParamSet fs =
    let fSet = Set.fromList fs
        fs'  = Set.toList fSet
        vs   = replicate (Set.size fSet) 0.0
    in  fromList (zip fs' vs)
