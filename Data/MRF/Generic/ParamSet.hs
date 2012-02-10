module Data.MRF.Generic.ParamSet
( ParamCore (..)
, ParamSet (..)
, mkParamSet
) where

import qualified Data.Set as Set
import Control.Monad.Primitive (PrimMonad)

import Data.MRF.Generic.Feature

class ParamCore p where

    -- | Writes values from supplied (parameter index, value) list using
    -- the given function to compute new values.  NOTE: This is unsafe,
    -- internal operation which modify the parameter set *in place*.
    -- The input parameter set should not be used after the operation.
    unsafeConsume   :: PrimMonad m => (Double -> Double -> Double)
                                   -> [(Int, Double)] -> p -> m p
    -- | Map the entire parameter set using the supplied function.
    -- NOTE: This is unsafe, internal operation which modify
    -- the parameter set *in place*.  The input parameter set
    -- should not be used after the operation.
    unsafeMap       :: PrimMonad m => (Double -> Double) -> p -> m p

class (Feature f c x, ParamCore p) => ParamSet p f c x | p -> f where

    -- | Make parameter set from a list of (feature, value) pairs.
    -- There should be no repetitions in the input list.
    fromList        :: [(f, Double)] -> p
    size            :: p -> Int
    phi             :: p -> f -> Double

    -- | Index of the given feature.  It can be assumed, that
    -- feature f is present in the parameter set.
    featureToIx     :: f -> p -> Int

-- | Make params from a list of features with initial weights set to 0.0.
-- There may be repetitions in the input list.
mkParamSet :: (Ord f, ParamSet p f c x) => [f] -> p
mkParamSet fs =
    let fs' = Set.toList $ Set.fromList fs
    in  fromList [(ft, 0.0) | ft <- fs']
