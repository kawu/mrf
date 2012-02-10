module Data.MRF.Features
( features
, featuresIn
, features'
, featuresIn'
) where

import Data.MRF.Base
import Data.MRF.Generic
import Data.MRF.Values

-- | Chosen features with assigned probabilities (in log scale).
features :: (WGV g c v x, Feature f c x) => g -> c -> [(f, Double)]
features graph fc =
    concatMap mkfs $ values graph $ adjacent fc
  where
    mkfs (xs, pr) = [(ft, pr) | ft <- genFeatures fc xs]

-- | Chosen features with assigned probabilities (in log scale).
featuresIn :: (WGV g c v x, Feature f c x) => g -> [(f, Double)]
featuresIn graph = concatMap (features graph) $ factors graph

-- | All features for given factor.
features' :: (FGV g c v x, Feature f c x) => g -> c -> [f]
features' graph fc = concatMap (genFeatures fc) $ values' graph $ adjacent fc

-- | All features in given factor graph.
featuresIn' :: (FGV g c v x, Feature f c x) => g -> [f]
featuresIn' graph = concatMap (features' graph) $ factors graph
