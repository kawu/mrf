module Data.MRF.Train.Gradient
( computeGradient
, applyGradient
, Gradient
) where

import Data.MRF.Generic
import Data.MRF.LogMath (logAdd)
import Data.MRF.Features (featuresIn)
import Data.MRF.Inference (expectedFeaturesIn)
import qualified Data.MRF.MarkedArray as MA

type Gradient = MA.MarkedArray

computeGradient :: (ParamSet p f c x, WGV g c v x)
                => p -> Double -> Gradient -> [g] -> IO Gradient
computeGradient params scale buffer part =
    let ns = concat $ map featuresIn part
        ens = concat $ map (expectedFeaturesIn params) part
    	followPtrs = map $ \(feat, val) -> (featureToIx feat params, val)
    in do
        gradient <- MA.consumeWith logAdd (followPtrs ens) buffer
                >>= MA.mapArray (\v -> - exp v) 
                >>= MA.consumeWith (+) (followPtrs ns)
                >>= MA.mapArray (* scale)
        return gradient

applyGradient :: ParamSet p f c x => Gradient -> p -> IO p
applyGradient grad params =
    MA.elems grad >>= \xs -> unsafeConsume (+) xs params
