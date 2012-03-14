module Data.MRF.Gradient where

import           SGD
import qualified Data.MarkedArray as MA

import           Data.MRF.Generic
import           Data.MRF.LogMath (logAdd)
import           Data.MRF.Features (featuresIn)
import           Data.MRF.Inference (expectedFeaturesIn, accuracy)
import qualified Data.MRF.Inference as I

instance (ParamSet p f c x, WGV g c v x) => DataElem p g where  

    computeGrad params part buffer =
        let ns = concat $ map featuresIn part
            ens = concat $ map (expectedFeaturesIn params) part
            followPtrs = map $ \(feat, val) -> (featureToIx feat params, val)
        in do
            gradient <- MA.consumeWith logAdd (followPtrs ens) buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) (followPtrs ns)
            return gradient

    accuracy = I.accuracy 
