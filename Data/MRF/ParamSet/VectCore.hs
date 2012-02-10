module Data.MRF.ParamSet.VectCore where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad (forM_)

import Data.MRF.Generic.ParamSet
import Data.MRF.Generic

instance ParamCore (U.Vector Double) where

    unsafeConsume f xs values = do
        vect <- U.unsafeThaw values
        forM_ xs $ \(i, v) -> do
            w <- UM.read vect i
            UM.write vect i $! f w v
        values' <- U.unsafeFreeze vect
        return values'
    
    unsafeMap f values = do
        vect <- U.unsafeThaw values
        forM_ [0 .. UM.length vect - 1] $
            \i -> UM.write vect i . f =<< UM.read vect i
        values' <- U.unsafeFreeze vect
        return values'
