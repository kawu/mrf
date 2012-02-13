module Data.MRF.ParamSet.VectCore where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Control.Monad (forM_)
import           GHC.Conc (numCapabilities)
import           Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)

import           Data.MRF.Generic.ParamSet
import           Data.MRF.Generic

instance ParamCore (U.Vector Double) where

    unsafeConsume f xs values = do
        vect <- U.unsafeThaw values
        forM_ xs $ \(i, v) -> do
            w <- UM.read vect i
            UM.write vect i $! f w v
        values' <- U.unsafeFreeze vect
        return values'
    
--     unsafeMap f values = do
--         vect <- U.unsafeThaw values
--         forM_ [0 .. UM.length vect - 1] $
--             \i -> UM.write vect i . f =<< UM.read vect i
--         values' <- U.unsafeFreeze vect
--         return values'

    -- | Parallel map.  TODO: use implicit parallelism ?
    unsafeMap f values = do
        vect <- U.unsafeThaw values

        -- | Explicit concurrent computation.
        com <- newEmptyMVar
        forM_ bounds $ \(p, q) -> forkIO $ do  
            forM_ [p .. q - 1] $ \i ->
                UM.write vect i . f =<< UM.read vect i
            putMVar com ()
        sequence [takeMVar com | _ <- bounds]

        values' <- U.unsafeFreeze vect
        return values'
      where
        k       = numCapabilities
        n       = U.length values
        ps      = ((n - 1) `div` k) + 1
        uppers  = [i * ps | i <- [1 .. k - 1]] ++ [n]
        bounds  = zip (0 : uppers) uppers
