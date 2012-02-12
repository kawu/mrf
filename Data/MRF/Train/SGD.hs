module Data.MRF.Train.SGD
( trainParams
, TrainArgs (..)
) where

import Prelude hiding (length)
import Data.ListLike (ListLike, length, index, toList)
import System.Random (randomRIO, setStdGen, mkStdGen)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (ST, runST)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)

import qualified Data.MRF.MarkedArray as MA
import Data.MRF.Generic
import Data.MRF.Values
import Data.MRF.Features
import Data.MRF.Util (partition)
import Data.MRF.Train.Gradient
import Data.MRF.Inference (accuracy)

data TrainArgs = TrainArgs
    { batchSize :: Int
    , regVar :: Double
    , iterNum :: Double
    , scale0 :: Double
    , tau :: Double
    , workersNum :: Int }

trainParams :: (ListLike v g, ParamSet p f c x, WGV g c w x)
            => v -> v -> TrainArgs -> p -> IO p
trainParams trainData evalData args params = do
    let step :: Double
        step = fromIntegral (batchSize args)
             / fromIntegral (length trainData)
        scales = map (\done -> (scale0 args * tau args)
                             / (tau args + done)) [0, step ..]
        points = takeWhile (> 0.0) [iterNum args, iterNum args - step ..]

    hSetBuffering stdout NoBuffering
    putStr "Training data size = "
    putStrLn $ show $ length trainData
    when (length evalData > 0) $ do
        putStr "Evaluation data size = "
        putStrLn $ show $ length evalData

--     params <- return $ mkParams $ concat
--         $ map featuresIn' $ toList trainData  

    putStr "Model size = "
    ms <- return $ size params
    putStrLn $ show ms
    putStrLn "\n  -- TRAINING --"

    -- buffers for gradients
    gradBufs <- sequence $ replicate (workersNum args) (MA.new ms)
    setStdGen $ mkStdGen 0
    params' <- foldM
        (trainStep trainData evalData args step gradBufs)
        params
        (zip points scales)
    putStrLn "\n  -- FINISHED --"

    putStrLn $ ("\naccuracy train = " ++)
             $ show $ (accuracy $ workersNum args) params'
             -- $ show $ accuracy params'
             $ toList trainData
    return params'

putInfo :: (ListLike v g, ParamSet p f c x, WGV g c w x)
        => p -> v -> Double -> TrainArgs -> IO ()
putInfo params dataSet point args = do
    acc <- return $ case length dataSet of
        0 -> "#"
        _ -> show $ accuracy (workersNum args) params $ toList dataSet
--         _ -> show $ accuracy params $ toList dataSet
    putStrLn $ "\n" ++ "[" ++ (show $ floor $ point) ++ "] "
        ++ "accuracy eval = " ++ acc

trainStep :: (ListLike v g, ParamSet p f c x, WGV g c w x)
     => v -> v -> TrainArgs -> Double -> [MA.MarkedArray] -> p
     -> (Double, Double) -> IO p
trainStep trainData evalData args step gradBufs params (point, scale) = do
    if floor point /= floor (point - step)
        then putInfo params evalData point args
        else putStr "."
    batch <- getBatch trainData (batchSize args)
    updateParams params gradBufs batch args scale $ length trainData

updateParams :: (ListLike v g, ParamSet p f c x, WGV g c w x)
             => p -> [MA.MarkedArray] -> v -> TrainArgs
             -> Double -> Int -> IO p
updateParams params gradBufs batch args scale trainSize =
    let regularization v = v * regCoef
        regCoef = 1.0 - iVar2 * coef * scale
        coef = (fromIntegral $ batchSize args)
             / (fromIntegral trainSize)
        iVar2 = 1.0 / (regVar args ^ 2)

        parts = partition (workersNum args) $ toList batch
        cg (gradBuf, part) = computeGradient params scale gradBuf part
    in do
        -- explicit concurrent computation of gradients
        com <- newEmptyMVar
        forM_ (zip gradBufs parts) $ \(buffer, part) -> forkIO $ do
            gradient <- computeGradient params scale buffer part
            putMVar com gradient
        grads <- sequence [takeMVar com | _ <- [1..workersNum args]]

        -- | TODO: Use parallel version ?
        -- params' <- unsafeMapParallel (workersNum args) regularization params
        params' <- unsafeMap regularization params
        result <- foldM (\params grad ->
            applyGradient grad params) params' grads
        forM_ grads MA.clear
        return result

getBatch :: (ListLike v x) => v -> Int -> IO [x]
getBatch xs n = do
    return (replicate n $ randomRIO (0, length xs - 1))
    >>= sequence >>= return . map (index xs)
