{-# LANGUAGE ForeignFunctionInterface #-}

module Data.MRF.LogMath
( logIsZero
, logAdd
, logSum
, inf
, mInf
) where

-- import Data.List (sort)

foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double

inf :: RealFloat a => a
inf = (1/0)

mInf :: RealFloat a => a
mInf = -(1/0)

logIsZero :: Double -> Bool
logIsZero x = x == mInf

logAdd :: Double -> Double -> Double
logAdd x y
    | logIsZero x   = y
    | x > y         = x + log1p(exp(y - x))
    | otherwise     = y + log1p(exp(x - y))

logSub :: Double -> Double -> Double
logSub x y
    | logIsZero x   = y
    | x > y         = x + log1p(exp(y - x))
    | otherwise     = y + log1p(exp(x - y))

logSum :: [Double] -> Double
--logSum l = foldl logAdd mInf $ sort l
logSum l = foldl logAdd mInf l
