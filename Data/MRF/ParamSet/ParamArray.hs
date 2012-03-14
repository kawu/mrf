module Data.MRF.ParamSet.ParamArray
( ParamArray
, mkParamArray
, MkBounds (..)
) where

import           Data.List (foldl')
import qualified Data.Array.Unboxed as A
import qualified Data.Vector.Unboxed as U
import           Data.Binary (Binary, put, get)
import           Control.Monad (forM_)
import qualified Data.Ix as Ix -- (Ix, range, inRange)

import           SGD

import           Data.MRF.Vector.Binary
import           Data.MRF.Generic.ParamSet
import           Data.MRF.Generic

data ParamArray f = ParamArray
    { ixs    :: A.Array f Int
    , values :: U.Vector Double }


instance (Ix.Ix f, Binary f) => Binary (ParamArray f) where
    put params = do
        put $ ixs params
        put $ values params
    get = do
        ixs <- get
        values <- get
        return $ ParamArray ixs values


instance ParamCore (ParamArray f) where

    unsafeConsume f xs params = do
        values' <- unsafeConsume f xs $ values params
        return $ ParamArray (ixs params) values' 

    unsafeMap f params = do
        values' <- unsafeMap f $ values params
        return $ ParamArray (ixs params) values' 

    size = size . values


class Ix.Ix f => MkBounds f where
    selBounds   :: (f, f) -> f -> (f, f)
    mkBounds    :: [f] -> (f, f)
    mkBounds xs =
        foldl' selBounds (head xs, head xs) (tail xs)


instance (MkBounds f, Feature f c x) => ParamSet (ParamArray f) f c x where

    fromList params =
        ixs `seq` ParamArray ixs (U.fromList values)
      where
        (feats, values) = unzip params
        ixs = A.array bounds [(key, -1) | key <- Ix.range bounds]
                        A.// zip feats [0..]
        bounds = mkBounds feats

    featureToIx feat ps =
        ixs ps A.! feat

    phi ps feat = doPhi
      where
        doPhi
            | not inRange   = 0.0
            | ix < 0        = 0.0
            | otherwise     = values ps U.! ix
        inRange = Ix.inRange (A.bounds $ ixs ps) feat
        ix      = ixs ps A.! feat

mkParamArray :: (MkBounds f, Feature f c x) => [f] -> ParamArray f
mkParamArray = mkParamSet
