module Data.MRF.ParamSet.ParamArray
( ParamArray
, mkParamArray
) where

import qualified Data.Array.Unboxed as A
import qualified Data.Vector.Unboxed as U
import           Data.Binary (Binary, put, get)
import           Control.Monad (forM_)
import qualified Data.Ix as Ix -- (Ix, range, inRange)

import           Data.MRF.Vector.Binary
import           Data.MRF.Generic.ParamSet
import           Data.MRF.Generic
import           Data.MRF.ParamSet.VectCore


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


instance (Ix.Ix f, Feature f c x) => ParamSet (ParamArray f) f c x where

    fromList paramList =
        ParamArray ixs values
      where
        feats = map fst paramList
        values = U.fromList $ map snd paramList
        ixs = A.array bounds [(key, -1) | key <- Ix.range bounds]
         A.// zip feats [0..]
        bounds = (minimum feats, maximum feats)

    size = U.length . values

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

mkParamArray :: (Ix.Ix f, Feature f c x) => [f] -> ParamArray f
mkParamArray = mkParamSet
