module Data.MRF.ParamSet.ParamMap
( ParamMap
, mkParamMap
) where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U
import           Data.Binary (Binary, put, get)
import           Control.Monad (forM_)

import           Data.MRF.Vector.Binary
import           Data.MRF.Generic.ParamSet
import           Data.MRF.Generic
import           Data.MRF.ParamSet.VectCore


data ParamMap f = ParamMap
    { ixs    :: Map.Map f Int
    , values :: U.Vector Double }


instance (Ord f, Binary f) => Binary (ParamMap f) where
    put params = do
        put $ ixs params
        put $ values params
    get = do
        ixs <- get
        values <- get
        return $ ParamMap ixs values


instance ParamCore (ParamMap f) where

    unsafeConsume f xs params = do
        values' <- unsafeConsume f xs $ values params
        return $ ParamMap (ixs params) values' 

    unsafeMap f params = do
        values' <- unsafeMap f $ values params
        return $ ParamMap (ixs params) values' 


instance (Ord f, Feature f c x) => ParamSet (ParamMap f) f c x where

    fromList paramList =
        ParamMap ixs values
      where
        feats = map fst paramList
        values = U.fromList $ map snd paramList
        ixs = Map.fromList $ zip feats [0..]

    size = U.length . values

    featureToIx feat ps =
        fromJust $ Map.lookup feat $ ixs ps
      where
        fromJust (Just x) = x
        fromJust Nothing  =
            error "ParamMap.featureToIx: feature not in the parameter set"

    phi ps feat =
        maybe 0.0 val ix
      where
        ix = {-# SCC "ParamMap.lookup" #-} Map.lookup feat $ ixs ps
        val = (values ps U.!)

mkParamMap :: (Ord f, Feature f c x) => [f] -> ParamMap f
mkParamMap = mkParamSet
