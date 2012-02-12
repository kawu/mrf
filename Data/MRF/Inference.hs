module Data.MRF.Inference
( activeSets
, expectedFeaturesIn
, forward
, backward
, disamb
, accuracy
, ProbSum (..)
, AccTable
) where

import           Prelude hiding (length, filter, elem, null)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Control.Applicative ((<$>), (<|>))
import           Data.Maybe (fromJust)
import qualified Data.ListLike as LL
import           Data.ListLike (ListLike, fromList, toList, length, filter,
                                elem, elemIndex, index, empty, append, sort,
                                null)
import           Data.ListLike.Vector
import qualified Data.Ix as Ix
import           Data.List (maximumBy)
import           Data.Function (on)
import qualified Data.Array as A
import           Control.Parallel.Strategies (rseq, parMap)

import           Data.MRF.Base
import           Data.MRF.LogMath
import           Data.MRF.Generic
import           Data.MRF.Values
import		 Data.MRF.Util

-- | Type synonim for a set of *active* nodes.
type ActiveSet  = NodeIxs
type ActiveSets = Int -> ActiveSet

-- | TODO: Change ValueIxs to (U.Vector Int) ?
type AccColumn t = ValueIxs -> t 
type AccTable  t = Int -> AccColumn t

-- | Remove constant node indexes.
filterVar :: (ListLike v Int, FGV g c w x) => g -> v -> v
filterVar = filterNodeIxs $ (>1) . length
-- | Remove variable node indexes.
filterConst :: (ListLike v Int, FGV g c w x) => g -> v -> v
filterConst = filterNodeIxs $ (==1) . length

-- | Filter node indexes with respect to given predicate.
filterNodeIxs :: (ListLike v Int, FGV g c w x) => (w -> Bool) -> g -> v -> v
filterNodeIxs p graph = filter $ p . node graph

-- | Compute vector of active (mutual) sets.
activeSets :: FGV g c v x => g -> V.Vector ActiveSet
activeSets graph
    = fromList $ map fromList $ doActives (factors graph) []
  where
    doActives [] active = [active]
    doActives graph@(fc:fcs) active =
        active : doActives fcs (nextActive graph active)

    nextActive :: PlainFactor c => [c] -> [Int] -> [Int]
    nextActive (fc:fcs) active =
        filter engaged $ nub $ active ++ newActive
      where
        -- | New candidates to active set -- all adjacent nodes
        -- with domain of cardinality > 1.
        newActive = toList $ filterVar graph $ adjacent fc
        -- | Vertex with adjacent factor among the facs factors.
        engaged v = any (elem v . adjacent) fcs
        -- | Enforce proper ordering of vertices in active set.
        nub = Set.toList . Set.fromList

class Semiring t where
    sPlus :: t -> t -> t
    sMul  :: Double -> t -> t
    sZero :: t
    sOne  :: t
    sSum  :: [t] -> t
    sSum = foldl sPlus sZero

-- | State during the backward computations: factor graph, active sets,
-- step and known values in the current step.
data BackwardSt g x = BackwardSt
    { stGraph   :: g
    , stActives :: ActiveSets
    , stStep    :: Int
    , stNodes   :: NodeIxs
    , stValues  :: ValueIxs }

-- | We need to have different (*) operation in a contenxt of factor graph.
class (FGV g c v x, Semiring t) => Graphing t g c v x where
    gMul :: BackwardSt g x -> Double -> t -> t
    gMul _ = sMul

-- | We need this instance to store doBackward partial results efficiently.
-- Assumption: lists have exactly *the same* length. Otherwise, the code
-- will not work correctly !
instance Ix.Ix [Int] where
-- instance Ix.Ix x => Ix.Ix [x] where
    range (v, w)
        = map fromList $ sequence $ map Ix.range
        $ zip (toList v) (toList w)

    inRange (v, w) u = all id
        [ Ix.inRange (x, y) z
        | (x, y, z) <- zip3 (toList v) (toList w) (toList u) ]

    index (v, w) u =
        foldl f 0 $ zip3 (toList v) (toList w) (toList u)
      where
        f acc (p, q, i) = acc * (q - p + 1) + i - p
        -- f acc (p, q, i) = acc * Ix.rangeSize (p, q) + Ix.index (p, q) i

doBackward :: (ParamSet p f c x, Graphing t g c v x)
           => p -> g -> ActiveSets -> V.Vector (AccColumn t)
doBackward params graph as =
    betaT
  where
    n  = factorsNum graph
    vs = valueIxs' graph
    vsOn = valuesOn' graph
    fs = factor graph
    ns = adjacent . fs
    ds k = (as (k + 1) `vUnion` ns k) `vDifference` as k

    beta  = (betaT V.!)
    betaT = fromList $ map betaMapOn [0..n]
--     betaMapOn = (Map.!) . Map.fromList . betaOn
    betaMapOn k =
        let bns = bounds $ as k
            arr = A.array bns $ betaOn k
        in  (A.!) arr 
    bounds vs =
        ( fromList [0 | _ <- toList vs]
        , fromList [length (node graph i) - 1 | i <- toList vs] )

    betaOn k = [(xs, compute k xs) | xs <- vs $ as k]

    compute k xs
        | k < n = sSum
            [ (mulOn $ xs `append` ys)
                (phiOn        $ restrictTo (ns k)       $ xs `append` ys)
                (beta (k + 1) $ restrictTo (as $ k + 1) $ xs `append` ys)
            | ys <- vs $ ds k ]
        | otherwise = sOne
      where
        phiOn = sum . map (phi params)
              . genFeatures (fs k) . vsOn (ns k)
        both = as k `append` ds k
        restrictTo vs = select vs both 
        mulOn = gMul . BackwardSt graph as k both

forward :: (ParamSet p f c x, Graphing t g c v x)
        => p -> g -> V.Vector ActiveSet -> AccTable t
forward params graph actives = (V.!) $ V.reverse $
    doBackward params (reverseFactors graph) as
  where
    as = (V.!) $ V.reverse actives

backward :: (ParamSet p f c x, Graphing t g c v x)
         => p -> g -> V.Vector ActiveSet -> AccTable t
backward params graph actives = (V.!) $
    doBackward params graph as
  where
    as = (V.!) $ actives

-- | Instance for sum (marginals) computations.  All operations
-- are performed in logarithmic scale.
newtype ProbSum = ProbSum { unProbSum :: Double } deriving Show

instance Semiring ProbSum where
    sPlus (ProbSum x) (ProbSum y) = ProbSum (x `logAdd` y)
    sMul x (ProbSum y)            = ProbSum (x + y)
    sZero                         = ProbSum mInf
    sOne                          = ProbSum 0.0

instance FGV g c v x => Graphing ProbSum g c v x where

psi :: FGV g c v x => g -> NodeIxs -> AccColumn ProbSum
    -> NodeIxs -> ValueIxs -> Double
psi graph as beta vs xs = unProbSum $ sSum
    [ beta $ betaX diffX 
    | diffX <- valueIxs' graph diffV ]
  where
    jointV = as `vIntersect` vs
    diffV = as `vDifference` jointV
    -- ?!? jointV posortowane, można zagwarantować posortowanie vs i
    -- zaimplementować wersję select[From]Sorted.  Przydałby się
    -- osobny typ reprezentujący posortowany typ ListLike. 
    jointX = select jointV vs xs
    -- ?!? jointV, diffV, as posortowane
    betaX = merge jointV diffV as jointX

-- | Probability, that variables adjacent to the given factor take on
-- given values.  NOTE: The result is only *proportional* to probability
-- and has to be normalized using the Z normalization factor.
prob :: (ParamSet p f c x, FGV g c v x) => p -> g -> ActiveSets
     -> AccTable ProbSum -> AccTable ProbSum -> Int -> ValueIxs -> Double
prob params graph actives alpha beta k xs
    = sum
        [ phi params ft
        | ft <- genFeatures (fs k) (vsOn vs xs) ]
    + logSum
        [ psi graph as  (alpha k)      (vs `append` vs') (xs `append` xs')
        + psi graph as' (beta $ k + 1) (vs `append` vs') (xs `append` xs')
        | xs' <- valueIxs' graph vs' ]
  where
    fs  = factor graph
    ns  = adjacent . fs
    vsOn = valuesOn' graph

    as  = actives k
    as' = actives $ k + 1
    vs  = ns k
    vs' = (as `vIntersect` as') `vDifference` vs

-- | Compute expected number of features in the given factor graph.
-- There may be (and probably will) duplicates in the output list.
-- TODO: Should return also Z normalization factor ?
expectedFeaturesIn :: (ParamSet p f c x, FGV g c v x) => p -> g -> [(f, Double)]
expectedFeaturesIn params graph = concat
    [expectedOn k | k <- [0 .. n - 1]]
  where
    n = factorsNum graph
    as = activeSets graph
    fs = factor graph
    ns = adjacent . fs

    vs = valueIxs' graph
    vsOn = valuesOn' graph

    alpha = forward params graph as
    beta = backward params graph as
    zx = unProbSum $ beta 0 empty
    -- zx = alpha n empty

    expectedOn k =
        [ (ft, pr xs - zx) | xs <- vs $ ns k
        , ft <- genFeatures (fs k) (vsOn (ns k) xs) ]
      where
        pr = prob params graph ((V.!) $ as) alpha beta k

------------------------------------------------------------------------------

-- | Data structure for disambiguation computation.
data Disamb x = DS Double (DisambPairs x)
type DisambPairs x = [(Int, x)]

instance Semiring (Disamb x) where
    sPlus (DS x ds) (DS x' ds')
        | x > x'        = DS x ds
        | otherwise     = DS x' ds'
    sZero               = DS mInf []
    sOne                = DS 0.0 []
    sMul x (DS y ds)   = DS (x + y) ds -- ^ or undefined ?

instance FGV g c v x => Graphing (Disamb x) g c v x where
    gMul st x (DS y ds) =
        DS (x + y) (newDs ++ ds)
      where
        fs = factor graph
        ns = adjacent . fs

        BackwardSt graph as k vs xs = st
        vs' = filterVar graph (ns k) `vDifference` as k
        xs' = valuesOn' graph vs' $ select vs' vs xs
        newDs = zip (toList vs') (toList xs')
            
disamb :: (ParamSet p f c x, FGV g c v x) => p -> g -> [x]
disamb params graph =
    map snd $ sort $ ds ++ zip constIxs const
  where
    beta        = backward params graph $ activeSets graph
    DS _ ds     = beta 0 empty
    constIxs    = filterConst graph [0 .. nodesNum graph - 1]
    const       = [index (node graph k) 0 | k <- constIxs]

goodAndBad :: (ParamSet p f c x, WGV g c v x) => p -> g -> (Int, Int)
goodAndBad params graph =
    foldl gather (0, 0) $ zip labels labels'
  where
    labels' = disamb params graph
    labels = [ valueOn i $ fst $ maximumBy (compare `on` snd)
                         $ toList $ weights graph i
             | i <- [0 .. nodesNum graph - 1] ]
    valueOn i = index $ node graph i
    gather (good, bad) (x, y)
        | x == y = (good + 1, bad)
        | otherwise = (good, bad + 1)

goodAndBad' :: (ParamSet p f c x, WGV g c v x)
            => p -> [g] -> (Int, Int)
goodAndBad' params dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) $ map (goodAndBad params) dataset

-- accuracy :: (ParamSet p f c x, WGV g c v x) => p -> [g] -> Double
-- accuracy params dataset = fromIntegral good / fromIntegral (good + bad)
--     where (good, bad) = goodAndBad' params dataset

-- | Parallel accuracy computation.
accuracy :: (ParamSet p f c x, WGV g c v x) => Int -> p -> [g] -> Double
accuracy k params dataset =
    let parts = partition k dataset
        xs = parMap rseq (goodAndBad' params) parts
        (good, bad) = foldl add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)

------------------------------------------------------------------------------

-- | Set-like operation on list-like structure.  We use those special
-- operations, because we want to preserve the invariant that lists
-- are sorted.
vSetOp :: (ListLike v e, Ord e)
       => (Set.Set e -> Set.Set e -> Set.Set e) -> v -> v -> v
vSetOp op v w =
    fromList $ Set.toList $ vSet `op` wSet
  where
    vSet = mkSet v
    wSet = mkSet w
    mkSet  = Set.fromList . toList

vUnion :: (ListLike v e, Ord e) => v -> v -> v
vUnion = vSetOp Set.union

vIntersect :: (ListLike v e, Ord e) => v -> v -> v
vIntersect  = vSetOp Set.intersection

vDifference :: (ListLike v e, Ord e) => v -> v -> v
vDifference = vSetOp Set.difference

-------------------------------------------------------------------------------

merge :: (Ix.Ix i, ListLike w i, ListLike v e, Ord e)
      => w -> w -> w -> v -> v -> v
merge vs vs' vsTo xs xs'
    | null vs'  = select vsTo vs xs
    | null vs   = select vsTo vs' xs'
    | otherwise = select vsTo (vs `append` vs') (xs `append` xs')

select :: (Ix.Ix i, ListLike w i, ListLike v e, Ord e) => w -> w -> v -> v
select which vs xs
    | which `eq` vs = xs
    | otherwise     = LL.map (index xs) is
  where
    eq v w  = length v == length w && all (uncurry (==)) (LL.zip v w)
    is      = LL.map iFor which  :: [Int]
    -- vsMap   = Map.fromList $ LL.zip vs [0..]
    iFor v  = fromJust
        -- $   Map.lookup v vsMap
        $   elemIndex v vs
        <|> error "select: vertex not in vs"
