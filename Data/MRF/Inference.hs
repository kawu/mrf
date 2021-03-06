module Data.MRF.Inference
( activeSets
, expectedFeaturesIn
, forward
, backward
, disamb
, disamb'
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
import           Control.Parallel (par, pseq)
import           GHC.Conc (numCapabilities)

import           Data.MRF.Base
import           Data.MRF.LogMath
import           Data.MRF.Generic hiding (fromList)
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
    range (v, w) = sequence $ map Ix.range $ zip v w

    inRange (v, w) u = all id
        [ Ix.inRange (x, y) z
        | (x, y, z) <- zip3 v w u ]

    index (v, w) u =
        foldl f 0 $ zip3 v w u
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
    betaMapOn k = {-# SCC "betaMapOn" #-}
        let bns = bounds $ as k
            arr = A.array bns $ betaOn k
        in  (A.!) arr 
    bounds vs =
        ( fromList [0 | _ <- toList vs]
        , fromList [length (node graph i) - 1 | i <- toList vs] )

    betaOn k =
        [(xs, compute xs) | xs <- vs $ as k]
      where
        compute xs
            | k < n = sSum
                [ (mulOn $ xs `append` ys)
                    (phiOn        $ restrictToNs $ xs `append` ys)
                    (beta (k + 1) $ restrictToAs $ xs `append` ys)
                | ys <- vs $ ds k ]
            | otherwise = sOne
        phiOn = {-# SCC "compute.phiOn" #-} sum . map (phi params)
              . genFeatures (fs k) . vsOn (ns k)

        restrictToNs    = restrictTo $ ns k
        restrictToAs    = restrictTo $ as $ k + 1

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
psi graph as beta vs =
    psi'
  where
    jointV = as `vIntersect` vs
    diffV = as `vDifference` jointV

    selectV = select jointV vs
    mergeV  = merge jointV diffV as

    psi' xs = unProbSum $ sSum
        [ beta $ betaX diffX 
        | diffX <- valueIxs' graph diffV ]
      where
        jointX = selectV xs
        betaX = mergeV jointX
    
-- | Probability, that variables adjacent to the given factor take on
-- given values.  NOTE: The result is only *proportional* to probability
-- and has to be normalized using the Z normalization factor.
prob :: (ParamSet p f c x, FGV g c v x) => p -> g -> ActiveSets
     -> AccTable ProbSum -> AccTable ProbSum -> Int -> ValueIxs -> Double
prob params graph actives alpha beta k =
    prob'
  where
    fs  = factor graph
    ns  = adjacent . fs
    vsOn = valuesOn' graph

    as  = actives k
    as' = actives $ k + 1
    vs  = ns k
    vs' = (as `vIntersect` as') `vDifference` vs

    prob' xs
        = sum [ phi params ft
              | ft <- genFeatures (fs k) (vsOn vs xs) ]
        + logSum
            [ psiAlpha (xs `append` xs')
            + psiBeta  (xs `append` xs')
            | xs' <- valueIxs' graph vs' ]
            
    psiAlpha    = psi graph as  (alpha k)      (vs `append` vs')
    psiBeta     = psi graph as' (beta $ k + 1) (vs `append` vs')


-- | Compute expected number of features in the given factor graph.
-- There may be (and probably will) duplicates in the output list.
-- TODO: Should return also Z normalization factor ?
expectedFeaturesIn :: (ParamSet p f c x, FGV g c v x) => p -> g -> [(f, Double)]
expectedFeaturesIn params graph =
    zx `par` zx' `pseq` zx `pseq`
      concat [expectedOn k | k <- [0 .. n - 1]]
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
    zx' = unProbSum $ alpha n empty

    expectedOn k =
        [ (ft, pr xs - zx) | xs <- vs $ ns k
        , ft <- genFeatures (fs k) (vsOn (ns k) xs) ]
      where
        pr = prob params graph ((V.!) $ as) alpha beta k

------------------------------------------------------------------------------

-- | Data structure for disambiguation computation.
data Disamb         = DS Double DisambPairs
type NodeIx         = Int
type ValueIx        = Int
type DisambPairs    = [(NodeIx, ValueIx)]

instance Semiring Disamb where
    sPlus (DS x ds) (DS x' ds')
        | x > x'        = DS x ds
        | otherwise     = DS x' ds'
    sZero               = DS mInf []
    sOne                = DS 0.0 []
    sMul x (DS y ds)    = DS (x + y) ds -- ^ or undefined ?

instance FGV g c v x => Graphing Disamb g c v x where
    gMul st x (DS y ds) =
        DS (x + y) (newDs ++ ds)
      where
        fs = factor graph
        ns = adjacent . fs

        BackwardSt graph as k vs xs = st
        vs' = filterVar graph (ns k) `vDifference` as k
        xs' = select vs' vs xs
        newDs = zip vs' xs'
            
disamb :: (ParamSet p f c x, FGV g c v x) => p -> g -> ValueIxs
disamb params graph =
    map snd $ sort $ ds ++ zip constIxs (repeat 0)
  where
    beta        = backward params graph $ activeSets graph
    DS _ ds     = beta 0 empty
    constIxs    = filterConst graph [0 .. nodesNum graph - 1]

disamb' :: (ParamSet p f c x, FGV g c v x) => p -> g -> [x]
disamb' params graph =
    [ index node k | (node, k) <- zip ns ys ]
  where
    ns = nodes graph
    ys = disamb params graph

goodAndBad :: (ParamSet p f c x, WGV g c v x) => p -> g -> (Int, Int)
goodAndBad params graph =
    foldl gather (0, 0) $ zip labels labels'
  where
    labels' = disamb params graph
    labels = [ fst $ maximumBy (compare `on` snd)
                   $ toList $ weights graph i
             | i <- [0 .. nodesNum graph - 1] ]
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

-- | Parallel accuracy computation.  TODO: use implicit parallelism ?
accuracy :: (ParamSet p f c x, WGV g c v x) => p -> [g] -> Double
accuracy params dataset =
    let k = numCapabilities
    	parts = partition k dataset
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
merge vs vs' vsTo
    | null vs'  = takeL $ select vsTo vs
    | null vs   = takeR $ select vsTo vs'
    | otherwise = takeB $ select vsTo (vs `append` vs')
  where
    takeL sel xs xs' = sel xs
    takeR sel xs xs' = sel xs'
    takeB sel xs xs' = sel $ xs `append` xs'

select :: (Ix.Ix i, ListLike w i, ListLike v e, Ord e) => w -> w -> v -> v
select which vs
    | which `eq` vs = id
    | otherwise     = select'
  where
    eq v w  = length v == length w && all (uncurry (==)) (LL.zip v w)
    is      = LL.map iFor which  :: [Int]
    iFor v  = fromJust
        $   elemIndex v vs
        <|> error "select: vertex not in vs"
    select' xs =  LL.map (index xs) is
