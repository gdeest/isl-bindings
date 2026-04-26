{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Validate parallel and vectorize schedule annotations.
--
-- A dimension marked 'Parallel' or 'Vectorize' must carry no
-- dependences — i.e., after applying the schedule to the dependence
-- relation, the annotated dimension must show no backward deps.
module Alpha.Codegen.Parallel
  ( validateAnnotations
  , AnnotationError(..)
  ) where

import Control.DeepSeq (NFData(..))
import qualified Data.Map.Strict as Map

import Isl.Typed.Constraints
  ( NamedMap(..), Constraint(..), Conjunction(..)
  , MapIx(..), Expr(..), buildBasicMap )
import Isl.Typed.Params (KnownSymbols)
import qualified Isl.Types as Isl
import qualified Isl.Map as RawM
import qualified Isl.UnionMap as UM
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_, freeM, dupM)
import qualified Isl.Linear as Isl
import Alpha.Surface.Core (System)
import qualified Alpha.Core as Core
import Alpha.Surface.Elaborate (elaborate, ElabMode(..))
import Alpha.Lower (lowerSystem)
import Alpha.Schedule
  ( Schedule(..), EqSchedule(..), DimAnnotation(..) )
import Alpha.Polyhedral.Dependence
  ( lowerScheduleMaps, projectBodyReads, computeAllDeps
  , buildUnionFromNamed )
import Alpha.Codegen (ReduceInfo(..), buildReduceMap)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Error type
-- ═══════════════════════════════════════════════════════════════════════

data AnnotationError
  = CarriedDependence !Int !DimAnnotation !String
    -- ^ Dim carries a dependence; 'Parallel' or 'Vectorize' is unsound.
  | ParallelOnReductionDim !Int !DimAnnotation !String
    -- ^ 'Parallel' / 'Vectorize' attached to a reduction dim of the
    -- named equation.  The dim carries the reduction's WAW on the
    -- accumulator; 'ReductionParallel' is required here.
  | ReductionOnNonReductionDim !Int !String
    -- ^ 'ReductionParallel' attached to a non-reduction dim: there is
    -- no reduction clause to emit.
  deriving (Show, Eq)

instance NFData AnnotationError where
  rnf (CarriedDependence d a s)        = rnf d `seq` rnf a `seq` rnf s
  rnf (ParallelOnReductionDim d a s)   = rnf d `seq` rnf a `seq` rnf s
  rnf (ReductionOnNonReductionDim d s) = rnf d `seq` rnf s


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Public API
-- ═══════════════════════════════════════════════════════════════════════

validateAnnotations
  :: forall ps pctx inputs outputs locals.
     KnownSymbols ps
  => System ps pctx inputs outputs locals
  -> Schedule
  -> IO (Either AnnotationError ())
validateAnnotations sys sched =
  elaborate @ps @pctx @inputs @outputs @locals @Double TrustPlugin sys $
    \r -> case r of
      Left e      -> error ("Alpha.Codegen.Parallel.validateAnnotations: BUG: TrustPlugin elaborate failed: " ++ show e)
      Right coreSys -> validateCore coreSys sched

-- | Polymorphic in @sys@ so the elaborator-bound skolem flows through;
-- the scalar @a@ is irrelevant here (no expression evaluation), so any
-- 'AlphaScalar' choice at the 'elaborate' call site is sound.
validateCore
  :: forall sys a.
     Core.System sys a
  -> Schedule
  -> IO (Either AnnotationError ())
validateCore sys sched =
  let redMap = buildReduceMap sys
  in case validateReductionDims sched redMap of
    Left err -> pure (Left err)
    Right () ->
      -- ReductionParallel is sound by construction (OMP reduction clause);
      -- only Parallel/Vectorize need the carried-dep check below.
      let annotations = mergedAnnotationsFiltered sched
      in if Map.null annotations
        then pure (Right ())
        else runIslT $ Isl.do
          let (domains, writes, reads_, projections) = lowerSystem sys
              schedMaps = lowerScheduleMaps sched domains
              nOut = case schedMaps of { (sm:_) -> nmNOut sm; [] -> 0 }

          Ur eqReads <- projectBodyReads reads_ projections
          Ur flowDeps <- computeAllDeps eqReads writes
          case flowDeps of
            [] -> Isl.pure (Ur (Right ()))
            (d:ds) -> Isl.do
              allDeps <- Isl.foldM (\acc x -> UM.union acc x) d ds
              schedUM <- buildUnionFromNamed schedMaps

              -- Scheduled deps: S ∘ D ∘ S⁻¹
              (s1, s2) <- dupM schedUM
              step1 <- UM.applyRange allDeps s1
              schedDeps <- UM.applyDomain step1 s2

              Ur result <- checkDims schedDeps nOut (Map.toAscList annotations)
              Isl.pure (Ur result)

-- | Per-equation local check: @Parallel@/@Vectorize@ forbidden on
-- reduction dims, @ReductionParallel@ forbidden on non-reduction dims.
--
-- Reduction dims occupy positions @[esNTime .. esNTime + riRedDims - 1]@
-- in the extended schedule (see 'Alpha.Codegen.lowerScheduleMaps').
validateReductionDims
  :: Schedule
  -> Map.Map String ReduceInfo
  -> Either AnnotationError ()
validateReductionDims (Schedule entries) redMap =
  Map.foldrWithKey checkEq (Right ()) entries
  where
    checkEq _ _ err@(Left _) = err
    checkEq name es (Right ()) =
      let nTime    = esNTime es
          nRedDims = maybe 0 riRedDims (Map.lookup name redMap)
          isRedDim d = d >= nTime && d < nTime + nRedDims
      in Map.foldrWithKey (checkDim name isRedDim) (Right ()) (esAnnotations es)
    checkDim _ _ _ _ err@(Left _) = err
    checkDim name isRedDim d ann (Right ()) = case ann of
      Parallel          | isRedDim d ->
        Left (ParallelOnReductionDim d Parallel name)
      Vectorize         | isRedDim d ->
        Left (ParallelOnReductionDim d Vectorize name)
      ReductionParallel | not (isRedDim d) ->
        Left (ReductionOnNonReductionDim d name)
      _ -> Right ()


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Per-dimension checking
-- ═══════════════════════════════════════════════════════════════════════

checkDims :: Isl.UnionMap %1 -> Int -> [(Int, DimAnnotation)]
          -> IslT IO (Ur (Either AnnotationError ()))
checkDims deps _nOut [] = Isl.do
  freeM deps
  Isl.pure (Ur (Right ()))
checkDims deps nOut ((dim, ann):rest) = Isl.do
  carried <- buildCarriedAt nOut dim
  (deps1, deps2) <- dupM deps
  violations <- UM.intersect deps1 carried
  Ur empty <- query_ violations UM.isEmpty
  if empty
    then checkDims deps2 nOut rest
    else Isl.do
      freeM deps2
      let label = show ann ++ " on dim " ++ show dim
                  ++ ": dependence carried at this level"
      Isl.pure (Ur (Left (CarriedDependence dim ann label)))

-- | Build: @{ [t0..t_{n-1}] -> [t0'..t_{n-1}'] : t_i=t_i' for i<k, t_k'>t_k }@
--
-- This captures dependences carried at level k: outer dims equal,
-- sink is strictly after source at dim k.
buildCarriedAt :: Int -> Int -> IslT IO Isl.UnionMap
buildCarriedAt nOut dim = Isl.do
  let -- Equality constraints: OutDim i - InDim i = 0 for i < dim
      eqCs = [ EqualityConstraint
                 (Add (Ix (OutDim i)) (Mul (-1) (Ix (InDim i))))
             | i <- [0 .. dim - 1] ]
      -- Strict forward: OutDim dim - InDim dim - 1 >= 0 (t_k' > t_k)
      fwdC = InequalityConstraint
               (Add (Ix (OutDim dim))
                    (Add (Mul (-1) (Ix (InDim dim)))
                         (Constant (-1))))
      conj = Conjunction (eqCs ++ [fwdC])
  bm <- buildBasicMap [] nOut nOut conj
  m <- RawM.fromBasicMap bm
  RawM.toUnionMap m


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Helpers
-- ═══════════════════════════════════════════════════════════════════════

mergedAnnotations :: Schedule -> Map.Map Int DimAnnotation
mergedAnnotations (Schedule entries) =
  Map.unionsWith (\a b -> if a == b then a else error $
    "conflicting annotations on same dim: " ++ show a ++ " vs " ++ show b)
    [esAnnotations es | es <- Map.elems entries]

-- | Like 'mergedAnnotations', but drops 'ReductionParallel' entries
-- before merging, so a reduction annotation on one equation doesn't
-- spuriously conflict with a Parallel/Vectorize on a different one.
mergedAnnotationsFiltered :: Schedule -> Map.Map Int DimAnnotation
mergedAnnotationsFiltered (Schedule entries) =
  Map.unionsWith (\a b -> if a == b then a else error $
    "conflicting annotations on same dim: " ++ show a ++ " vs " ++ show b)
    [ Map.filter (/= ReductionParallel) (esAnnotations es)
    | es <- Map.elems entries ]


