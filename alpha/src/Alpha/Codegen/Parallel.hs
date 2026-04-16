{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
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
  , MapIx(..), Expr(..), buildBasicMap, buildUnionMapFromNamed )
import Isl.Typed.Params (KnownSymbols)
import qualified Isl.Types as Isl
import qualified Isl.Map as RawM
import qualified Isl.UnionMap as UM
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_, freeM, dup)
import qualified Isl.Linear as Isl
import Alpha.Core (System)
import Alpha.Lower (lowerSystem)
import Alpha.Schedule
  ( Schedule(..), EqSchedule(..), DimAnnotation(..) )
import Alpha.Polyhedral.Dependence
  ( lowerScheduleMaps, projectBodyReads, computeAllDeps
  , buildUnionFromNamed )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Error type
-- ═══════════════════════════════════════════════════════════════════════

data AnnotationError
  = CarriedDependence !Int !DimAnnotation !String
  deriving (Show, Eq)

instance NFData AnnotationError where
  rnf (CarriedDependence d a s) = rnf d `seq` rnf a `seq` rnf s

instance NFData DimAnnotation where
  rnf Parallel  = ()
  rnf Vectorize = ()


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Public API
-- ═══════════════════════════════════════════════════════════════════════

validateAnnotations
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule
  -> IO (Either AnnotationError ())
validateAnnotations sys sched = do
  let annotations = mergedAnnotations sched
  if Map.null annotations
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
          let !(s1, s2) = dup schedUM
          step1 <- UM.applyRange allDeps s1
          schedDeps <- UM.applyDomain step1 s2

          Ur result <- checkDims schedDeps nOut (Map.toAscList annotations)
          Isl.pure (Ur result)


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
  let !(deps1, deps2) = dup deps
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
  Map.unions [esAnnotations es | es <- Map.elems entries]
