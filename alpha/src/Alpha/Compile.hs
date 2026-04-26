{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Compilation pipeline for Alpha systems: validates a surface
-- 'System' against its 'Schedule' and 'Allocation' and returns an
-- opaque 'Compiled' artifact consumed by 'Alpha.Codegen.codegen'.
module Alpha.Compile
  ( CompileError(..)
  , compile
  , validateSchedule
    -- * Compiled artifact (constructor is module-private)
  , Compiled
  , withCompiledCore
  , compiledSchedule
  , compiledAllocation
  , compiledParams
  , compiledDomains
  , compiledReduceMap
  , compiledSchedMaps
  ) where

import Control.DeepSeq (NFData(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import GHC.TypeLits (KnownNat)

import Isl.Typed.Constraints
  ( NamedSet(..), NamedMap(..)
  , Constraint(..), Conjunction(..), MapIx(..), Expr(..)
  , buildUnionMapFromNamed, buildBasicMap )
import Isl.Typed.Params (KnownSymbols, Length, symbolVals)
import qualified Isl.Types as Isl
import qualified Isl.Map as RawM
import qualified Isl.UnionMap as UM
import qualified Isl.Space as Space
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_, freeM, dupM)
import qualified Isl.Linear as Isl

import Alpha.Surface.Core (System, pattern System, eqListNames)
import Alpha.Surface.Elaborate (elaborate, ElabMode(..))
import qualified Alpha.Core as Core
import Alpha.Lower (lowerSystem)
import Alpha.Schedule
  ( Schedule(..), EqSchedule(..), DimAnnotation(..), schedEntries )
import Alpha.Allocation (Allocation(..), EqStorage(..))
import Alpha.Polyhedral.Contraction (storageToNamedMap', contractionOutputDeps)
import Alpha.Polyhedral.Dependence
  ( lowerScheduleMaps, projectBodyReads, computeAllDeps, freeAll )
import Alpha.Polyhedral.Reduce (ReduceInfo(..), buildReduceMap)
import Alpha.Transform.NormalizeCases (normalizeCases)
import Alpha.Transform.Types (TransformError)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Error type
-- ═══════════════════════════════════════════════════════════════════════

data CompileError
  = ScheduleViolation !String
  | ContractionUnsafe !String
  | ScheduleIncomplete ![String]
    -- ^ Equations defined in the system but not scheduled.
  | ScheduleOverspecified ![String]
    -- ^ Names present in the schedule but not in the system.
  | OutputDependenceViolated !String
    -- ^ Post-contraction WAW: two aliasing writes land on the same
    -- schedule point.  The payload names the contracted equation
    -- whose storage map produced the collision.
  | TransformFailed !TransformError
    -- ^ A pre-lowering transform (e.g., 'normalizeCases') reported an
    -- error.  Structural transforms never fail at runtime, so this
    -- signals an internal bug.
  | CarriedDependence !Int !DimAnnotation !String
    -- ^ Dim carries a dependence; 'Parallel' or 'Vectorize' is unsound.
  | ParallelOnReductionDim !Int !DimAnnotation !String
    -- ^ 'Parallel' / 'Vectorize' attached to a reduction dim of the
    -- named equation.  The dim carries the reduction's WAW on the
    -- accumulator; 'ReductionParallel' is required here.
  | ReductionOnNonReductionDim !Int !String
    -- ^ 'ReductionParallel' attached to a non-reduction dim: there is
    -- no reduction clause to emit.
  deriving (Show, Eq)

instance NFData CompileError where
  rnf (ScheduleViolation s)        = rnf s
  rnf (ContractionUnsafe s)        = rnf s
  rnf (ScheduleIncomplete xs)      = rnf xs
  rnf (ScheduleOverspecified xs)   = rnf xs
  rnf (OutputDependenceViolated s) = rnf s
  rnf (TransformFailed e)          = e `seq` ()
  rnf (CarriedDependence d a s)        = rnf d `seq` rnf a `seq` rnf s
  rnf (ParallelOnReductionDim d a s)   = rnf d `seq` rnf a `seq` rnf s
  rnf (ReductionOnNonReductionDim d s) = rnf d `seq` rnf s


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Compiled artifact
-- ═══════════════════════════════════════════════════════════════════════

-- | Opaque output of a successful 'compile'.  The constructor is
-- module-private: a 'Compiled' value cannot be fabricated without
-- going through validation.
data Compiled ps pctx inputs outputs locals = forall sys. Compiled
  { _compiledCoreSys     :: !(Core.System sys Double)
  , compiledSchedule     :: !Schedule
  , compiledAllocation   :: !Allocation
  , compiledParams       :: ![String]
  , compiledDomains      :: ![NamedSet]
  , compiledWrites       :: ![NamedMap]
  , compiledReads        :: ![NamedMap]
  , compiledProjections  :: ![NamedMap]
  , compiledReduceMap    :: !(Map.Map String ReduceInfo)
  , compiledSchedMaps    :: ![NamedMap]
    -- ^ Dependence-variant schedule maps.  The reduce-aware variant
    -- (extended with reduction dims) is recomputed by 'Alpha.Codegen'.
  , compiledContracted   :: ![(String, NonEmpty NamedMap, NamedMap)]
  }

-- | CPS-unpack the elaborated 'Core.System' from a 'Compiled'; binds
-- the existential @sys@ skolem under the continuation.
withCompiledCore
  :: Compiled ps pctx i o l
  -> (forall sys. Core.System sys Double -> r)
  -> r
withCompiledCore (Compiled { _compiledCoreSys = sys }) k = k sys


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Public API
-- ═══════════════════════════════════════════════════════════════════════

compile
  :: forall ps pctx inputs outputs locals.
     (KnownSymbols ps, KnownNat (Length ps))
  => System ps pctx inputs outputs locals
  -> Schedule
  -> Allocation
  -> IO (Either CompileError (Compiled ps pctx inputs outputs locals))
compile sys0@(System _decls eqs) sched alloc =
  let eqNames    = Set.fromList (eqListNames eqs)
      schedNames = Set.fromList (Map.keys (schedEntries sched))
      missing    = Set.toAscList (eqNames `Set.difference` schedNames)
      extra      = Set.toAscList (schedNames `Set.difference` eqNames)
  in case (missing, extra) of
    (_:_, _) -> pure (Left (ScheduleIncomplete missing))
    (_, _:_) -> pure (Left (ScheduleOverspecified extra))
    ([], []) ->
      -- Hoist 'Case' nodes above 'Pw'/'PMap' so lowering can emit one
      -- polyhedral statement per branch.
      case normalizeCases sys0 of
        Left terr -> pure (Left (TransformFailed terr))
        Right sys ->
          elaborate @ps @pctx @inputs @outputs @locals @Double TrustPlugin sys $
            \res -> case res of
              Left elabErr ->
                error ("Alpha.Compile.compile: BUG: TrustPlugin elaborate failed: " ++ show elabErr)
              Right coreSys -> compileCore coreSys sched alloc

-- | Validate a schedule without an allocation: flow positivity and
-- annotation soundness only.  Unsound 'Parallel' / 'Vectorize' are
-- rejected here even when no 'Allocation' is supplied.
validateSchedule
  :: forall ps pctx inputs outputs locals.
     (KnownSymbols ps, KnownNat (Length ps))
  => System ps pctx inputs outputs locals
  -> Schedule
  -> IO (Either CompileError ())
validateSchedule sys sched =
  fmap (fmap (const ())) (compile sys sched (Allocation Map.empty))


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Core pipeline
-- ═══════════════════════════════════════════════════════════════════════

-- Polymorphic in @sys@ so the elaborator-bound skolem flows through
-- to the existential pack.
compileCore
  :: forall sys ps pctx inputs outputs locals.
     KnownSymbols ps
  => Core.System sys Double
  -> Schedule
  -> Allocation
  -> IO (Either CompileError (Compiled ps pctx inputs outputs locals))
compileCore coreSys sched alloc =
  let (domains, writes, reads_, projections) = lowerSystem coreSys
      reduceMap   = buildReduceMap coreSys
      params      = symbolVals @ps
      schedMaps   = lowerScheduleMaps sched domains
      contracted  = collectContracted params alloc writes
  in case validateReductionDims sched reduceMap of
    Left err -> pure (Left err)
    Right () -> do
      -- 'Compiled' is existential and cannot easily be 'NFData', so
      -- we run the IslT checks returning @Either CompileError ()@
      -- and pack the artifact outside the runIslT scope.
      checkRes <- runIslT $ Isl.do
        Ur eqReads  <- projectBodyReads reads_ projections
        Ur flowDeps <- computeAllDeps eqReads writes
        runChecks flowDeps schedMaps contracted sched
      pure $ case checkRes of
        Left err -> Left err
        Right () -> Right Compiled
          { _compiledCoreSys    = coreSys
          , compiledSchedule    = sched
          , compiledAllocation  = alloc
          , compiledParams      = params
          , compiledDomains     = domains
          , compiledWrites      = writes
          , compiledReads       = reads_
          , compiledProjections = projections
          , compiledReduceMap   = reduceMap
          , compiledSchedMaps   = schedMaps
          , compiledContracted  = contracted
          }

-- Flow / WAW / annotation checks.  The shared
-- @schedDeps = S ∘ flowDeps ∘ S⁻¹@ relation is built once and dup'd
-- across the flow-positivity and carried-dep checks; the WAW check
-- builds its own analogous relation from the storage-composed writes.
runChecks
  :: [Isl.UnionMap]
  -> [NamedMap]
  -> [(String, NonEmpty NamedMap, NamedMap)]
  -> Schedule
  -> IslT IO (Ur (Either CompileError ()))
runChecks flowDeps schedMaps contracted sched = Isl.do
  Ur schedUMs <- Isl.mapM buildUnionMapFromNamed schedMaps
  case schedUMs of
    [] -> Isl.do
      -- Empty schedule ⇒ all checks vacuous.  Unreachable in practice
      -- after the coverage check (a non-empty system has non-empty
      -- domains, hence non-empty schedMaps), but flowDeps may still
      -- carry an Ur-wrapped UnionMap that the linear checker won't
      -- catch — free it explicitly.
      freeAll flowDeps
      Isl.pure (Ur (Right ()))
    (first':rest') -> Isl.do
      schedUM <- Isl.foldM (\acc x -> UM.union acc x) first' rest'
      let nOut = schedNOut schedMaps
      case flowDeps of
        []     -> Isl.do
          freeM schedUM
          runWawCheck contracted schedMaps
        (d:ds) -> Isl.do
          allDeps <- Isl.foldM (\acc x -> UM.union acc x) d ds
          (sched1, sched2) <- dupM schedUM
          step1     <- UM.applyRange allDeps sched1
          schedDeps <- UM.applyDomain step1 sched2
          (sd1, sd2) <- dupM schedDeps
          Ur posResult <- checkLexPositivityOnSched sd1 nOut
          case posResult of
            Just vio -> Isl.do
              freeM sd2
              Isl.pure (Ur (Left (ScheduleViolation
                ("flow deps violated: " <> vio))))
            Nothing -> Isl.do
              let annotations = collectAnnotationsToCheck sched
              if null annotations
                then Isl.do
                  freeM sd2
                  runWawCheck contracted schedMaps
                else Isl.do
                  Ur annResult <- checkAnnotations sd2 nOut annotations
                  case annResult of
                    Left err -> Isl.pure (Ur (Left err))
                    Right () -> runWawCheck contracted schedMaps

runWawCheck
  :: [(String, NonEmpty NamedMap, NamedMap)]
  -> [NamedMap]
  -> IslT IO (Ur (Either CompileError ()))
runWawCheck [] _ = Isl.pure (Ur (Right ()))
runWawCheck ((eqName, writeNMs, storageNM) : rest) schedMaps = Isl.do
  -- Case-split equations contribute N per-branch write maps; union
  -- them so 'contractionOutputDeps' sees the full logical write.
  writeUM1    <- unionMapsFromNamed writeNMs
  writeUM2    <- unionMapsFromNamed writeNMs
  storageUM1  <- buildUnionMapFromNamed storageNM
  storageUM2  <- buildUnionMapFromNamed storageNM
  waw         <- contractionOutputDeps writeUM1 writeUM2 storageUM1 storageUM2
  Ur schedUMs <- Isl.mapM buildUnionMapFromNamed schedMaps
  case schedUMs of
    [] -> Isl.do
      freeM waw
      Isl.pure (Ur (Left (ScheduleViolation "lowering produced no schedule maps")))
    (h:t) -> Isl.do
      sched <- Isl.foldM (\acc x -> UM.union acc x) h t
      (sched1, sched2) <- dupM sched
      Ur race <- checkContractionRaceFree waw sched1 sched2 (schedNOut schedMaps)
      case race of
        Just _  -> Isl.pure (Ur (Left (OutputDependenceViolated eqName)))
        Nothing -> runWawCheck rest schedMaps


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Lex-positivity / race / carried-dep checks
-- ═══════════════════════════════════════════════════════════════════════

buildLexGe :: Int -> IslT IO Isl.UnionMap
buildLexGe n = Isl.do
  sp <- Space.setAlloc 0 n
  rawMap <- RawM.lexGe sp
  RawM.toUnionMap rawMap

buildLexLe :: Int -> IslT IO Isl.UnionMap
buildLexLe n = Isl.do
  sp <- Space.setAlloc 0 n
  rawMap <- RawM.lexLe sp
  RawM.toUnionMap rawMap

-- Tests @schedDep ∩ lex_ge = ∅@; consumes the relation.
checkLexPositivityOnSched :: Isl.UnionMap %1 -> Int
                          -> IslT IO (Ur (Maybe String))
checkLexPositivityOnSched schedDep nOut = Isl.do
  lexGe <- buildLexGe nOut
  violations <- UM.intersect schedDep lexGe
  Ur empty <- query_ violations UM.isEmpty
  if empty
    then Isl.pure (Ur Nothing)
    else Isl.pure (Ur (Just "schedule violates dependence ordering"))

-- Post-contraction WAW race check.  @waw@ is symmetric, so a
-- lex-positivity test would be degenerate; instead we reject iff
-- @S ∘ waw ∘ S⁻¹@ intersects the schedule-space diagonal
-- (@lexGe ∩ lexLe@) — i.e. two aliasing writes share a schedule point.
checkContractionRaceFree :: Isl.UnionMap %1 -> Isl.UnionMap %1 -> Isl.UnionMap %1 -> Int
                         -> IslT IO (Ur (Maybe String))
checkContractionRaceFree waw sched schedCopy nOut = Isl.do
  step1    <- UM.applyRange waw sched
  schedWaw <- UM.applyDomain step1 schedCopy
  lexGe    <- buildLexGe nOut
  lexLe    <- buildLexLe nOut
  lexEq    <- UM.intersect lexGe lexLe
  races    <- UM.intersect schedWaw lexEq
  Ur empty <- query_ races UM.isEmpty
  if empty
    then Isl.pure (Ur Nothing)
    else Isl.pure (Ur (Just "contracted storage aliases at the same schedule point"))

-- Walks each (dim, annotation) pair and rejects if @schedDeps@
-- intersects the carried-at-dim relation.  Consumes @deps@.
checkAnnotations
  :: Isl.UnionMap %1
  -> Int
  -> [(Int, DimAnnotation)]
  -> IslT IO (Ur (Either CompileError ()))
checkAnnotations deps _nOut [] = Isl.do
  freeM deps
  Isl.pure (Ur (Right ()))
checkAnnotations deps nOut ((dim, ann):rest) = Isl.do
  carried <- buildCarriedAt nOut dim
  (deps1, deps2) <- dupM deps
  violations <- UM.intersect deps1 carried
  Ur empty <- query_ violations UM.isEmpty
  if empty
    then checkAnnotations deps2 nOut rest
    else Isl.do
      freeM deps2
      let label = show ann ++ " on dim " ++ show dim
                  ++ ": dependence carried at this level"
      Isl.pure (Ur (Left (CarriedDependence dim ann label)))

-- @{ [t0..t_{n-1}] -> [t0'..t_{n-1}'] : t_i=t_i' for i<k, t_k'>t_k }@
buildCarriedAt :: Int -> Int -> IslT IO Isl.UnionMap
buildCarriedAt nOut dim = Isl.do
  let eqCs = [ EqualityConstraint
                 (Add (Ix (OutDim i)) (Mul (-1) (Ix (InDim i))))
             | i <- [0 .. dim - 1] ]
      fwdC = InequalityConstraint
               (Add (Ix (OutDim dim))
                    (Add (Mul (-1) (Ix (InDim dim)))
                         (Constant (-1))))
      conj = Conjunction (eqCs ++ [fwdC])
  bm <- buildBasicMap [] nOut nOut conj
  m  <- RawM.fromBasicMap bm
  RawM.toUnionMap m


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Reduction-dim annotation soundness (pure)
-- ═══════════════════════════════════════════════════════════════════════

-- 'Parallel' / 'Vectorize' on reduction dims and 'ReductionParallel'
-- on non-reduction dims are both rejected.  Reduction dims occupy
-- positions @[esNTime .. esNTime + riRedDims - 1]@ in the extended
-- schedule (see 'Alpha.Codegen.lowerScheduleMaps').
validateReductionDims
  :: Schedule
  -> Map.Map String ReduceInfo
  -> Either CompileError ()
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

-- (dim, annotation) pairs to carried-dep-check, deduplicated.
-- Drops 'ReductionParallel' (sound by construction via OMP clause).
-- Conflicting non-reduction annotations on the same dim are caught
-- later by 'Alpha.Codegen' as 'ConflictingAnnotation'.
collectAnnotationsToCheck :: Schedule -> [(Int, DimAnnotation)]
collectAnnotationsToCheck (Schedule entries) =
  Set.toAscList $ Set.fromList
    [ (dim, ann)
    | es <- Map.elems entries
    , (dim, ann) <- Map.toList (esAnnotations es)
    , ann /= ReductionParallel
    ]


-- ═══════════════════════════════════════════════════════════════════════
-- §7. Helpers
-- ═══════════════════════════════════════════════════════════════════════

schedNOut :: [NamedMap] -> Int
schedNOut (sm:_) = nmNOut sm
schedNOut []     = 0

unionMapsFromNamed :: NonEmpty NamedMap -> IslT IO Isl.UnionMap
unionMapsFromNamed (nm :| nms) = Isl.do
  u0 <- buildUnionMapFromNamed nm
  Isl.foldM
    (\acc n -> Isl.do
       u <- buildUnionMapFromNamed n
       UM.union acc u)
    u0 nms

-- Pair contracted equations with their write maps and storage map.
-- Keyed by 'allocEntries' (logical array name); after the Case fan-out
-- domain names carry @__brI@ suffixes, so iterating by domain would
-- drop contracted split equations.
collectContracted
  :: [String]
  -> Allocation
  -> [NamedMap]
  -> [(String, NonEmpty NamedMap, NamedMap)]
collectContracted params alloc writes =
  [ (eqName, ws, storageNM)
  | (eqName, Contracted stor) <- Map.toAscList (allocEntries alloc)
  , Just ws <- [NE.nonEmpty (lookupWriteMaps eqName writes)]
  , let nDims     = nmNOut (NE.head ws)
        storageNM = storageToNamedMap' eqName (eqName ++ "__buf") params stor nDims
  ]

lookupWriteMaps :: String -> [NamedMap] -> [NamedMap]
lookupWriteMaps name = filter (\w -> nmRangeName w == Just name)
