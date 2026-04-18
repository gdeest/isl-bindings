{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Compilation pipeline for Alpha systems.
--
-- Validates the schedule against data dependences via ISL's
-- lexicographic positivity check.
module Alpha.Compile
  ( CompileError(..)
  , compile
  , validateSchedule
  ) where

import Control.DeepSeq (NFData(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import GHC.TypeLits (KnownNat)

import Isl.Typed.Constraints (NamedMap(..), buildUnionMapFromNamed)
import Isl.Typed.Params (KnownSymbols, Length, symbolVals)
import qualified Isl.Types as Isl
import qualified Isl.Map as RawM
import qualified Isl.UnionMap as UM
import qualified Isl.Space as Space
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_, freeM, dupM)
import qualified Isl.Linear as Isl
import Alpha.Core (System, pattern System, eqListNames)
import Alpha.Lower (lowerSystem)
import Alpha.Schedule (Schedule(..), schedEntries)
import Alpha.Allocation (Allocation(..), EqStorage(..))
import Alpha.Polyhedral.Contraction (storageToNamedMap', contractionOutputDeps)
import Alpha.Polyhedral.Dependence
  ( lowerScheduleMaps, projectBodyReads, computeAllDeps )
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
  deriving (Show, Eq)

instance NFData CompileError where
  rnf (ScheduleViolation s)     = rnf s
  rnf (ContractionUnsafe s)     = rnf s
  rnf (ScheduleIncomplete xs)   = rnf xs
  rnf (ScheduleOverspecified xs) = rnf xs
  rnf (OutputDependenceViolated s) = rnf s
  rnf (TransformFailed e)       = e `seq` ()


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Public API
-- ═══════════════════════════════════════════════════════════════════════

compile
  :: forall ps pctx inputs outputs locals.
     (KnownSymbols ps, KnownNat (Length ps))
  => System ps pctx inputs outputs locals
  -> Schedule
  -> Allocation
  -> IO (Either CompileError ())
compile sys0@(System _decls eqs) sched alloc =
  let eqNames    = Set.fromList (eqListNames eqs)
      schedNames = Set.fromList (Map.keys (schedEntries sched))
      missing    = Set.toAscList (eqNames `Set.difference` schedNames)
      extra      = Set.toAscList (schedNames `Set.difference` eqNames)
  in case (Set.null eqNames && Set.null schedNames, missing, extra) of
    (True, _, _)    -> pure (Right ())
    (_, _:_, _)     -> pure (Left (ScheduleIncomplete missing))
    (_, _, _:_)     -> pure (Left (ScheduleOverspecified extra))
    (_, [], [])     ->
      -- Phase A: hoist 'Case' nodes above 'Pw'/'PMap' so the lowering
      -- fan-out can emit one polyhedral statement per branch.
      case normalizeCases sys0 of
        Left terr -> pure (Left (TransformFailed terr))
        Right sys -> runIslT $ Isl.do
          let (domains, writes, reads_, projections) = lowerSystem sys
              schedMaps = lowerScheduleMaps sched domains
              params    = symbolVals @ps
              -- Contracted equations paired with their (list of
              -- per-branch write maps, storage map).  After the Case
              -- fan-out, a single contracted equation may be backed by
              -- N write maps; the WAW check unions them before pushing
              -- through storage composition.  Empty list ⇒ no
              -- post-contraction aliasing possible, WAW check skipped.
              contracted = collectContracted params alloc writes
          Ur eqReads <- projectBodyReads reads_ projections
          Ur flowDeps <- computeAllDeps eqReads writes
          -- Flow-dep lex-positivity check.
          Ur flowResult <- runFlowCheck flowDeps schedMaps
          case flowResult of
            Left err -> Isl.pure (Ur (Left err))
            Right () -> runWawCheck params contracted schedMaps

-- | Flow-dep lex-positivity check, factored out for composition with
-- the post-contraction WAW check.
runFlowCheck
  :: [Isl.UnionMap]
  -> [NamedMap]
  -> IslT IO (Ur (Either CompileError ()))
runFlowCheck flowDeps schedMaps = Isl.do
  Ur schedUMs <- Isl.mapM buildUnionMapFromNamed schedMaps
  case schedUMs of
    -- Coverage check above ruled out the empty case; any empty result
    -- here indicates the lowering itself produced nothing, a bug.
    [] -> Isl.pure (Ur (Left (ScheduleViolation "lowering produced no schedule maps")))
    (first':rest') ->
      let mkSched = Isl.foldM (\acc x -> UM.union acc x) first' rest'
      in case flowDeps of
        []     -> Isl.do schedUM <- mkSched; freeM schedUM; Isl.pure (Ur (Right ()))
        (d:ds) -> validateDeps (d :| ds) mkSched (schedNOut schedMaps) "flow"

-- | Run the post-contraction WAW check for every contracted equation.
--
-- Returns @Ur@-wrapped since this is the terminal step of @compile@'s
-- @runIslT@ block.  @FullStorage@-only allocations (including the
-- empty allocation produced by @validateSchedule@) short-circuit to
-- @Right ()@ — pre-contraction every equation writes its own virtual
-- cell, so no aliasing is possible.
runWawCheck
  :: [String]
  -> [(String, [NamedMap], NamedMap)]
  -> [NamedMap]
  -> IslT IO (Ur (Either CompileError ()))
runWawCheck _params [] _schedMaps = Isl.pure (Ur (Right ()))
runWawCheck _params (entry : rest) schedMaps =
  let (eqName, writeNMs, storageNM) = entry in Isl.do
  -- Build writeUM (×2) and storageUM (×2) for 'contractionOutputDeps'.
  -- For Case-split equations there are N per-branch write maps;
  -- 'unionMapsFromNamed' unions them so 'contractionOutputDeps'
  -- composes storage over the full logical write relation.  The
  -- resulting WAW relation then includes cross-branch aliasing pairs
  -- @(br_i[a], br_j[c])@ whenever @storage(a) = storage(c)@; the
  -- diagonal-of-lex test in 'checkContractionRaceFree' correctly
  -- demands cross-branch schedule disambiguation.
  writeUM1    <- unionMapsFromNamed writeNMs
  writeUM2    <- unionMapsFromNamed writeNMs
  storageUM1  <- buildUnionMapFromNamed storageNM
  storageUM2  <- buildUnionMapFromNamed storageNM
  waw         <- contractionOutputDeps writeUM1 writeUM2 storageUM1 storageUM2
  -- Push through schedule and test race-freeness (no two aliasing
  -- writes mapped to the same schedule point).
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
        Just _ -> Isl.pure (Ur (Left (OutputDependenceViolated eqName)))
        Nothing -> runWawCheck _params rest schedMaps

-- | Validate a schedule without an allocation (flow deps only).
validateSchedule
  :: forall ps pctx inputs outputs locals.
     (KnownSymbols ps, KnownNat (Length ps))
  => System ps pctx inputs outputs locals
  -> Schedule
  -> IO (Either CompileError ())
validateSchedule sys sched =
  compile sys sched (Allocation Map.empty)

schedNOut :: [NamedMap] -> Int
schedNOut (sm:_) = nmNOut sm
schedNOut []     = 0

-- | Build one 'Isl.UnionMap' that is the union of all given
-- 'NamedMap's.  Precondition: the input list is non-empty (callers
-- that might have an empty list should short-circuit before calling
-- this; 'collectContracted' already filters those out).
unionMapsFromNamed :: [NamedMap] -> IslT IO Isl.UnionMap
unionMapsFromNamed []     =
  error "Alpha.Compile.unionMapsFromNamed: empty list (collectContracted invariant violated)"
unionMapsFromNamed (nm:nms) = Isl.do
  u0 <- buildUnionMapFromNamed nm
  Isl.foldM
    (\acc n -> Isl.do
       u <- buildUnionMapFromNamed n
       UM.union acc u)
    u0 nms

-- | Gather contracted equations paired with their write-access
-- 'NamedMap's (one per lowered statement — N per Case-split equation,
-- one otherwise) and storage-map 'NamedMap'.  Pre-contraction (every
-- equation 'FullStorage') the list is empty and the WAW check is
-- skipped — virtual SARE semantics already guarantees no write
-- aliasing.
--
-- Keyed by 'allocEntries' (logical array name), not by iteration
-- domains: after the Case-split fan-out the domain entries are named
-- @eqName__brI@, so a by-domain iteration would silently drop
-- contracted split equations.
collectContracted
  :: [String]     -- ^ parameter names (shared across the system)
  -> Allocation
  -> [NamedMap]   -- ^ write maps (from lowerSystem); multiple per
                  -- contracted Case-split equation
  -> [(String, [NamedMap], NamedMap)]
collectContracted params alloc writes =
  [ (eqName, ws, storageNM)
  | (eqName, Contracted stor) <- Map.toAscList (allocEntries alloc)
  , ws@(w0:_) <- [lookupWriteMaps eqName writes]
  -- All branches write to a space with the same rank as the equation;
  -- pick any branch's nmNOut.
  , let nDims     = nmNOut w0
        storageNM = storageToNamedMap' eqName (eqName ++ "__buf") params stor nDims
  ]

-- | All write maps targeting the given logical array (range) name.
-- Returns N entries when @name@ has been Case-fanned out into N
-- branch statements, exactly one entry otherwise.
lookupWriteMaps :: String -> [NamedMap] -> [NamedMap]
lookupWriteMaps name = filter (\w -> nmRangeName w == Just name)


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Lex-positivity check
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

checkLexPositivity :: Isl.UnionMap %1 -> Isl.UnionMap %1 -> Isl.UnionMap %1 -> Int
                   -> IslT IO (Ur (Maybe String))
checkLexPositivity deps sched schedCopy nOut = Isl.do
  step1 <- UM.applyRange deps sched
  schedDep <- UM.applyDomain step1 schedCopy
  lexGe <- buildLexGe nOut
  violations <- UM.intersect schedDep lexGe
  Ur empty <- query_ violations UM.isEmpty
  if empty
    then Isl.pure (Ur Nothing)
    else Isl.pure (Ur (Just "schedule violates dependence ordering"))

-- | Race-freeness check for post-contraction WAW.  The @waw@ relation
-- is /symmetric/ (if @(a, c)@ aliases then @(c, a)@ does too), so a
-- strict lex-positivity test is degenerate — any non-empty @waw@ would
-- fail in one of the two directions.  The meaningful property is:
-- /no two aliasing writes share the same schedule point/.  We push
-- @waw@ through the schedule and reject iff the result intersects the
-- schedule-space diagonal (@lexGe ∩ lexLe@).
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

validateDeps :: NonEmpty Isl.UnionMap -> IslT IO Isl.UnionMap -> Int -> String
             -> IslT IO (Ur (Either CompileError ()))
validateDeps (d :| ds) mkSched nOut label = Isl.do
  allDeps <- Isl.foldM (\acc x -> UM.union acc x) d ds
  sched <- mkSched
  (sched1, sched2) <- dupM sched
  Ur result <- checkLexPositivity allDeps sched1 sched2 nOut
  case result of
    Nothing  -> Isl.pure (Ur (Right ()))
    Just vio -> Isl.pure (Ur (Left (ScheduleViolation (label <> " deps violated: " <> vio))))
