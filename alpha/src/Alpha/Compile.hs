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

import Isl.Typed.Constraints (NamedMap(..), buildUnionMapFromNamed)
import Isl.Typed.Params (KnownSymbols)
import qualified Isl.Types as Isl
import qualified Isl.Map as RawM
import qualified Isl.UnionMap as UM
import qualified Isl.Space as Space
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_, freeM, dup)
import qualified Isl.Linear as Isl
import Alpha.Core (System, pattern System, eqListNames)
import Alpha.Lower (lowerSystem)
import Alpha.Schedule (Schedule(..), schedEntries)
import Alpha.Allocation (Allocation(..))
import Alpha.Polyhedral.Dependence
  ( lowerScheduleMaps, projectBodyReads, computeAllDeps, freeAll )


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
  deriving (Show, Eq)

instance NFData CompileError where
  rnf (ScheduleViolation s)     = rnf s
  rnf (ContractionUnsafe s)     = rnf s
  rnf (ScheduleIncomplete xs)   = rnf xs
  rnf (ScheduleOverspecified xs) = rnf xs


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Public API
-- ═══════════════════════════════════════════════════════════════════════

compile
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule
  -> Allocation
  -> IO (Either CompileError ())
compile sys@(System _decls eqs) sched _alloc =
  let eqNames    = Set.fromList (eqListNames eqs)
      schedNames = Set.fromList (Map.keys (schedEntries sched))
      missing    = Set.toAscList (eqNames `Set.difference` schedNames)
      extra      = Set.toAscList (schedNames `Set.difference` eqNames)
  in case (Set.null eqNames && Set.null schedNames, missing, extra) of
    (True, _, _)    -> pure (Right ())
    (_, _:_, _)     -> pure (Left (ScheduleIncomplete missing))
    (_, _, _:_)     -> pure (Left (ScheduleOverspecified extra))
    (_, [], [])     -> runIslT $ Isl.do
      let (domains, writes, reads_, projections) = lowerSystem sys
          schedMaps = lowerScheduleMaps sched domains
      Ur eqReads <- projectBodyReads reads_ projections
      Ur flowDeps <- computeAllDeps eqReads writes
      Ur schedUMs <- Isl.mapM buildUnionMapFromNamed schedMaps
      case schedUMs of
        -- Coverage check above ruled out the empty case; any empty
        -- result here indicates the lowering itself produced nothing,
        -- which is a bug.
        [] -> Isl.pure (Ur (Left (ScheduleViolation "lowering produced no schedule maps")))
        (first':rest') ->
          let mkSched = Isl.foldM (\acc x -> UM.union acc x) first' rest'
          in case flowDeps of
            []     -> Isl.do schedUM <- mkSched; freeM schedUM; Isl.pure (Ur (Right ()))
            (d:ds) -> Isl.do
              Ur result <- validateDeps (d :| ds) mkSched (schedNOut schedMaps) "flow"
              Isl.pure (Ur result)

-- | Validate a schedule without an allocation (flow deps only).
validateSchedule
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule
  -> IO (Either CompileError ())
validateSchedule sys sched =
  compile sys sched (Allocation Map.empty)

schedNOut :: [NamedMap] -> Int
schedNOut (sm:_) = nmNOut sm
schedNOut []     = 0


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Lex-positivity check
-- ═══════════════════════════════════════════════════════════════════════

buildLexGe :: Int -> IslT IO Isl.UnionMap
buildLexGe n = Isl.do
  sp <- Space.setAlloc 0 n
  rawMap <- RawM.lexGe sp
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

validateDeps :: NonEmpty Isl.UnionMap -> IslT IO Isl.UnionMap -> Int -> String
             -> IslT IO (Ur (Either CompileError ()))
validateDeps (d :| ds) mkSched nOut label = Isl.do
  allDeps <- Isl.foldM (\acc x -> UM.union acc x) d ds
  sched <- mkSched
  let !(sched1, sched2) = dup sched
  Ur result <- checkLexPositivity allDeps sched1 sched2 nOut
  case result of
    Nothing  -> Isl.pure (Ur (Right ()))
    Just vio -> Isl.pure (Ur (Left (ScheduleViolation (label <> " deps violated: " <> vio))))
