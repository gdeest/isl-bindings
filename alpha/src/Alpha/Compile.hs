{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Compilation pipeline for Alpha systems.
--
-- Validates the schedule against data dependences via ISL's
-- lexicographic positivity check.  C codegen is deferred.
module Alpha.Compile
  ( CompileError(..)
  , compile
  , validateSchedule
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as Map

import Isl.Typed.Constraints
  ( NamedSet(..), NamedMap(..), buildUnionMapFromNamed )
import Isl.Typed.Params (KnownSymbols)
import qualified Alpha.Polyhedral.Schedule as S
import qualified Isl.Types as Isl
import qualified Isl.Map as RawM
import qualified Isl.UnionMap as UM
import qualified Isl.Space as Space
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_, freeM, dup, urWrap)
import qualified Isl.Linear as Isl
import Unsafe.Coerce (unsafeCoerce)

import Alpha.Core (System)
import Alpha.Lower (lowerSystem)
import Alpha.Schedule (Schedule(..), EqSchedule(esDef))
import Alpha.Allocation (Allocation(..))


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Error type
-- ═══════════════════════════════════════════════════════════════════════

data CompileError
  = ScheduleViolation !String
  | ContractionUnsafe !String
  deriving (Show, Eq)

instance NFData CompileError where
  rnf (ScheduleViolation s) = rnf s
  rnf (ContractionUnsafe s)  = rnf s


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
compile sys sched _alloc = runIslT $ Isl.do
  let (domains, writes, reads_, projections) = lowerSystem sys
      schedMaps = lowerScheduleMaps sched domains
  Ur eqReads <- projectBodyReads reads_ projections
  Ur flowDeps <- computeAllDeps eqReads writes
  Ur schedUMs <- Isl.mapM buildUnionMapFromNamed schedMaps
  case schedUMs of
    [] -> Isl.pure (Ur (Right ()))
    (first':rest') -> Isl.do
      schedUM <- Isl.foldM (\acc x -> UM.union acc x) first' rest'
      Ur result <- validateDeps flowDeps schedUM (schedNOut schedMaps) "flow"
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

-- | Extract the output dimensionality from schedule maps.
schedNOut :: [NamedMap] -> Int
schedNOut (sm:_) = nmNOut sm
schedNOut []     = 0


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Body-space read projection
-- ═══════════════════════════════════════════════════════════════════════

projectBodyReads
  :: [NamedMap] -> [NamedMap] -> IslT IO (Ur [Isl.UnionMap])
projectBodyReads reads_ [] = Isl.mapM buildUnionMapFromNamed reads_
projectBodyReads reads_ projections = Isl.do
  let bodyNames = [ n | NamedMap { nmDomainName = Just n } <- projections ]
      (bodyReads, eqReads) = partitionBy
        (\r -> maybe False (`elem` bodyNames) (nmDomainName r)) reads_
  Ur eqReadUMs <- Isl.mapM buildUnionMapFromNamed eqReads
  if null bodyReads
    then Isl.pure (Ur eqReadUMs)
    else Isl.do
      Ur projUMs <- Isl.mapM buildUnionMapFromNamed projections
      projUM <- case projUMs of { (h:t) -> Isl.foldM (\acc x -> UM.union acc x) h t; [] -> error "unreachable" }
      Ur bodyReadUMs <- Isl.mapM buildUnionMapFromNamed bodyReads
      bodyReadUM <- case bodyReadUMs of { (h:t) -> Isl.foldM (\acc x -> UM.union acc x) h t; [] -> error "unreachable" }
      composedUM <- UM.applyDomain bodyReadUM projUM
      Ur c <- urWrap composedUM
      Isl.pure (Ur (c : eqReadUMs))

partitionBy :: (a -> Bool) -> [a] -> ([a], [a])
partitionBy _ [] = ([], [])
partitionBy f (x:xs)
  | f x       = let (ys, ns) = partitionBy f xs in (x:ys, ns)
  | otherwise  = let (ys, ns) = partitionBy f xs in (ys, x:ns)


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Schedule lowering
-- ═══════════════════════════════════════════════════════════════════════

lowerScheduleMaps :: Schedule -> [NamedSet] -> [NamedMap]
lowerScheduleMaps (Schedule entries) domains =
  [ S.schedToNamedMap' name dom (esDef eq)
  | NamedSet { nsName = Just name } <- domains
  , Just eq <- [Map.lookup name entries]
  , dom:_ <- [[ d | d@(NamedSet { nsName = Just n }) <- domains, n == name ]]
  ]


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Dependence computation
-- ═══════════════════════════════════════════════════════════════════════

computeAllDeps :: [Isl.UnionMap] -> [NamedMap] -> IslT IO (Ur [Isl.UnionMap])
computeAllDeps readUMs allWrites =
  if null readUMs || null allWrites
    then Isl.do
      freeAll readUMs
      Isl.pure (Ur [])
    else Isl.do
      Ur writeUMs <- Isl.mapM buildUnionMapFromNamed allWrites
      readUM  <- case readUMs of { (h:t) -> Isl.foldM (\acc x -> UM.union acc x) h t; [] -> error "unreachable" }
      writeUM <- case writeUMs of { (h:t) -> Isl.foldM (\acc x -> UM.union acc x) h t; [] -> error "unreachable" }
      let !(writeUM1, writeUM2) = dup writeUM
      writeInv <- UM.reverse writeUM1
      deps <- UM.applyRange readUM writeInv
      depsWR <- UM.reverse deps
      freeM writeUM2
      Ur d <- urWrap depsWR
      Isl.pure (Ur [d])


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Lex-positivity check
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

validateDeps :: [Isl.UnionMap] -> Isl.UnionMap %1 -> Int -> String
             -> IslT IO (Ur (Either CompileError ()))
validateDeps = unsafeCoerce go where
  go :: [Isl.UnionMap] -> Isl.UnionMap -> Int -> String
     -> IslT IO (Ur (Either CompileError ()))
  go [] sched _nOut _label = Isl.do
    freeM sched
    Isl.pure (Ur (Right ()))
  go deps sched nOut label = Isl.do
    allDeps <- case deps of
      (d:ds) -> Isl.foldM (\acc x -> UM.union acc x) d ds
      []     -> error "validateDeps: unreachable"
    let (sched1, sched2) = dup sched
    Ur result <- checkLexPositivity allDeps sched1 sched2 nOut
    case result of
      Nothing  -> Isl.pure (Ur (Right ()))
      Just vio -> Isl.pure (Ur (Left (ScheduleViolation (label <> " deps violated: " <> vio))))


-- ═══════════════════════════════════════════════════════════════════════
-- §7. Helpers
-- ═══════════════════════════════════════════════════════════════════════

freeAll :: [Isl.UnionMap] -> IslT IO ()
freeAll [] = Isl.pure ()
freeAll (x:xs) = Isl.do
  freeM x
  freeAll xs

