{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Dependence computation for Alpha systems.
--
-- Factored out of 'Alpha.Compile' so both schedule validation and
-- parallel annotation checking can reuse it.
module Alpha.Polyhedral.Dependence
  ( -- * Schedule lowering
    lowerScheduleMaps
    -- * Body-space read projection
  , projectBodyReads
    -- * Dependence computation
  , computeAllDeps
    -- * Helpers
  , freeAll
  , buildUnionFromNamed
  ) where

import Data.List (partition)
import qualified Data.Map.Strict as Map

import Isl.Typed.Constraints
  ( NamedSet(..), NamedMap(..), buildUnionMapFromNamed )
import qualified Isl.Types as Isl
import qualified Isl.UnionMap as UM
import Isl.Monad (IslT, Ur(..))
import Isl.Linear (freeM, dup, urWrap)
import qualified Isl.Linear as Isl
import qualified Alpha.Polyhedral.Schedule as S
import Alpha.Schedule (Schedule(..), EqSchedule(esDef))


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Schedule lowering
-- ═══════════════════════════════════════════════════════════════════════

lowerScheduleMaps :: Schedule -> [NamedSet] -> [NamedMap]
lowerScheduleMaps (Schedule entries) domains =
  [ S.schedToNamedMap' name dom (esDef eq)
  | NamedSet { nsName = Just name } <- domains
  , Just eq <- [Map.lookup name entries]
  , dom:_ <- [[ d | d@(NamedSet { nsName = Just n }) <- domains, n == name ]]
  ]


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Body-space read projection
-- ═══════════════════════════════════════════════════════════════════════

projectBodyReads
  :: [NamedMap] -> [NamedMap] -> IslT IO (Ur [Isl.UnionMap])
projectBodyReads reads_ [] = Isl.mapM buildUnionMapFromNamed reads_
projectBodyReads reads_ projections = Isl.do
  let bodyNames = [ n | NamedMap { nmDomainName = Just n } <- projections ]
      (bodyReads, eqReads) = partition
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

-- ═══════════════════════════════════════════════════════════════════════
-- §3. Dependence computation
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
-- §4. Helpers
-- ═══════════════════════════════════════════════════════════════════════

freeAll :: [Isl.UnionMap] -> IslT IO ()
freeAll [] = Isl.pure ()
freeAll (x:xs) = Isl.do
  freeM x
  freeAll xs

buildUnionFromNamed :: [NamedMap] -> IslT IO Isl.UnionMap
buildUnionFromNamed nms = Isl.do
  Ur ums <- Isl.mapM buildUnionMapFromNamed nms
  case ums of
    []     -> error "buildUnionFromNamed: empty list"
    (h:t)  -> Isl.foldM (\acc x -> UM.union acc x) h t
