{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Memory contraction: mapping virtual arrays to physical storage.
--
-- A 'StorageMap' defines how virtual array coordinates map to physical
-- buffer coordinates using quasi-affine expressions. The contraction
-- induces anti-dependencies that are derived automatically via ISL
-- composition — fully general, no hardcoded modulus or dimension.
--
-- @
-- storage = modularTime 3 5  -- A[t,i,j] → buf[t mod 5, i, j]
-- antiDeps <- contractionAntiDeps flowDeps writeUM storageUM
-- @
module Alpha.Polyhedral.Contraction
  ( -- * Storage mapping type
    StorageMap(..)
    -- * Common mappings
  , modularTime
  , identityStorage
    -- * Anti-dep derivation (fully general, ISL-based)
  , contractionAntiDeps
    -- * Output-dep (WAW) derivation for post-contraction aliasing
  , contractionOutputDeps
    -- * Conversion
  , storageToNamedMap
  , storageToNamedMap'
  ) where

import Control.Monad.IO.Class (MonadIO)

import Isl.Typed.Constraints
  ( Expr(..), SetIx(..), MapIx(..), Constraint(..), Conjunction(..), modExpr )
import Isl.Typed.Constraints (NamedMap(..))
import Isl.Typed.Params (KnownSymbols(symbolVals))
import qualified Isl.UnionMap as UM
import qualified Isl.UnionSet as US
import Isl.Monad (IslT)
import qualified Isl.Linear as Isl
import qualified Isl.Types as Isl


-- | A storage mapping: each element is a quasi-affine expression
-- over virtual array dimensions, defining one physical buffer coordinate.
newtype StorageMap = StorageMap { smExprs :: [Expr SetIx] }
  deriving (Show)


-- | Modular time contraction: @A[t, d1, .., d_{n-1}] → buf[t mod K, d1, .., d_{n-1}]@.
modularTime :: Int -> Integer -> StorageMap
modularTime nDims k = StorageMap $
  modExpr (Ix (SetDim 0)) k : [Ix (SetDim d) | d <- [1..nDims-1]]


-- | Identity storage (no contraction).
identityStorage :: Int -> StorageMap
identityStorage nDims = StorageMap [Ix (SetDim d) | d <- [0..nDims-1]]


-- | Derive contraction-induced anti-deps via ISL composition.
--
-- Fully general — works for ANY quasi-affine storage map.
-- No hardcoded K, dimension, or modulus.
--
-- Algorithm:
--   contractedWrite = storage ∘ write       { S[a] → buf[f(write(a))] }
--   conflict = cw⁻¹ ∘ cw                   { S[a] → S[c] : same buf }
--   antiDeps = flowDeps⁻¹ ∘ conflict       { S[reader] → S[conflicting writer] }
contractionAntiDeps :: MonadIO m
  => Isl.UnionMap    -- ^ flow deps (consumed)
  -> Isl.UnionMap    -- ^ write access (consumed)
  -> Isl.UnionMap    -- ^ write access copy (consumed)
  -> Isl.UnionMap    -- ^ storage map (consumed)
  -> Isl.UnionMap    -- ^ storage map copy (consumed)
  -> IslT m Isl.UnionMap
contractionAntiDeps flowDeps writeAccess writeAccessCopy storageMap storageMapCopy = Isl.do
  cw <- UM.applyRange writeAccess storageMap
  cwRev <- UM.reverse cw
  cw2 <- UM.applyRange writeAccessCopy storageMapCopy
  conflict <- UM.applyRange cwRev cw2
  flowRev <- UM.reverse flowDeps
  UM.applyRange flowRev conflict


-- | Derive post-contraction output (WAW) dependencies.
--
-- After allocation collapses distinct virtual writes into the same
-- physical buffer cell, two equation iterations that originally wrote
-- disjoint virtual coordinates can now alias.  The schedule is only
-- sound if such aliasing pairs are distinguished by their schedule
-- time (no two aliasing writes mapped to the same schedule point).
--
-- The returned union map is the symmetric aliasing relation on the
-- virtual iteration space, minus the diagonal:
--
-- @
--   cw  = write ∘ storage                { S[a] → buf[f(a)] }
--   waw = (cw ∘ cw⁻¹) \\ id(domain(cw))
--       = { S[a] → S[c] | f(a) = f(c), a ≠ c }
-- @
--
-- Pre-contraction (identity storage) @cw ∘ cw⁻¹@ degenerates to the
-- diagonal and @waw@ is empty — so callers can safely skip the check
-- when no equation has contracted storage.
--
-- The caller is expected to push the returned relation through the
-- schedule and verify that no aliasing pair lands on the same schedule
-- point (race-freeness).  Because @waw@ is symmetric a strict
-- lex-positivity test would be degenerate — see
-- 'Alpha.Compile.checkContractionRaceFree' for the diagonal-of-lex
-- test that is actually used.
contractionOutputDeps :: MonadIO m
  => Isl.UnionMap %1  -- ^ write access (consumed)
  -> Isl.UnionMap %1  -- ^ write access copy (consumed)
  -> Isl.UnionMap %1  -- ^ storage map (consumed)
  -> Isl.UnionMap %1  -- ^ storage map copy (consumed)
  -> IslT m Isl.UnionMap
contractionOutputDeps writeAccess writeAccessCopy storageMap storageMapCopy = Isl.do
  -- cw   : { S[a] → buf[f(a)] }  where S = writeAccess.range, buf = storage.range
  -- waw  : { S[a] → S[c] : f(a) = f(c) } = cw ∘ cw⁻¹
  --        = applyRange cw (reverse cwCopy)
  cw      <- UM.applyRange writeAccess     storageMap
  cwCopy  <- UM.applyRange writeAccessCopy storageMapCopy
  cwCopyRev <- UM.reverse cwCopy
  wawSym  <- UM.applyRange cw cwCopyRev
  -- Remove the diagonal { S[a] → S[a] } so only genuine collisions remain.
  let !(wawA, wawB) = Isl.dup wawSym
  dom     <- UM.domain wawA
  diag    <- US.identity dom
  UM.subtract wawB diag


-- | Convert a storage map to a 'NamedMap'.
-- @{ arrayName[d0,..,d_{n-1}] → bufName[expr_0, .., expr_{m-1}] }@
storageToNamedMap :: forall ps. KnownSymbols ps
  => String -> String -> StorageMap -> Int -> NamedMap
storageToNamedMap arrName bufName stor nIn =
  storageToNamedMap' arrName bufName (symbolVals @ps) stor nIn

-- | Like 'storageToNamedMap' but takes params as a value instead of from a typeclass.
storageToNamedMap' :: String -> String -> [String] -> StorageMap -> Int -> NamedMap
storageToNamedMap' arrName bufName params (StorageMap outExprs) nIn =
  let nOut = length outExprs
      constrs =
        [ EqualityConstraint (Add (Ix (OutDim k)) (Mul (-1) (mapifySetExpr (outExprs !! k))))
        | k <- [0..nOut-1]
        ]
  in NamedMap
    { nmDomainName = Just arrName
    , nmRangeName  = Just bufName
    , nmParams     = params
    , nmNIn        = nIn
    , nmNOut       = nOut
    , nmConjs      = [Conjunction constrs]
    }


-- | Convert SetIx → MapIx (SetDim → InDim, SetParam → MapParam).
mapifySetExpr :: Expr SetIx -> Expr MapIx
mapifySetExpr (Ix (SetDim d))   = Ix (InDim d)
mapifySetExpr (Ix (SetParam p)) = Ix (MapParam p)
mapifySetExpr (Constant n)      = Constant n
mapifySetExpr (Add a b)         = Add (mapifySetExpr a) (mapifySetExpr b)
mapifySetExpr (Mul k e)         = Mul k (mapifySetExpr e)
mapifySetExpr (FloorDiv e d)    = FloorDiv (mapifySetExpr e) d
