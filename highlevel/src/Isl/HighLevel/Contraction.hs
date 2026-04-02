{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
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
module Isl.HighLevel.Contraction
  ( -- * Storage mapping type
    StorageMap(..)
    -- * Common mappings
  , modularTime
  , identityStorage
    -- * Anti-dep derivation (fully general, ISL-based)
  , contractionAntiDeps
    -- * Conversion
  , storageToNamedMap
  ) where

import Control.Monad.IO.Class (MonadIO)

import Isl.HighLevel.Constraints
  ( Expr(..), SetIx(..), MapIx(..), Constraint(..), Conjunction(..), modExpr )
import Isl.HighLevel.Pure (NamedMap(..))
import Isl.HighLevel.Params (KnownSymbols(symbolVals))
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.UnionMap as RawUM
import Isl.HighLevel.Context (IslT)


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
  => UM.UnionMap    -- ^ flow deps: { S[a] → S[b] } (consumed)
  -> UM.UnionMap    -- ^ write access: { S[a] → A[write(a)] } (consumed)
  -> UM.UnionMap    -- ^ write access copy (consumed)
  -> UM.UnionMap    -- ^ storage map: { A[dims] → buf[f(dims)] } (consumed)
  -> UM.UnionMap    -- ^ storage map copy (consumed)
  -> IslT m UM.UnionMap  -- ^ anti-deps: { S[reader] → S[conflicting_writer] }
contractionAntiDeps flowDeps writeAccess writeAccessCopy storageMap storageMapCopy = do
  -- { S[a] → buf[f(write(a))] }
  cw <- umApplyRange writeAccess storageMap
  -- { buf[...] → S[a] }
  cwRev <- umReverse cw
  -- { S[a] → buf[f(write(a))] } (second copy)
  cw2 <- umApplyRange writeAccessCopy storageMapCopy
  -- { S[a] → S[c] : f(write(a)) = f(write(c)) } (conflict)
  conflict <- umApplyRange cwRev cw2
  -- { S[b] → S[a] } (reverse flow deps)
  flowRev <- umReverse flowDeps
  -- { S[b] → S[c] : exists a. flowDep(a,b) and conflict(a,c) }
  umApplyRange flowRev conflict


-- | Convert a storage map to a 'NamedMap'.
-- @{ arrayName[d0,..,d_{n-1}] → bufName[expr_0, .., expr_{m-1}] }@
storageToNamedMap :: forall ps. KnownSymbols ps
  => String -> String -> StorageMap -> Int -> NamedMap
storageToNamedMap arrName bufName (StorageMap outExprs) nIn =
  let nOut = length outExprs
      constrs =
        [ EqualityConstraint (Add (Ix (OutDim k)) (Mul (-1) (mapifySetExpr (outExprs !! k))))
        | k <- [0..nOut-1]
        ]
  in NamedMap
    { nmDomainName = Just arrName
    , nmRangeName  = Just bufName
    , nmParams     = symbolVals @ps
    , nmNIn        = nIn
    , nmNOut       = nOut
    , nmConjs      = [Conjunction constrs]
    }


-- Internal: raw union map operations (bypass linear types for regular do)

umApplyRange :: MonadIO m => UM.UnionMap -> UM.UnionMap -> IslT m UM.UnionMap
umApplyRange (UM.UnionMap a) (UM.UnionMap b) = UM.UnionMap <$> RawUM.applyRange a b

umReverse :: MonadIO m => UM.UnionMap -> IslT m UM.UnionMap
umReverse (UM.UnionMap a) = UM.UnionMap <$> RawUM.reverse a


-- | Convert SetIx → MapIx (SetDim → InDim, SetParam → MapParam).
mapifySetExpr :: Expr SetIx -> Expr MapIx
mapifySetExpr (Ix (SetDim d))   = Ix (InDim d)
mapifySetExpr (Ix (SetParam p)) = Ix (MapParam p)
mapifySetExpr (Constant n)      = Constant n
mapifySetExpr (Add a b)         = Add (mapifySetExpr a) (mapifySetExpr b)
mapifySetExpr (Mul k e)         = Mul k (mapifySetExpr e)
mapifySetExpr (FloorDiv e d)    = FloorDiv (mapifySetExpr e) d
