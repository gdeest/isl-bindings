{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Memory contraction: mapping virtual arrays to physical storage.
--
-- A 'StorageMap' defines how virtual array coordinates map to physical
-- buffer coordinates using quasi-affine expressions (supporting floor
-- division and modular arithmetic).
--
-- The contraction induces anti-dependencies that must be checked against
-- the schedule to ensure safety.
--
-- @
-- -- Modular time contraction: A[t,i,j] → buf[t mod 5, i, j]
-- storage = modularTime 3 5
--
-- -- Block-cyclic: A[t,i,j] → buf[floor(t/4), t mod 4, i, j]
-- storage = StorageMap [FloorDiv (Ix (SetDim 0)) 4, modExpr (Ix (SetDim 0)) 4, Ix (SetDim 1), Ix (SetDim 2)]
-- @
module Isl.HighLevel.Contraction
  ( -- * Storage mapping type
    StorageMap(..)
    -- * Common mappings
  , modularTime
  , identityStorage
    -- * Anti-dep generation
  , contractionAntiDeps
    -- * Conversion
  , storageToNamedMap
  ) where

import Isl.HighLevel.Constraints
  ( Expr(..), SetIx(..), MapIx(..), Constraint(..), Conjunction(..), modExpr )
import Isl.HighLevel.Pure (NamedMap(..))
import Isl.HighLevel.Params (KnownSymbols(symbolVals))
import Isl.HighLevel.Stencil (StencilDef(..))


-- | A storage mapping: each element is a quasi-affine expression
-- over the virtual array dimensions, defining one physical buffer coordinate.
--
-- Length = number of physical buffer dimensions.
-- Expressions reference 'SetDim k' for virtual array dim k.
newtype StorageMap = StorageMap { smExprs :: [Expr SetIx] }
  deriving (Show)


-- | Modular time contraction: @A[t, d1, .., d_{n-1}] → buf[t mod K, d1, .., d_{n-1}]@.
modularTime :: Int      -- ^ number of virtual array dims
            -> Integer  -- ^ K (number of buffers / modulus)
            -> StorageMap
modularTime nDims k = StorageMap $
  modExpr (Ix (SetDim 0)) k : [Ix (SetDim d) | d <- [1..nDims-1]]


-- | Identity storage: @A[d0,..,d_{n-1}] → buf[d0,..,d_{n-1}]@. No contraction.
identityStorage :: Int -> StorageMap
identityStorage nDims = StorageMap [Ix (SetDim d) | d <- [0..nDims-1]]


-- | Convert a storage map to a 'NamedMap' for ISL operations.
-- @{ arrayName[d0,..,d_{n-1}] → bufName[expr_0, .., expr_{m-1}] }@
storageToNamedMap :: forall ps. KnownSymbols ps
  => String       -- ^ array name (domain tuple)
  -> String       -- ^ buffer name (range tuple)
  -> StorageMap
  -> Int          -- ^ number of virtual array dims
  -> NamedMap
storageToNamedMap arrName bufName (StorageMap outExprs) nIn =
  let nOut = length outExprs
      params = symbolVals @ps
      -- Constraints: out_k = expr_k(in_dims)
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


-- | Build contraction-induced anti-deps as NamedMaps.
--
-- For each stencil read offset and each "conflicting write" (same physical
-- location, different virtual location), generates:
-- @{ S[t, i, j] → S[t + K + dt, i + di, j + dj] }@
-- meaning "the reader must execute before the next write to the same buf slot".
--
-- For modular contraction @A[t,..] → buf[t mod K,..]@ with stencil offset @(dt,..)@:
-- the anti-dep is @{ S[t,i,j] → S[t + K + dt, i + di, j + dj] }@
-- (the reader at offset (dt,di,dj) must finish before the write K steps later).
contractionAntiDeps :: forall ps n. KnownSymbols ps
  => StencilDef ps n
  -> Integer         -- ^ K (modulus for time dimension)
  -> [NamedMap]
contractionAntiDeps sd k =
  [ mkAntiDep off | off <- sdOffsets sd ]
  where
    params = symbolVals @ps
    nDims = length (head (sdOffsets sd))  -- all offsets have same length

    mkAntiDep :: [Int] -> NamedMap
    mkAntiDep offsets =
      let -- Anti-dep: { S[dims] → S[dims + K_offset] }
          -- where K_offset = (K + dt, di, dj, ...)
          -- The anti-dep distance in time dim is K + dt (next conflict after reader)
          kOffsets = (fromIntegral k + fromIntegral (head offsets))
                   : map fromIntegral (tail offsets)
          -- Access constraints: out_d = in_d + kOffset_d
          accessConstrs =
            [ EqualityConstraint (Add (Ix (OutDim d))
                (Mul (-1) (Add (Ix (InDim d)) (Constant (kOffsets !! d)))))
            | d <- [0..nDims-1]
            ]
          -- Domain + range bounds: 1 <= dim <= param
          domConstrs = concat
            [ [ InequalityConstraint (Add (Ix (InDim d)) (Constant (-1)))
              , InequalityConstraint (Add (Ix (MapParam d)) (Mul (-1) (Ix (InDim d))))
              , InequalityConstraint (Add (Ix (OutDim d)) (Constant (-1)))
              , InequalityConstraint (Add (Ix (MapParam d)) (Mul (-1) (Ix (OutDim d))))
              ]
            | d <- [0..nDims-1], d < length params
            ]
      in NamedMap
        { nmDomainName = Just (sdName sd)
        , nmRangeName  = Just (sdName sd)
        , nmParams     = params
        , nmNIn        = nDims
        , nmNOut       = nDims
        , nmConjs      = [Conjunction (accessConstrs ++ domConstrs)]
        }


-- | Convert a SetIx expression to MapIx (SetDim → InDim, SetParam → MapParam).
mapifySetExpr :: Expr SetIx -> Expr MapIx
mapifySetExpr (Ix (SetDim d))   = Ix (InDim d)
mapifySetExpr (Ix (SetParam p)) = Ix (MapParam p)
mapifySetExpr (Constant n)      = Constant n
mapifySetExpr (Add a b)         = Add (mapifySetExpr a) (mapifySetExpr b)
mapifySetExpr (Mul k e)         = Mul k (mapifySetExpr e)
mapifySetExpr (FloorDiv e d)    = FloorDiv (mapifySetExpr e) d
