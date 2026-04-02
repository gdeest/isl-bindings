{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Stencil definition with phantom-indexed dimensionality.
--
-- A stencil is a named statement over a parametric iteration domain
-- with a set of read offset vectors into a virtual array.
-- From this, read/write access relations and the iteration domain
-- are derived as typed 'NamedMap' / 'NamedSet' objects — no ISL strings.
--
-- @
-- jacobi2d :: StencilDef '["M","N","T"] 3
-- jacobi2d = StencilDef
--   { sdName = "S", sdArray = "A"
--   , sdOffsets = [[-1,-1,0], [-1,1,0], [-1,0,-1], [-1,0,1]]
--   }
-- @
module Isl.HighLevel.Stencil
  ( StencilDef(..)
  , mkReadAccess
  , mkWriteAccess
  , mkDomain
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, KnownNat, natVal, Symbol)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Params (KnownSymbols(symbolVals), Length)
import Isl.HighLevel.Pure


-- | A stencil computation with phantom-indexed parameter space and dimensionality.
--
-- @ps@ — parameter names (e.g., @'["M","N","T"]@), alphabetically sorted.
-- @n@  — number of iteration dimensions (e.g., 3 for @[t,i,j]@).
--
-- The iteration domain is @{ S[d0,..,d_{n-1}] : 1 <= d_k <= param_k }@.
-- Each offset vector has length @n@ and describes a read from the virtual
-- array: @S[d0,..,d_{n-1}]@ reads @A[d0+off_0, .., d_{n-1}+off_{n-1}]@.
data StencilDef (ps :: [Symbol]) (n :: Nat) = StencilDef
  { sdName    :: !String     -- ^ Statement name (e.g., "S")
  , sdArray   :: !String     -- ^ Virtual array name (e.g., "A")
  , sdOffsets :: ![[Int]]    -- ^ Read offset vectors (each length @n@, checked at use site)
  }


-- | Build read access relations: one 'NamedMap' per offset vector.
--
-- For offset @[dt, di, dj]@:
-- @{ S[t,i,j] → A[t+dt, i+di, j+dj] : domain constraints }@
mkReadAccess :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => StencilDef ps n -> [NamedMap]
mkReadAccess sd =
  [ mkOneAccess sd off | off <- sdOffsets sd ]

-- | Build the write access relation.
--
-- @{ S[t,i,j] → A[t, i, j] : domain constraints }@
mkWriteAccess :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => StencilDef ps n -> NamedMap
mkWriteAccess sd = mkOneAccess sd (replicate nDims 0)
  where nDims = fromIntegral (natVal (Proxy @n))

-- | Build the iteration domain.
--
-- @{ S[d0,..,d_{n-1}] : 1 <= d_k <= param_k }@
mkDomain :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => StencilDef ps n -> NamedSet
mkDomain sd =
  let nDims   = fromIntegral (natVal (Proxy @n))
      nParams = fromIntegral (natVal (Proxy @(Length ps)))
      conj = buildDomainConj nDims nParams
  in NamedSet
    { nsName   = Just (sdName sd)
    , nsParams = symbolVals @ps
    , nsNDims  = nDims
    , nsConjs  = [conj]
    }


-- | Build a single access relation for one offset vector.
-- @{ S[d0,..,d_{n-1}] → A[d0+off_0, .., d_{n-1}+off_{n-1}] : domain }@
mkOneAccess :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => StencilDef ps n -> [Int] -> NamedMap
mkOneAccess sd offsets =
  let nDims   = fromIntegral (natVal (Proxy @n))
      nParams = fromIntegral (natVal (Proxy @(Length ps)))
      -- Build access constraints: out_k = in_k + offset_k
      accessConstrs = [ EqualityConstraint
                           (Add (Ix (OutDim k)) (Mul (-1) (Add (Ix (InDim k)) (Constant (fromIntegral (offsets !! k))))))
                       | k <- [0..nDims-1] ]
      -- Domain constraints: 1 <= in_k <= param_k
      domConstrs = domainConstrs nDims nParams
  in NamedMap
    { nmDomainName = Just (sdName sd)
    , nmRangeName  = Just (sdArray sd)
    , nmParams     = symbolVals @ps
    , nmNIn        = nDims
    , nmNOut       = nDims
    , nmConjs      = [Conjunction (accessConstrs ++ domConstrs)]
    }


-- | Domain constraints: @1 <= d_k <= param_k@ for each dimension.
domainConstrs :: Int -> Int -> [Constraint MapIx]
domainConstrs nDims nParams =
  concat [ [ InequalityConstraint (Add (Ix (InDim k)) (Constant (-1)))         -- d_k >= 1
           , InequalityConstraint (Add (Ix (MapParam k)) (Mul (-1) (Ix (InDim k))))  -- d_k <= param_k
           ]
         | k <- [0..nDims-1], k < nParams  -- pair each dim with corresponding param
         ]

-- | Build domain as a set conjunction: @1 <= d_k <= param_k@.
buildDomainConj :: Int -> Int -> Conjunction SetIx
buildDomainConj nDims nParams = Conjunction $
  concat [ [ InequalityConstraint (Add (Ix (SetDim k)) (Constant (-1)))          -- d_k >= 1
           , InequalityConstraint (Add (Ix (SetParam k)) (Mul (-1) (Ix (SetDim k))))  -- d_k <= param_k
           ]
         | k <- [0..nDims-1], k < nParams
         ]
