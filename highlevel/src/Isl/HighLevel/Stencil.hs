{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Stencil definition with phantom-indexed dimensionality.
--
-- A stencil is JUST the access pattern: statement name, array name, offsets.
-- The iteration domain is supplied separately — no hardcoded domain shape.
--
-- @
-- stencil :: StencilDef '["M","N","T"] 3
-- stencil = StencilDef "S" "A" [[-1,-1,0], [-1,1,0], [-1,0,-1], [-1,0,1]]
--
-- domain = rectangularDomain \@'["M","N","T"] \@3 "S"
-- reads  = mkReadAccess stencil (domainConstrs domain)
-- @
module Isl.HighLevel.Stencil
  ( StencilDef(..)
    -- * Access relation builders
  , mkReadAccess
  , mkWriteAccess
    -- * Domain helpers
  , rectangularDomain
  , mkNamedDomain
  , domainConstrs
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, KnownNat, natVal, Symbol)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Params (KnownSymbols(symbolVals), Length)
import Isl.HighLevel.Pure


-- | A stencil: statement name, virtual array name, and read offset vectors.
-- The domain shape is NOT part of the stencil — it's supplied separately.
data StencilDef (ps :: [Symbol]) (n :: Nat) = StencilDef
  { sdName    :: !String     -- ^ Statement name (e.g., "S")
  , sdArray   :: !String     -- ^ Virtual array name (e.g., "A")
  , sdOffsets :: ![[Int]]    -- ^ Read offset vectors (each length @n@)
  }


-- | Build read access relations, one per offset.
-- Domain constraints are supplied explicitly — no hardcoded domain shape.
mkReadAccess :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => StencilDef ps n -> [Constraint SetIx] -> [NamedMap]
mkReadAccess sd domConstrs =
  [ mkOneAccess sd off domConstrs | off <- sdOffsets sd ]

-- | Build the write access relation (identity: A[d0,..,d_{n-1}]).
mkWriteAccess :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => StencilDef ps n -> [Constraint SetIx] -> NamedMap
mkWriteAccess sd domConstrs =
  mkOneAccess sd (replicate nDims 0) domConstrs
  where nDims = fromIntegral (natVal (Proxy @n))


-- =========================================================================
-- Domain helpers (user-facing, not baked into StencilDef)
-- =========================================================================

-- | Rectangular domain: @{ name[d0,..,d_{n-1}] : 1 <= d_k <= param_k }@.
-- Common case helper — NOT the only option.
rectangularDomain :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => String -> NamedSet
rectangularDomain name =
  let nDims   = fromIntegral (natVal (Proxy @n))
      nParams = fromIntegral (natVal (Proxy @(Length ps)))
  in mkNamedDomain name (symbolVals @ps) nDims (rectConstrs nDims nParams)

-- | Fully general domain from explicit constraints.
mkNamedDomain :: String -> [String] -> Int -> [Constraint SetIx] -> NamedSet
mkNamedDomain name params nDims constrs = NamedSet
  { nsName   = Just name
  , nsParams = params
  , nsNDims  = nDims
  , nsConjs  = [Conjunction constrs]
  }

-- | Extract constraints from a NamedSet (first conjunct).
domainConstrs :: NamedSet -> [Constraint SetIx]
domainConstrs ns = case nsConjs ns of
  (Conjunction cs : _) -> cs
  []                   -> []


-- =========================================================================
-- Internal
-- =========================================================================

-- | Build one access relation with user-supplied domain constraints.
mkOneAccess :: forall ps n.
  (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => StencilDef ps n -> [Int] -> [Constraint SetIx] -> NamedMap
mkOneAccess sd offsets domConstrs =
  let nDims = fromIntegral (natVal (Proxy @n))
      -- Access: out_k = in_k + offset_k
      accessConstrs =
        [ EqualityConstraint
            (Add (Ix (OutDim k))
                 (Mul (-1) (Add (Ix (InDim k)) (Constant (fromIntegral (offsets !! k))))))
        | k <- [0..nDims-1]
        ]
      -- Domain constraints converted from SetIx to MapIx (InDim side)
      mapDomConstrs = map (mapConstraint setToMapIx) domConstrs
  in NamedMap
    { nmDomainName = Just (sdName sd)
    , nmRangeName  = Just (sdArray sd)
    , nmParams     = symbolVals @ps
    , nmNIn        = nDims
    , nmNOut       = nDims
    , nmConjs      = [Conjunction (accessConstrs ++ mapDomConstrs)]
    }

-- | Convert SetIx to MapIx (domain side): SetDim → InDim, SetParam → MapParam.
setToMapIx :: SetIx -> MapIx
setToMapIx (SetDim d)   = InDim d
setToMapIx (SetParam p) = MapParam p

-- | Map over the index type in a Constraint.
mapConstraint :: (a -> b) -> Constraint a -> Constraint b
mapConstraint f (EqualityConstraint e)   = EqualityConstraint (fmap f e)
mapConstraint f (InequalityConstraint e) = InequalityConstraint (fmap f e)

-- | Rectangular domain constraints: @1 <= d_k <= param_k@.
rectConstrs :: Int -> Int -> [Constraint SetIx]
rectConstrs nDims nParams =
  concat [ [ InequalityConstraint (Add (Ix (SetDim k)) (Constant (-1)))
           , InequalityConstraint (Add (Ix (SetParam k)) (Mul (-1) (Ix (SetDim k))))
           ]
         | k <- [0..min nDims nParams - 1]
         ]
