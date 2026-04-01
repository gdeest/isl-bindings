{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Phantom-indexed pure Haskell representations of ISL objects.
-- These types carry parameter names at the type level (@ps@) and
-- dimension counts (@n@), ensuring you cannot accidentally mix
-- representations of different parameter spaces or dimensionality.
--
-- Use 'decomposeBS', 'decomposeSet', etc. to convert from ISL objects
-- to these pure types, and 'mkBasicSet', 'toBasicSet' etc. to go back.
--
-- Use 'mkPConjunction' and 'mkPMapConjunction' to build pure constraint
-- representations using the same DSL as 'mkBasicSet' / 'mkBasicMap',
-- without allocating ISL objects.
module Isl.HighLevel.Pure
  ( -- * Set-like pure representations
    PConjunction(..)
  , PDisjunction(..)
    -- * Map-like pure representations
  , PMapConjunction(..)
  , PMapDisjunction(..)
    -- * Multi-aff / piecewise pure representations
  , PMultiAff(..)
  , PPwAff(..)
  , PPwMultiAff(..)
    -- * DSL builders
  , mkPConjunction
  , mkPDisjunction
  , mkPMapConjunction
    -- * Existential wrappers (for union types with unknown dimensionality)
  , SomeDisjunction(..)
  , SomeMapDisjunction(..)
    -- * Named (value-level) representations for union decomposition
  , NamedSet(..)
  , NamedMap(..)
    -- * Named DSL builders (pure, no ISL)
  , mkNamedPConjunction
  , mkNamedPMapConjunction
  ) where

import Control.DeepSeq (NFData(..))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, KnownNat, natVal, Symbol, KnownSymbol, symbolVal)

import Isl.HighLevel.Constraints (Conjunction, Expr, SetIx(..), MapIx(..))
import Isl.HighLevel.Indices (IxList, coerceIxList, mkIxListWith)
import Isl.HighLevel.Params (KnownSymbols(..), Length)

-- | A single convex polyhedron (conjunction of constraints) with
-- parameter names @ps@ and @n@ set dimensions. Pure Haskell — no ISL pointers.
newtype PConjunction (ps :: [Symbol]) (n :: Nat) = PConjunction (Conjunction SetIx)

deriving instance Show (PConjunction ps n)
deriving instance Eq (PConjunction ps n)

instance NFData (PConjunction ps n) where
  rnf (PConjunction c) = rnf c

-- | A union of convex polyhedra (disjunction of conjunctions) with
-- parameter names @ps@ and @n@ set dimensions.
-- Represents the same thing as an ISL 'Set'.
newtype PDisjunction (ps :: [Symbol]) (n :: Nat) = PDisjunction [PConjunction ps n]

deriving instance Show (PDisjunction ps n)
deriving instance Eq (PDisjunction ps n)

instance NFData (PDisjunction ps n) where
  rnf (PDisjunction cs) = rnf cs

-- | A single convex polyhedron in map space with parameter names @ps@,
-- @nIn@ input dimensions and @nOut@ output dimensions.
-- Constraints use 'MapIx' to distinguish input ('InDim'), output ('OutDim'),
-- and parameter ('MapParam') variables.
newtype PMapConjunction (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = PMapConjunction (Conjunction MapIx)

deriving instance Show (PMapConjunction ps nIn nOut)
deriving instance Eq (PMapConjunction ps nIn nOut)

-- | A union of map polyhedra with parameter names @ps@,
-- @nIn@ input and @nOut@ output dimensions.
-- Represents the same thing as an ISL 'Map'.
newtype PMapDisjunction (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = PMapDisjunction [PMapConjunction ps nIn nOut]

deriving instance Show (PMapDisjunction ps nIn nOut)
deriving instance Eq (PMapDisjunction ps nIn nOut)

-- | Pure multi-aff: list of output expressions over input dims/params.
newtype PMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = PMultiAff [Expr SetIx]

deriving instance Show (PMultiAff ps ni no)
deriving instance Eq (PMultiAff ps ni no)

-- | Pure piecewise aff: list of (domain constraints, expression) pieces.
newtype PPwAff (ps :: [Symbol]) (n :: Nat) = PPwAff [(Conjunction SetIx, Expr SetIx)]

deriving instance Show (PPwAff ps n)
deriving instance Eq (PPwAff ps n)

-- | Pure piecewise multi-aff: list of (domain constraints, output expressions) pieces.
newtype PPwMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = PPwMultiAff [(Conjunction SetIx, [Expr SetIx])]

deriving instance Show (PPwMultiAff ps ni no)
deriving instance Eq (PPwMultiAff ps ni no)

-- | Existentially-quantified disjunction. Used when deconstructing
-- union types where the dimension count and parameters are not statically known.
data SomeDisjunction = forall ps n. (KnownSymbols ps, KnownNat n) => SomeDisjunction (PDisjunction ps n)

instance Show SomeDisjunction where
  show (SomeDisjunction d) = show d

-- | Existentially-quantified map disjunction.
data SomeMapDisjunction = forall ps ni no. (KnownSymbols ps, KnownNat ni, KnownNat no) => SomeMapDisjunction (PMapDisjunction ps ni no)

instance Show SomeMapDisjunction where
  show (SomeMapDisjunction d) = show d

-- DSL builders

-- | Build a 'PConjunction' using the same lambda DSL as 'mkBasicSet',
-- but purely — no ISL allocation, no 'IslT' monad needed.
--
-- @
-- domain :: PConjunction '["N"] 2
-- domain = mkPConjunction \@'["N"] \@2 $
--   \\(n :- Nil) (x :- y :- Nil) ->
--     idx x >=: cst 0 &&: idx x <=: idx n -: cst 1
--     &&: idx y >=: cst 0 &&: idx y <=: idx x
-- @
mkPConjunction
  :: forall ps (n :: Nat). (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => (IxList (Length ps) SetIx -> IxList n SetIx -> Conjunction SetIx)
  -> PConjunction ps n
mkPConjunction mkConstraints = PConjunction conj
  where
    nParams = natVal (Proxy @(Length ps))
    nDims = natVal (Proxy @n)
    paramList = coerceIxList $ mkIxListWith SetParam 0 nParams
    dimList = coerceIxList $ mkIxListWith SetDim 0 nDims
    conj = mkConstraints paramList dimList

-- | Build a 'PDisjunction' from a list of constraint-building lambdas.
mkPDisjunction
  :: forall ps (n :: Nat). (KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => [IxList (Length ps) SetIx -> IxList n SetIx -> Conjunction SetIx]
  -> PDisjunction ps n
mkPDisjunction fns = PDisjunction (map (mkPConjunction @ps @n) fns)

-- | Build a 'PMapConjunction' using the same lambda DSL as 'mkBasicMap'.
mkPMapConjunction
  :: forall ps (ni :: Nat) (no :: Nat).
     (KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => (IxList (Length ps) MapIx -> IxList ni MapIx -> IxList no MapIx -> Conjunction MapIx)
  -> PMapConjunction ps ni no
mkPMapConjunction mkConstraints = PMapConjunction conj
  where
    nParams = natVal (Proxy @(Length ps))
    paramList = coerceIxList $ mkIxListWith MapParam 0 nParams
    inList  = coerceIxList $ mkIxListWith InDim 0 (natVal (Proxy @ni))
    outList = coerceIxList $ mkIxListWith OutDim 0 (natVal (Proxy @no))
    conj = mkConstraints paramList inList outList

-- * Named (value-level) representations

-- | A set decomposed from a 'UnionSet', with its tuple name and parameter
-- names preserved as value-level data. Used for multi-statement programs
-- where the tuple name identifies the statement.
data NamedSet = NamedSet
  { nsName   :: !(Maybe String)     -- ^ Tuple name (statement ID), e.g. @Just "S0"@
  , nsParams :: ![String]           -- ^ Parameter names from the ISL space
  , nsNDims  :: !Int                -- ^ Number of set dimensions
  , nsConjs  :: ![Conjunction SetIx] -- ^ Disjuncts (basic sets)
  } deriving (Show, Eq, Generic)

instance NFData NamedSet

-- | A map decomposed from a 'UnionMap', with domain tuple name and parameter
-- names preserved. Used for extracting per-statement schedule inverses.
data NamedMap = NamedMap
  { nmDomainName :: !(Maybe String)  -- ^ Domain tuple name (statement ID)
  , nmRangeName  :: !(Maybe String)  -- ^ Range tuple name
  , nmParams     :: ![String]        -- ^ Parameter names from the ISL space
  , nmNIn        :: !Int             -- ^ Number of input (domain) dimensions
  , nmNOut       :: !Int             -- ^ Number of output (range) dimensions
  , nmConjs      :: ![Conjunction MapIx] -- ^ Disjuncts (basic maps)
  } deriving (Show, Eq, Generic)

instance NFData NamedMap

-- * Named DSL builders (pure)

-- | Build a named 'NamedSet' using the same lambda DSL as 'mkPConjunction'.
-- The type-level @name@ becomes the tuple name (statement ID).
--
-- @
-- s0dom = mkNamedPConjunction \@"S0" \@'["N"] \@2 $
--   \\(n :- Nil) (i :- j :- Nil) ->
--     idx i >=: cst 0 &&: idx i <=: idx n -: cst 1
--     &&: idx j >=: cst 0 &&: idx j <=: idx i
-- @
mkNamedPConjunction
  :: forall (name :: Symbol) ps (n :: Nat).
     (KnownSymbol name, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => (IxList (Length ps) SetIx -> IxList n SetIx -> Conjunction SetIx)
  -> NamedSet
mkNamedPConjunction mkConstraints = NamedSet
  { nsName   = Just (symbolVal (Proxy @name))
  , nsParams = symbolVals @ps
  , nsNDims  = fromIntegral (natVal (Proxy @n))
  , nsConjs  = [conj]
  }
  where
    nParams = natVal (Proxy @(Length ps))
    nDims = natVal (Proxy @n)
    paramList = coerceIxList $ mkIxListWith SetParam 0 nParams
    dimList = coerceIxList $ mkIxListWith SetDim 0 nDims
    conj = mkConstraints paramList dimList

-- | Build a named 'NamedMap' using the same lambda DSL as 'mkPMapConjunction'.
-- The type-level @name@ becomes the domain tuple name (statement ID).
--
-- @
-- s0sched = mkNamedPMapConjunction \@"S0" \@'["N"] \@2 \@3 $
--   \\_ (i :- j :- Nil) (t0 :- t1 :- t2 :- Nil) ->
--     idx t0 ==: idx i &&: idx t1 ==: idx j &&: idx t2 ==: cst 0
-- @
mkNamedPMapConjunction
  :: forall (name :: Symbol) ps (ni :: Nat) (no :: Nat).
     (KnownSymbol name, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => (IxList (Length ps) MapIx -> IxList ni MapIx -> IxList no MapIx -> Conjunction MapIx)
  -> NamedMap
mkNamedPMapConjunction mkConstraints = NamedMap
  { nmDomainName = Just (symbolVal (Proxy @name))
  , nmRangeName  = Nothing
  , nmParams     = symbolVals @ps
  , nmNIn        = fromIntegral (natVal (Proxy @ni))
  , nmNOut       = fromIntegral (natVal (Proxy @no))
  , nmConjs      = [conj]
  }
  where
    nParams = natVal (Proxy @(Length ps))
    paramList = coerceIxList $ mkIxListWith MapParam 0 nParams
    inList  = coerceIxList $ mkIxListWith InDim 0 (natVal (Proxy @ni))
    outList = coerceIxList $ mkIxListWith OutDim 0 (natVal (Proxy @no))
    conj = mkConstraints paramList inList outList
