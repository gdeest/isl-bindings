{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Skolem-indexed Alpha Core (Phase A.2 scaffolding).
--
-- This module is the target shape of the Alpha Core rewrite described
-- in @/home/huginn/.claude/plans/vectorized-crafting-widget.md@.  It
-- replaces the list-index-fused GADT of "Alpha.Core" with a three-layer
-- split:
--
--   * __Identity__: domains, maps, and variables are identified by
--     opaque skolems (@d@, @m@, @v@) introduced via rank-N CPS.  Two
--     nodes refer to the same ISL object iff they share a skolem.
--   * __Coherence__: polyhedral relations (@ImageSubset@, @Partition@,
--     @Subset@, @DefinesAll@, @InScope@) are witnessed by uninhabited
--     proof tokens minted only by the checkers in "Alpha.Core.Tokens".
--   * __Content__: the ISL payload backing a skolem is recovered on
--     demand via the @KnownDom@ / @KnownMap@ / @KnownVar@ classes in
--     "Alpha.Core.Reify".
--
-- This turn only defines the data shape; no elaborator, no bridge,
-- no consumer migration.  Both this module and the legacy
-- "Alpha.Core" compile in parallel until Phase C.
module Alpha.Core.V2
  ( -- * Expressions
    Expr(..)
  , CaseBranches(..)
    -- * Declarations
  , VarDecl(..)
  , SomeVarDecl(..)
    -- * Equations
  , SomeEquation(..)
    -- * Systems
  , System(..)
  , SomeDefinesAll(..)
    -- * Re-exports (Expr operation tags, scalar aliases)
  , BinOp(..), UnaryOp(..), ReduceOp(..)
  , VarName, ScalarTy
  , IslSet, IslMap
    -- * Re-exported from "Alpha.Core.Reify" so consumers of 'Expr'
    --   patterns see @VarDomOf sys v@ without a second import.
  , VarDomOf
  ) where

import Data.Kind    (Type)
import Data.Proxy   (Proxy)
import GHC.TypeLits (Symbol)

import Alpha.Codegen.COp  (BinOp(..), UnaryOp(..), ReduceOp(..))
import Alpha.Core.Named   (Named)
import Alpha.Core.Reify   (KnownDom, KnownMap, KnownVar, VarDomOf, VarName, ScalarTy)
import Alpha.Core.Tokens
  ( DefinesAll
  , ImageSubset
  , InScope
  , IslMap
  , IslSet
  , Partition
  , Subset
  )
import Alpha.Scalar       (AlphaScalar)

-- ═══════════════════════════════════════════════════════════════════════
-- §2. Expressions
-- ═══════════════════════════════════════════════════════════════════════

-- | An Alpha expression scoped to system @sys@, living on domain
-- skolem @dom@, producing values of type @a@.
--
-- The domain index @dom@ is an opaque type: two expressions share a
-- domain iff they share the skolem.  @sys@ scopes the skolem to one
-- elaborated system so that reify-time instances don't leak across
-- systems.
type Expr :: Type -> Type -> Type -> Type
data Expr sys dom a where
  -- | Variable reference.  Carries a proof token @InScope sys v@ that
  -- the elaborator minted for @v@, and a @Proxy v@ to recover the
  -- payload via 'KnownVar'.  The variable's domain is @VarDomOf sys v@.
  Var    :: KnownVar sys v
         => InScope sys v
         -> Proxy v
         -> Expr sys (VarDomOf sys v) a

  -- | Domain-polymorphic scalar literal.  @AlphaScalar a@ captures the
  -- value's bridge for downstream rendering (same rationale as in the
  -- legacy 'Alpha.Core.Const').
  Const  :: AlphaScalar a
         => a
         -> Expr sys dom a

  -- | Pointwise binary operation.  Operands and result share domain
  -- @d@ and scalar type @a@ — structurally equal, no token required.
  Pw     :: BinOp
         -> Expr sys d a
         -> Expr sys d a
         -> Expr sys d a

  -- | Pointwise unary operation.
  PMap   :: UnaryOp
         -> Expr sys d a
         -> Expr sys d a

  -- | Dependence: reindex a sub-expression on @src@ into @dom@ via
  -- the map skolem @m@.  The @ImageSubset m src dom@ token is the
  -- elaborator's certificate that @image(m | src) ⊆ dom@.
  Dep    :: ( KnownMap sys m
            , KnownDom sys src
            , KnownDom sys dom )
         => ImageSubset m src dom
         -> Proxy m
         -> Expr sys src a
         -> Expr sys dom a

  -- | Reduction along a projection map.  Image of @body@ under @p@
  -- must lie in @dom@; the token is the elaborator's certificate.
  Reduce :: ( KnownMap sys p
            , KnownDom sys body
            , KnownDom sys dom )
         => ImageSubset p body dom
         -> ReduceOp
         -> Proxy p
         -> Expr sys body a
         -> Expr sys dom a

  -- | Case split.  @Partition dom bs@ certifies that the branch
  -- domains @bs@ partition the ambient @dom@ (pairwise disjoint,
  -- covering) — the same polyhedral-soundness property that
  -- @IslPartitionsD@ discharged in the legacy Core.
  Case   :: KnownDom sys dom
         => Partition dom bs
         -> CaseBranches sys bs a
         -> Expr sys dom a

-- | Snoc-list of case branches, indexed at the type level by the list
-- @bs@ of branch-domain skolems so that the parent @Case@'s
-- @Partition@ token can reference them.
--
-- Shape mirrors the legacy @Alpha.Core.Branches@: each branch carries
-- its own domain skolem @b@ plus a @Subset b dom@ token constraining
-- it to lie inside the case's ambient.  @dom@ is existentially
-- quantified per-branch because @bs@ alone doesn't fix it; the parent
-- @Case@ ties them together via its @Partition@.
type CaseBranches :: Type -> [Type] -> Type -> Type
data CaseBranches sys bs a where
  BNil  :: CaseBranches sys '[] a
  BCons :: forall sys b dom bs a.
           KnownDom sys b
        => Subset b dom
        -> Proxy b
        -> Expr sys b a
        -> CaseBranches sys bs a
        -> CaseBranches sys (b ': bs) a

-- ═══════════════════════════════════════════════════════════════════════
-- §3. Declarations
-- ═══════════════════════════════════════════════════════════════════════

-- | Declared variable at the value level.
--
-- The variable's skolem @v@ is a type-level tag only; the domain is
-- recovered via @reifyDom \@sys \@(VarDomOf sys v)@, so 'VarDecl'
-- itself stores only the runtime-visible name, dimension count, and
-- scalar type.
type VarDecl :: Type -> Symbol -> Type
data VarDecl sys v = VarDecl
  { vdName   :: !VarName
  , vdDims   :: !Int
  , vdScalar :: !ScalarTy
  }

-- | A declaration whose skolem is existentially hidden.
--
-- Lists of declarations in 'System' use this wrapper because distinct
-- entries bind distinct skolems; consumers pattern-match to bring the
-- per-entry skolem back into scope, then use 'KnownVar' / 'VarDomOf'
-- to recover the payload.  Mirrors the 'SomeConstraintsMap' +
-- 'withKnownConstraints' idiom in "Isl.TypeLevel.Sing".
data SomeVarDecl sys where
  SomeVarDecl :: KnownVar sys v
              => Proxy v
              -> VarDecl sys v
              -> SomeVarDecl sys

-- ═══════════════════════════════════════════════════════════════════════
-- §4. Equations
-- ═══════════════════════════════════════════════════════════════════════

-- | An equation with its defined-variable skolem existentially hidden.
--
-- The equality constraint @dom ~ VarDomOf sys v@ pins the body's
-- domain to the declared variable's domain — the shape check the
-- legacy @Defines@ constructor enforced through 'Lookup name decls'.
data SomeEquation sys a where
  SomeEquation :: ( KnownVar sys v
                  , KnownDom sys dom
                  , dom ~ VarDomOf sys v )
               => Proxy v
               -> Expr sys dom a
               -> SomeEquation sys a

-- ═══════════════════════════════════════════════════════════════════════
-- §5. Systems
-- ═══════════════════════════════════════════════════════════════════════

-- | Existential wrapper for the totality token.
--
-- The specific type-level name lists @dcs@/@eqs@ are installed by the
-- elaborator and consumers only need "a valid token exists", so we
-- hide them rather than threading phantom families through 'System'.
-- Unpack with a pattern match when a consumer needs to thread the
-- token back into a fresh obligation (rare; transforms do not).
data SomeDefinesAll = forall dcs eqs. SomeDefinesAll (DefinesAll dcs eqs)

-- | A complete Alpha system.
--
-- 'sysParams' and 'sysParamCs' are skolem-tagged via the system
-- identity @sys@ so that all nested domains and maps are known to
-- share the same parameter space at compile time without any
-- structural type-family reasoning.
--
-- 'sysInputs' / 'sysOutputs' / 'sysLocals' are existential lists —
-- see 'SomeVarDecl'.  'sysEqs' is similarly existential.
--
-- 'sysTotality' witnesses that the set of equation LHS names matches
-- outputs ++ locals exactly (no missing, duplicate, or extra defs).
data System sys a = System
  { sysParams   :: Named sys [String]
  , sysParamCs  :: Named sys IslSet
  , sysInputs   :: [SomeVarDecl sys]
  , sysOutputs  :: [SomeVarDecl sys]
  , sysLocals   :: [SomeVarDecl sys]
  , sysEqs      :: [SomeEquation sys a]
  , sysTotality :: !SomeDefinesAll
  }
