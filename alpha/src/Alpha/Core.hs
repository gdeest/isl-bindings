{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | Skolem-indexed Alpha Core.
--
-- Three-layer split:
--
--   * __Identity__: domains (@d@) and maps (@m@) carry opaque
--     Type-kinded skolems introduced via 'Alpha.Core.Named.name';
--     variables carry Symbol-kinded names.  Two nodes refer to the
--     same ISL object iff they share a skolem (or, for vars, a
--     Symbol).
--   * __Coherence__: polyhedral relations (@ImageSubset@, @Partition@,
--     @Subset@, @DefinesAll@, @InScope@) are witnessed by uninhabited
--     proof tokens minted only by the checkers in
--     "Alpha.Core.Tokens".
--   * __Content__: ISL payload travels with each node as a 'Named'
--     field; no class-based dictionary resolution.
--
-- No @unsafeCoerce@ lives here; the only trust statement is the one
-- in "Alpha.Core.Tokens.mkToken".  Replaces the @KnownDom@ /
-- @KnownMap@ / @KnownVar@ classes (formerly in "Alpha.Core.Reify",
-- now deleted) whose job is done directly by 'Named' fields.
module Alpha.Core
  ( -- * Expressions
    Expr(..)
  , CaseBranches(..)
    -- * Declarations
  , VarDecl(..)
  , SomeVarDecl(..)
  , varDeclProxy
    -- * Equations
  , SomeEquation(..)
    -- * Systems
  , System(..)
  , SomeDefinesAll(..)
    -- * Re-exports (Expr operation tags, aliases)
  , BinOp(..), UnaryOp(..), ReduceOp(..)
  , VarName, ScalarTy
  , IslSet, IslMap
  ) where

import Data.Kind    (Type)
import Data.Proxy   (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol)

import Alpha.Codegen.COp  (BinOp(..), UnaryOp(..), ReduceOp(..))
import Alpha.Core.Named   (Named)
import Alpha.Core.Tokens
  ( DefinesAll
  , ImageSubset
  , InScope
  , IslMap
  , IslSet
  , Partition
  , Subset
  )
import Alpha.Scalar       (AlphaScalar, CNumType)

-- ═══════════════════════════════════════════════════════════════════════
-- §1. Local aliases
-- ═══════════════════════════════════════════════════════════════════════

type VarName  = String
type ScalarTy = CNumType

-- ═══════════════════════════════════════════════════════════════════════
-- §2. Expressions
-- ═══════════════════════════════════════════════════════════════════════

-- | An Alpha expression scoped to system @sys@, living on domain
-- skolem @dom@, producing values of type @a@.
--
-- @dom@ is opaque at this layer: identity is by skolem equality; the
-- concrete 'IslSet' travels with each node that introduces one (see
-- 'Var', 'Dep', 'Reduce', 'Case').
type Expr :: Type -> Type -> Type -> Type
data Expr sys dom a where
  -- | Variable reference.  The 'Named dom IslSet' field carries the
  -- variable's declared domain; the @dom@ skolem is what ties this
  -- node to other 'Expr's on the same domain.  @KnownSymbol v@ lets
  -- walkers recover the runtime name via 'symbolVal'.
  Var    :: KnownSymbol v
         => Proxy v
         -> InScope sys v
         -> Named dom IslSet
         -> Expr sys dom a

  -- | Domain-polymorphic scalar literal.  @AlphaScalar a@ captures
  -- the value's bridge for downstream rendering.
  Const  :: AlphaScalar a
         => a
         -> Expr sys dom a

  -- | Pointwise binary operation.  Operands and result share domain
  -- @d@ and scalar @a@ — structurally equal, no token required.
  Pw     :: BinOp
         -> Expr sys d a
         -> Expr sys d a
         -> Expr sys d a

  -- | Pointwise unary operation.
  PMap   :: UnaryOp
         -> Expr sys d a
         -> Expr sys d a

  -- | Dependence: reindex a sub-expression living on the accessed
  -- array's domain @src@ into the iteration domain @dom@ via the
  -- access map @m : dom → src@.  Polarity: @image(m | dom) ⊆ src@ —
  -- every iteration point maps to a valid source point.  The token is
  -- the elaborator's certificate of this inclusion; in Alpha semantics
  -- the body on @src@ is the accessed array and the result on @dom@
  -- is the current iteration domain.
  Dep    :: Named m   IslMap
         -> Named src IslSet
         -> ImageSubset m dom src
         -> Expr sys src a
         -> Expr sys dom a

  -- | Reduction along a projection map @p@.  Image of @body@ under
  -- @p@ must lie in @dom@; the token is the elaborator's certificate.
  Reduce :: ReduceOp
         -> Named p    IslMap
         -> Named body IslSet
         -> ImageSubset p body dom
         -> Expr sys body a
         -> Expr sys dom a

  -- | Case split.  @Partition dom bs@ certifies that branch domains
  -- @bs@ partition the ambient @dom@ (pairwise disjoint, covering).
  Case   :: Partition dom bs
         -> CaseBranches sys dom bs a
         -> Expr sys dom a

-- | Snoc-list of case branches.  All branches share the ambient
-- @dom@; per-branch skolem @b@ is existentially introduced by 'BCons'
-- and tied to @dom@ by the 'Subset b dom' token.
type CaseBranches :: Type -> Type -> [Type] -> Type -> Type
data CaseBranches sys dom bs a where
  BNil  :: CaseBranches sys dom '[] a
  BCons :: Named b IslSet
        -> Subset b dom
        -> Expr sys b a
        -> CaseBranches sys dom bs a
        -> CaseBranches sys dom (b ': bs) a

-- ═══════════════════════════════════════════════════════════════════════
-- §3. Declarations
-- ═══════════════════════════════════════════════════════════════════════

-- | Declared variable.  @v :: Symbol@ is the variable's name at the
-- type level; @dom :: Type@ is the opaque skolem for its declared
-- domain.  The 'Named dom IslSet' field carries the domain's 'IslSet'
-- payload; the skolem @dom@ is the identity tag.
--
-- Fields mirror the legacy 'Alpha.Surface.Core.VarDecl'; 'vdDom' is new and
-- replaces the type-level 'DeclDomTag' / 'DomTag ps n' reflection.
type VarDecl :: Type -> Symbol -> Type -> Type
data VarDecl sys v dom = VarDecl
  { vdName   :: !VarName
  , vdDims   :: !Int
  , vdScalar :: !ScalarTy
  , vdDom    :: !(Named dom IslSet)
  }

-- | A declaration with both the variable's Symbol @v@ and its domain
-- skolem @dom@ hidden existentially.  Pattern-matching brings both
-- back into scope so walkers can recover the name via 'symbolVal' and
-- the domain via 'vdDom'.
data SomeVarDecl sys where
  SomeVarDecl :: KnownSymbol v
              => Proxy v
              -> VarDecl sys v dom
              -> SomeVarDecl sys

-- | Recover the type-level name proxy from a 'VarDecl'.  Useful when a
-- walker has the 'VarDecl' in hand and needs the 'Proxy v' for
-- 'symbolVal' without re-introducing a separate field.
varDeclProxy :: VarDecl sys v dom -> Proxy v
varDeclProxy _ = Proxy

-- ═══════════════════════════════════════════════════════════════════════
-- §4. Equations
-- ═══════════════════════════════════════════════════════════════════════

-- | An equation: a body 'Expr sys dom a' bound to variable @v@.
--
-- __Invariant (pinned by construction).__  The body's @dom@ skolem is
-- the same skolem as the declared variable's 'vdDom'; the 'VarDecl'
-- field carries both the 'Proxy' @v@ (via its type) and the
-- 'Named dom IslSet' (via 'vdDom').  A transform that rebuilds an
-- equation must therefore pass in the original 'VarDecl' (or a fresh
-- one sharing the same 'Named dom IslSet' skolem); it is impossible
-- for the body's domain to drift from the declared one without a
-- compile-time error.  What used to be a convention between two
-- independent fields is now a type-level equality.
data SomeEquation sys a where
  SomeEquation :: KnownSymbol v
               => VarDecl sys v dom
               -> Expr sys dom a
               -> SomeEquation sys a

-- ═══════════════════════════════════════════════════════════════════════
-- §5. Systems
-- ═══════════════════════════════════════════════════════════════════════

-- | Existential wrapper for the totality token.  The specific
-- type-level name lists @dcs@/@eqs@ are installed by the elaborator;
-- consumers only need "a valid token exists".
data SomeDefinesAll =
  forall dcs eqs. SomeDefinesAll (DefinesAll dcs eqs)

-- | A complete Alpha system.
--
-- 'sysParams' and 'sysParamCs' are tagged by the system skolem @sys@
-- so that all nested domains and maps are known to share the same
-- parameter context without any structural reasoning.
--
-- 'sysInputs' / 'sysOutputs' / 'sysLocals' are existential lists; see
-- 'SomeVarDecl'.  'sysEqs' is similarly existential.
--
-- 'sysTotality' witnesses that the set of equation LHS names matches
-- @outputs ++ locals@ exactly (no missing, duplicate, or extra defs).
data System sys a = System
  { sysParams   :: Named sys [String]
  , sysParamCs  :: Named sys IslSet
  , sysInputs   :: [SomeVarDecl sys]
  , sysOutputs  :: [SomeVarDecl sys]
  , sysLocals   :: [SomeVarDecl sys]
  , sysEqs      :: [SomeEquation sys a]
  , sysTotality :: !SomeDefinesAll
  }
