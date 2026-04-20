{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Reification classes for the Alpha Core rewrite: recover ISL
-- payload from opaque skolem indices via class dispatch.
--
-- Nodes of the skolem-indexed GADT in "Alpha.Core.V2" carry only
-- @Proxy d@ (or @Proxy m@, @Proxy v@); the ISL object is recovered
-- at use sites through 'reifyDom' / 'reifyMap' / 'reifyVar'.  CPS
-- installers fabricate fresh skolems and install the corresponding
-- class instances so subsequent uses see the payload.
--
-- __Dictionary fabrication.__  For all three name kinds we delegate
-- to 'Data.Reflection.reify' as the underlying dictionary-installation
-- engine: it gives a fresh kind-@Type@ skolem @s@ with @Reifies s a@
-- in scope, which overlappable instances lift to @KnownDom sys s@,
-- @KnownMap sys s@, or @KnownVar sys v@ respectively.
--
-- The variable path has two extra twists:
--
--   * A kind-coerce from @Type@ (what 'reify' binds) up to @Symbol@
--     (what @KnownVar@ demands).  Implemented by 'symbolKinded',
--     which repacks the existential through a same-layout GADT pair
--     ('MkSymProxy' → 'SymProxy') under a single 'unsafeCoerce'.
--
--   * An axiom @VarDomOf sys v ~ d@, installed by 'fakeVarDom' —
--     the only way to extend an open type family dynamically.  This
--     coerces a reflexive ':~:' value; its runtime representation is
--     independent of the index types so the coercion is inert.
--
-- Both coercions sit at the trust boundary; they are documented
-- inline.  No other 'unsafeCoerce' appears in the Alpha package
-- besides 'Alpha.Core.Tokens.mkToken'.
module Alpha.Core.Reify
  ( -- * Classes
    KnownDom(..)
  , KnownMap(..)
  , KnownVar(..)
    -- * Open family tying a variable skolem to its domain skolem
  , VarDomOf
    -- * CPS installers
  , withFreshDom
  , withFreshMap
  , withFreshVar
    -- * Types
  , VarName
  , ScalarTy
  ) where

import Data.Kind       (Type)
import Data.Proxy      (Proxy(..))
import Data.Reflection (Reifies, reify, reflect)
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits    (Symbol)
import Unsafe.Coerce   (unsafeCoerce)

import Alpha.Core.Tokens (IslSet, IslMap)
import Alpha.Scalar      (CNumType)

-- ═══════════════════════════════════════════════════════════════════════
-- §0. VarDomOf — skolem → domain skolem (open family)
-- ═══════════════════════════════════════════════════════════════════════

-- | The domain skolem of a declared variable.
--
-- Open family: 'withFreshVar' below installs a per-variable
-- equation at reify time via 'fakeVarDom' — it cannot literally add
-- open-family instances, but 'fakeVarDom' closes over a scoped
-- equation @VarDomOf sys v ~ d@ that a ':~:' pattern-match brings
-- into scope.  The family never reduces under an abstract @v@,
-- matching how the rest of the coherence machinery is consumed —
-- see risk (1) in the plan.
type family VarDomOf (sys :: Type) (v :: Symbol) :: Type

-- ═══════════════════════════════════════════════════════════════════════
-- §1. Aliases
-- ═══════════════════════════════════════════════════════════════════════

type VarName  = String
type ScalarTy = CNumType

-- ═══════════════════════════════════════════════════════════════════════
-- §2. Classes
-- ═══════════════════════════════════════════════════════════════════════

-- | @KnownDom sys d@: the opaque skolem @d@, scoped to system @sys@,
-- reifies to an 'IslSet'.
class KnownDom (sys :: Type) (d :: Type) where
  reifyDom :: Proxy sys -> Proxy d -> IslSet

-- | @KnownMap sys m@: the opaque skolem @m@, scoped to system @sys@,
-- reifies to an 'IslMap'.
class KnownMap (sys :: Type) (m :: Type) where
  reifyMap :: Proxy sys -> Proxy m -> IslMap

-- | @KnownVar sys v@: declared variable @v@ in system @sys@ carries
-- a name, dimensionality, and scalar type.
class KnownVar (sys :: Type) (v :: Symbol) where
  reifyVar :: Proxy sys -> Proxy v -> (VarName, Int, ScalarTy)

-- | Overlappable bridge: any Symbol-kinded skolem @v@ that reifies
-- to the scalar triple is a valid @KnownVar sys v@ for any @sys@.
-- (The elaborator's @withFreshVar@ installs the @Reifies@ instance.)
instance {-# OVERLAPPABLE #-} Reifies v (VarName, Int, ScalarTy)
    => KnownVar sys v where
  reifyVar _ p = reflect p

-- ═══════════════════════════════════════════════════════════════════════
-- §3. Reifies-backed witness carriers (dom / map)
-- ═══════════════════════════════════════════════════════════════════════
--
-- 'Data.Reflection.Reifies' is single-parameter: @Reifies s a@ says
-- "skolem s reifies to value of type a".  We layer the two-parameter
-- 'KnownDom' / 'KnownMap' classes on top by making @sys@ a
-- per-instance context: for any @d@ that @Reifies d IslSet@, we
-- provide @KnownDom sys d@ for every @sys@.  The @sys@ parameter
-- carries no runtime content here — it is a naming discipline.
--
-- This mirrors the pattern in
-- @typelevel/src/Isl/TypeLevel/Sing.hs:withKnownConstraints@ but
-- leans on the vetted 'reflection' engine rather than a bespoke
-- unsafeCoerce.  Chosen for: (a) less hand-rolled trust, (b) the
-- single-parameter shape fits each name kind cleanly.

instance {-# OVERLAPPABLE #-} Reifies d IslSet => KnownDom sys d where
  reifyDom _ p = reflect p

instance {-# OVERLAPPABLE #-} Reifies m IslMap => KnownMap sys m where
  reifyMap _ p = reflect p

-- ═══════════════════════════════════════════════════════════════════════
-- §4. CPS installers — dom / map
-- ═══════════════════════════════════════════════════════════════════════

-- | Install a fresh domain skolem @d@ bound to the given 'IslSet'.
--
-- The continuation's rank-N binding of @d@ guarantees that distinct
-- 'withFreshDom' calls produce incomparable skolems.
withFreshDom
  :: forall sys r
   . Proxy sys
  -> IslSet
  -> (forall d. KnownDom sys d => Proxy d -> r)
  -> r
withFreshDom _ s k = reify s $ \(p :: Proxy d) -> k @d p

-- | Install a fresh map skolem @m@ bound to the given 'IslMap'.
withFreshMap
  :: forall sys r
   . Proxy sys
  -> IslMap
  -> (forall m. KnownMap sys m => Proxy m -> r)
  -> r
withFreshMap _ mmap k = reify mmap $ \(p :: Proxy m) -> k @m p

-- ═══════════════════════════════════════════════════════════════════════
-- §5. CPS installer — variable (Symbol skolem + VarDomOf axiom)
-- ═══════════════════════════════════════════════════════════════════════
--
-- Two obligations ride on this single installer:
--
--   1.  A fresh @Symbol@-kinded skolem @v@ with @KnownVar sys v@ in
--       scope, returning the @(VarName, Int, ScalarTy)@ payload.
--   2.  An equation @VarDomOf sys v ~ d@, where @d@ is the caller's
--       (also-fresh) domain skolem.
--
-- Obligation 1 is satisfied by 'reify' + 'symbolKinded', obligation
-- 2 by 'fakeVarDom'.  Both helpers live in this section.


-- | Install a fresh variable skolem @v@ and pair it with the caller's
-- domain skolem @d@, such that @VarDomOf sys v ~ d@ holds.
--
-- @
-- withFreshVar (Proxy :: Proxy sys) (\"x\", 2, CFloat64, domSet) $ \\(Proxy :: Proxy v) (Proxy :: Proxy d) -> ...
-- @
--
-- The continuation's rank-N binding ensures the skolems escape no
-- further than the call site.
withFreshVar
  :: forall sys r
   . Proxy sys
  -> (VarName, Int, ScalarTy, IslSet)
  -> (forall (v :: Symbol) (d :: Type).
        ( KnownVar sys v
        , KnownDom sys d
        , VarDomOf sys v ~ d )
      => Proxy v -> Proxy d -> r)
  -> r
withFreshVar proxSys (nm, dims, scalar, islSet) k =
  -- Step 1: install a fresh Type-kinded domain skolem @d@ with
  -- @KnownDom sys d@ carrying @islSet@.
  withFreshDom proxSys islSet $ \(proxD :: Proxy d) ->
    -- Step 2: use 'Data.Reflection.reify' at Type-kind to install
    -- @Reifies s triple@ in scope; then open a 'SymProxy' existential
    -- to recast the Type-kinded @s@ as a @Symbol@-kinded @v@ with
    -- the same @Reifies v triple@ witness.  The overlappable
    -- @Reifies v triple => KnownVar sys v@ instance above then
    -- satisfies @KnownVar sys v@ at any use site.  Finally,
    -- 'fakeVarDom' supplies the axiom @VarDomOf sys v ~ d@.
    reify (nm, dims, scalar) $ \proxS ->
      case symbolKinded proxS of
        SymProxy (proxV :: Proxy v) ->
          case fakeVarDom @sys @v @d of
            Refl -> k @v @d proxV proxD

-- | Kind-coerce a 'Proxy s' (where @s :: Type@ with
-- @Reifies s (VarName, Int, ScalarTy)@ in scope) to a
-- 'Proxy (v :: Symbol)' with @Reifies v (...)@ in scope.  The
-- coercion changes only the __kind__ of the existential tag; the
-- underlying 'Reifies' dictionary is unchanged, so downstream
-- @reifyVar@ / @reflect@ calls resolve to the same triple.
--
-- Soundness: the two GADT wrappers 'SymProxy' and 'MkSymProxy' have
-- identical runtime representation (a single 'Reifies' dictionary
-- paired with a 'Proxy' of zero runtime data); both dictionaries
-- are the same 'Reifies' class, so the coercion is a pure
-- kind-signature rewrite.
symbolKinded
  :: forall (s :: Type)
   . Reifies s (VarName, Int, ScalarTy)
  => Proxy s -> SymProxy
symbolKinded _ = unsafeCoerce (MkSymProxy (Proxy :: Proxy s))

-- | Existential wrapper for a @Symbol@-kinded skolem @v@ with
-- 'Reifies' in scope.  The companion @KnownVar sys v@ is obtained
-- at use sites via the overlappable instance above, for any @sys@.
data SymProxy where
  SymProxy :: forall (v :: Symbol).
              Reifies v (VarName, Int, ScalarTy) => Proxy v -> SymProxy

-- | Type-kinded pre-image of 'SymProxy'.  Same runtime layout; the
-- unsafeCoerce in 'symbolKinded' relies on this being structurally
-- identical modulo the @v@ kind.
data MkSymProxy where
  MkSymProxy :: forall (s :: Type).
                Reifies s (VarName, Int, ScalarTy)
             => Proxy s -> MkSymProxy

-- | Install @VarDomOf sys v ~ d@ by fiat.  This is a genuine axiom:
-- open type families have no instance-definition channel exposed at
-- the value level, so the only way to extend the family at runtime
-- is to coerce a proof of reflexive equality into a proof of the
-- desired (heterogeneous) equality.  Both sides of a ':~:' value
-- share the same runtime representation (a single 'Refl'
-- constructor), so the coercion is operationally inert.
--
-- Soundness rests on the caller ('withFreshVar') pairing each
-- freshly-minted @v@ with exactly one @d@; distinct @v@ skolems are
-- incomparable, so no second axiom can contradict.
--
-- Analogous in role to 'Alpha.Core.Tokens.mkToken': the one place
-- the Alpha package buys a trust statement from 'unsafeCoerce'.
fakeVarDom :: forall sys (v :: Symbol) (d :: Type). VarDomOf sys v :~: d
fakeVarDom = unsafeCoerce (Refl :: () :~: ())

