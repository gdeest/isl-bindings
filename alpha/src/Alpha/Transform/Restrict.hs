{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Domain restriction (subsumption) for surface Alpha expressions.
--
-- 'restrict' narrows an expression's declared domain phantom from
-- @dInner@ to any @dOuter@ with @dOuter ⊆ dInner@, producing a
-- 'Alpha.Surface.Core.Restrict' node.  The subset obligation is
-- discharged at runtime via 'islSubsetCheckSPctx' (an ISL @isl_set_is_subset@
-- on the materialised DomTags); on success the returned expression
-- carries the new phantom and is semantically transparent (Restrict
-- nodes are recursed-through by every consumer).
module Alpha.Transform.Restrict
  ( restrict
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, Symbol)

import Alpha.Surface.Core (Expr(..), VarDecl)
import Isl.TypeLevel.Constraint (TConstraint)
import Isl.TypeLevel.Reflection
  ( Dict(..), DomTag, KnownDom, islSubsetCheckSPctx )
import Isl.Typed.Params (KnownSymbols, Length)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Restrict the declared domain of an expression from @dInner@ to
-- @dOuter@, producing a 'Restrict' wrap.
--
-- Returns 'Nothing' iff the ISL subset check @dOuter ⊆ dInner@ fails;
-- on success the returned expression carries the new phantom.
restrict
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (decls :: [VarDecl ps])
            (n :: Nat)
            (dInner :: DomTag ps n) (dOuter :: DomTag ps n) a.
     ( KnownSymbols ps, KnownNat (Length ps)
     , KnownNat n
     , KnownDom ps n dInner
     , KnownDom ps n dOuter
     )
  => Proxy dOuter
  -> Expr ps pctx decls n dInner a
  -> Maybe (Expr ps pctx decls n dOuter a)
restrict _ inner =
  case islSubsetCheckSPctx @ps @pctx @n @dOuter @dInner Proxy Proxy Proxy of
    Nothing   -> Nothing
    Just Dict -> Just (Restrict inner)
