{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Domain weakening for Alpha expressions.
--
-- 'weakenExpr' narrows an expression's declared domain phantom from
-- @dInner@ to any @dOuter@ with @dOuter ⊆ dInner@.  Implemented as a
-- wrap in a @'Alpha.Core.Dep' \@identityMap@ node — see the Transform
-- polarity block in "Alpha.Transform.Types" for why a recursive
-- phantom rewrite is unsound on 'Alpha.Core.Reduce' and
-- 'Alpha.Core.Case'.
--
-- The subset obligation is discharged at runtime via
-- @'islImageSubsetCheckS'@ against the identity map string; on success
-- the node typechecks, on failure a @Nothing@ comes back.  For
-- callers whose narrowing is structural (e.g. @dOuter = d_i ∩ amb ⊆
-- amb@ inside 'Alpha.Transform.NormalizeCases'), the check is
-- guaranteed to pass — we run it honestly rather than add a new
-- typelevel lemma.
module Alpha.Transform.Weaken
  ( weakenExpr
  ) where

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal, type (+))

import Alpha.Core (Expr(..), VarDecl)
import Isl.TypeLevel.Constraint (TConstraint)
import qualified Isl.Typed.Constraints as TC
import Isl.Typed.Constraints (Constraint(EqualityConstraint), MapIx(InDim, OutDim))
import Isl.Typed.Params (KnownSymbols, Length, symbolVals)
import Isl.TypeLevel.Reflection
  ( Dict(..), DomTag, KnownDom, islImageSubsetCheckSPctx )
import Isl.TypeLevel.Sing
  ( liftConstraintsMap, withKnownConstraints )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Weaken the declared domain of an expression from @dInner@ to
-- @dOuter@, producing a @'Dep' \@identityMap@ wrap.
--
-- Returns 'Nothing' iff the ISL subset check @dOuter ⊆ dInner@ fails;
-- on success the returned expression carries the new phantom and is
-- semantically identical (identity-map Deps are eliminated by ISL
-- during scheduling, so there is no runtime cost).
weakenExpr
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
weakenExpr _ inner =
  let n_val   = fromIntegral (natVal (Proxy @n))
      mapStr  = identityMapStr (symbolVals @ps) n_val
      idCs    = identityConstraints n_val
      someCs  = liftConstraintsMap @ps @(n + n) n_val idCs
   in withKnownConstraints someCs $ \(idProxy :: Proxy identityCs) ->
        case islImageSubsetCheckSPctx @ps @pctx @n @n
               @identityCs
               @dOuter @dInner
               mapStr Proxy Proxy Proxy of
          Nothing   -> Nothing
          Just Dict -> Just (Dep idProxy inner)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Identity-map construction
-- ═══════════════════════════════════════════════════════════════════════

-- | Identity map constraints over the flat @(n + n)@ joined space:
-- one equality @OutDim i - InDim i == 0@ for each @i ∈ [0, n)@.
identityConstraints :: Int -> [Constraint MapIx]
identityConstraints n =
  [ EqualityConstraint
      (TC.Add (TC.Ix (OutDim i)) (TC.Mul (-1) (TC.Ix (InDim i))))
  | i <- [0 .. n - 1]
  ]

-- | Render the identity ISL map string with a parameter prefix.
-- Format: @"[p0, ..., p_{k-1}] -> { [i0, ..., i_{n-1}] -> [i0, ..., i_{n-1}] }"@.
-- Empty parameter list elides the prefix.
identityMapStr :: [String] -> Int -> String
identityMapStr params n =
  let dims = [ "i" ++ show i | i <- [0 .. n - 1] ]
      tup  = "[" ++ intercalate ", " dims ++ "]"
      body = "{ " ++ tup ++ " -> " ++ tup ++ " }"
   in case params of
        [] -> body
        _  -> "[" ++ intercalate ", " params ++ "] -> " ++ body
