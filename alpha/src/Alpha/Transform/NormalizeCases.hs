{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Phase A of the Case-to-polyhedral-statement fan-out: hoist 'Case'
-- nodes toward the top of each equation's RHS so that 'Alpha.Lower'
-- can emit one polyhedral statement per branch.
--
-- Distributive rewrites (applied to fixed point by the expression
-- walker):
--
-- * @'Pw' op ('Case' bs) e@ ⇒ @'Case' { d_i → 'Pw' op e_i (weaken e) }@
-- * @'Pw' op e ('Case' bs)@ ⇒ symmetric
-- * @'PMap' op ('Case' bs)@ ⇒ @'Case' { d_i → 'PMap' op e_i }@
--
-- Not rewritten (the existing ternary path in
-- 'Alpha.Codegen.ExprRender' renders the surviving 'Case'):
--
-- * @'Dep' _ ('Case' …)@ — the inner Case lives in the referenced
--   variable's index space; hoisting would change meaning.
-- * @'Reduce' _ _ (… 'Case' …)@ — the inner Case lives in body-dim
--   @nBody@, not @n@; lifting would require introducing auxiliary
--   equations for per-branch partial reductions (out of scope,
--   see 'Alpha.Surface.Core.Case' Haddock).
--
-- The pass is best-effort: the return type is
-- @'Either' 'TransformError'@ for uniformity with sibling transforms,
-- but every narrowing in this pass is structural (@d_i ∩ amb ⊆ amb@),
-- so the ISL subset check inside 'weakenExpr' is guaranteed to pass
-- at runtime.  A @'Left'@ would indicate an internal bug.
module Alpha.Transform.NormalizeCases
  ( normalizeCases
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat)

import Alpha.Surface.Core
  ( Expr(..), Branches(..), EqList(..), Equation(..), System(..)
  , BinOp, UnaryOp, VarDecl )
import Alpha.Transform.Types (TransformError(..))
import Alpha.Transform.Weaken (weakenExpr)
import Isl.Typed.Params (KnownSymbols, Length)
import Isl.TypeLevel.Constraint (TConstraint)
import Isl.TypeLevel.Reflection
  (DomTag, EffectiveDomTag, KnownDom)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public entry point
-- ═══════════════════════════════════════════════════════════════════════

normalizeCases
  :: forall ps pctx inputs outputs locals.
     (KnownSymbols ps, KnownNat (Length ps))
  => System ps pctx inputs outputs locals
  -> Either TransformError (System ps pctx inputs outputs locals)
normalizeCases (System decls eqs) = do
  eqs' <- walkEqList eqs
  pure (System decls eqs')


-- ═══════════════════════════════════════════════════════════════════════
-- §2. EqList / Equation walkers
-- ═══════════════════════════════════════════════════════════════════════

walkEqList
  :: forall ps pctx decls defined.
     (KnownSymbols ps, KnownNat (Length ps))
  => EqList ps pctx decls defined
  -> Either TransformError (EqList ps pctx decls defined)
walkEqList EqNil        = Right EqNil
walkEqList (eq :& rest) = (:&) <$> walkEq eq <*> walkEqList rest

walkEq
  :: forall ps pctx decls name.
     (KnownSymbols ps, KnownNat (Length ps))
  => Equation ps pctx decls name
  -> Either TransformError (Equation ps pctx decls name)
walkEq (Defines pn body) = Defines pn <$> normExpr body


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Expression walker
-- ═══════════════════════════════════════════════════════════════════════

-- | Recurse into an expression, hoisting 'Case' above 'Pw'/'PMap'
-- whenever possible.  Types are phantom-preserving.
normExpr
  :: forall ps (pctx :: [TConstraint ps 0]) (decls :: [VarDecl ps])
            n (d :: DomTag ps n) a.
     ( KnownSymbols ps, KnownNat (Length ps)
     , KnownNat n, KnownDom ps n d )
  => Expr ps pctx decls n d a
  -> Either TransformError (Expr ps pctx decls n d a)
normExpr e0 = case e0 of
  Const k   -> Right (Const k)
  Var   pn  -> Right (Var pn)

  Pw op a b -> do
    a' <- normExpr a
    b' <- normExpr b
    hoistPw op a' b'

  PMap op a -> do
    a' <- normExpr a
    hoistPMap op a'

  -- Dep: recurse into inner; do NOT hoist out (inner lives in a
  -- different index space; see module Haddock).
  Dep mapP inner -> Dep mapP <$> normExpr inner

  -- Reduce: recurse into body; do NOT hoist out (body lives at
  -- nBody ≠ n; distribution would require auxiliary equations).
  Reduce rop projP body -> Reduce rop projP <$> normExpr body

  -- Case: recurse into each branch body at its effective ambient.
  Case branches -> Case <$> normBranches branches

normBranches
  :: forall ps (pctx :: [TConstraint ps 0]) (decls :: [VarDecl ps]) n
            (amb :: DomTag ps n) (branchDoms :: [DomTag ps n]) a.
     ( KnownSymbols ps, KnownNat (Length ps)
     , KnownNat n )
  => Branches ps pctx decls n amb branchDoms a
  -> Either TransformError (Branches ps pctx decls n amb branchDoms a)
normBranches BNil = Right BNil
normBranches (BCons pd body rest) =
  BCons pd <$> normExpr body <*> normBranches rest


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Hoisting rules
-- ═══════════════════════════════════════════════════════════════════════

-- | Distribute 'Pw' over a 'Case' operand, when one is present.
-- Weakens the /other/ operand into each branch's effective domain.
--
-- When both operands are 'Case', we distribute through the left one;
-- the right 'Case' ends up weakened (wrapped in 'Dep' @identityMap)
-- into each left branch.  The plan's "Cartesian product" distribution
-- would require an extra pass after unwrapping the identity-'Dep';
-- it is not implemented in this first cut.
hoistPw
  :: forall ps pctx (decls :: [VarDecl ps]) n (d :: DomTag ps n) a.
     ( KnownSymbols ps, KnownNat (Length ps)
     , KnownNat n, KnownDom ps n d )
  => BinOp
  -> Expr ps pctx decls n d a
  -> Expr ps pctx decls n d a
  -> Either TransformError (Expr ps pctx decls n d a)
hoistPw op (Case bs) rhs = Case <$> pushPwBranchesL op bs rhs
hoistPw op lhs (Case bs) = Case <$> pushPwBranchesR op lhs bs
hoistPw op a b           = Right (Pw op a b)

-- | Distribute 'PMap' over a 'Case' operand.
hoistPMap
  :: forall ps pctx (decls :: [VarDecl ps]) n (d :: DomTag ps n) a.
     ( KnownSymbols ps, KnownNat (Length ps)
     , KnownNat n, KnownDom ps n d )
  => UnaryOp
  -> Expr ps pctx decls n d a
  -> Either TransformError (Expr ps pctx decls n d a)
hoistPMap op (Case bs) = Right (Case (pushPMapBranches op bs))
hoistPMap op e         = Right (PMap op e)


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Per-constructor branch rebuilders
-- ═══════════════════════════════════════════════════════════════════════

-- | Distribute @Pw op · rhs@ through a branch list (Case on the left).
-- @rhs@ lives at the Case's ambient @d@; each branch body lives at
-- @EffectiveDomTag di d@; we weaken @rhs@ down to the same narrower
-- domain and pair it with the branch body.
pushPwBranchesL
  :: forall ps pctx (decls :: [VarDecl ps]) n
            (amb :: DomTag ps n) (branchDoms :: [DomTag ps n]) a.
     ( KnownSymbols ps, KnownNat (Length ps), KnownNat n
     , KnownDom ps n amb )
  => BinOp
  -> Branches ps pctx decls n amb branchDoms a
  -> Expr ps pctx decls n amb a
  -> Either TransformError (Branches ps pctx decls n amb branchDoms a)
pushPwBranchesL _  BNil                      _   = Right BNil
pushPwBranchesL op (BCons pd body rest) rhs = do
  body' <- mkPwL pd op body rhs
  rest' <- pushPwBranchesL op rest rhs
  pure (BCons pd body' rest')

-- | Symmetric: distribute @Pw op lhs · @ through branches.
pushPwBranchesR
  :: forall ps pctx (decls :: [VarDecl ps]) n
            (amb :: DomTag ps n) (branchDoms :: [DomTag ps n]) a.
     ( KnownSymbols ps, KnownNat (Length ps), KnownNat n
     , KnownDom ps n amb )
  => BinOp
  -> Expr ps pctx decls n amb a
  -> Branches ps pctx decls n amb branchDoms a
  -> Either TransformError (Branches ps pctx decls n amb branchDoms a)
pushPwBranchesR _  _   BNil                      = Right BNil
pushPwBranchesR op lhs (BCons pd body rest) = do
  body' <- mkPwR pd op lhs body
  rest' <- pushPwBranchesR op lhs rest
  pure (BCons pd body' rest')

-- | Distribute 'PMap' op through branches — no weakening needed.
pushPMapBranches
  :: forall ps pctx (decls :: [VarDecl ps]) n
            (amb :: DomTag ps n) (branchDoms :: [DomTag ps n]) a.
     UnaryOp
  -> Branches ps pctx decls n amb branchDoms a
  -> Branches ps pctx decls n amb branchDoms a
pushPMapBranches _  BNil               = BNil
pushPMapBranches op (BCons pd body rest) =
  BCons pd (PMap op body) (pushPMapBranches op rest)


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Per-branch weakening helpers
-- ═══════════════════════════════════════════════════════════════════════

-- | @Pw op body_i (weaken rhs)@ at the branch effective domain.
-- The 'Proxy' threads the branch existential @di@ into the signature
-- so callers pattern-matching 'BCons' don't need explicit type
-- applications to satisfy the non-injective 'EffectiveDomTag'.
mkPwL
  :: forall ps pctx (decls :: [VarDecl ps]) n
            (amb :: DomTag ps n) (di :: DomTag ps n) a.
     ( KnownSymbols ps, KnownNat (Length ps), KnownNat n
     , KnownDom ps n amb
     , KnownDom ps n (EffectiveDomTag di amb) )
  => Proxy di
  -> BinOp
  -> Expr ps pctx decls n (EffectiveDomTag di amb) a
  -> Expr ps pctx decls n amb a
  -> Either TransformError (Expr ps pctx decls n (EffectiveDomTag di amb) a)
mkPwL _ op body rhs =
  case weakenExpr (Proxy @(EffectiveDomTag di amb)) rhs of
    Just rhs' -> Right (Pw op body rhs')
    Nothing   -> Left (DomainShrinkUnsafe
                   "normalizeCases: weaken rhs"
                   "<branch-effective>" "<ambient>")

-- | @Pw op (weaken lhs) body_i@ — mirror of 'mkPwL'.
mkPwR
  :: forall ps pctx (decls :: [VarDecl ps]) n
            (amb :: DomTag ps n) (di :: DomTag ps n) a.
     ( KnownSymbols ps, KnownNat (Length ps), KnownNat n
     , KnownDom ps n amb
     , KnownDom ps n (EffectiveDomTag di amb) )
  => Proxy di
  -> BinOp
  -> Expr ps pctx decls n amb a
  -> Expr ps pctx decls n (EffectiveDomTag di amb) a
  -> Either TransformError (Expr ps pctx decls n (EffectiveDomTag di amb) a)
mkPwR _ op lhs body =
  case weakenExpr (Proxy @(EffectiveDomTag di amb)) lhs of
    Just lhs' -> Right (Pw op lhs' body)
    Nothing   -> Left (DomainShrinkUnsafe
                   "normalizeCases: weaken lhs"
                   "<branch-effective>" "<ambient>")
