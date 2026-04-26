{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Constructive reindexing transform for Alpha systems.
--
-- 'reindex' is the fundamental change-of-basis transform: given a
-- multi-aff map @f : Z^newN -> Z^oldN@ (new coords to old coords),
-- it walks the entire 'System' and reconstructs every node with
-- correct types.
--
-- Zero 'unsafeCoerce' in the walker — all phantom-type changes
-- live in the trusted seal modules ("Alpha.Surface.Core", "Isl.TypeLevel.Sing",
-- "Isl.TypeLevel.Reflection").
module Alpha.Transform.Reindex
  ( reindex
  , TransformError(..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits
  ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal, type (+) )

import Alpha.Surface.Core
import Alpha.Core.Lemmas
  (withReplaceDecl, replaceDeclList, withDefinesAllReplace, withReplaceDeclConcat)
import Alpha.Transform.Types
  (TransformError(..), Proof, requireJust, requireC, internalError)
import Alpha.Transform.Walk (composeAccess)
import Isl.Typed.Params (KnownSymbols(..), Length)
import Isl.TypeLevel.Constraint
  ( IslPreimageMultiAff, TConstraint, IslNonEmpty )
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Reflection
  ( DomTag(..)
  , KnownDom
  , domToString
  , islImageSubsetCheckSPctx
  )
import Isl.TypeLevel.Sing
  ( KnownExprs(..)
  , liftConstraintsMap, withKnownConstraints
  )


-- Axioms for ReplaceDecl (replaceDeclList, definesAllReplace,
-- replaceDeclConcat, lookupReplaceDecl) live in Alpha.Core.Lemmas.


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Expression walker (non-target equations)
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk an expression, changing @decls@ to @ReplaceDecl target nv decls@.
--
-- 'Dep'/'Reduce'/'Case' constraints don't mention @decls@ — the
-- dictionaries from the old constructor directly satisfy the new one.
-- 'Var' uses 'lookupReplaceDecl' to obtain the equality axiom.
--
-- When a 'Dep' wraps a 'Var' referencing the target, the Dep's map
-- is composed with the reverse of the reindex map at runtime (via ISL)
-- and the image-subset obligation is re-verified.
walkExprNonTarget
  :: forall (target :: Symbol) (ps :: [Symbol])
            (pctx :: [TConstraint ps 0])
            (decls :: [VarDecl ps]) (nv :: VarDecl ps)
            (mapExprs :: [TExpr ps (DeclDims nv)])
            n (d :: DomTag ps n) a.
     ( KnownSymbol target
     , KnownDom ps (DeclDims nv) (DeclDomTag nv)
     , KnownNat (DeclDims nv)
     , KnownExprs ps (DeclDims nv) mapExprs
     , DeclType nv ~ DeclType (Lookup target decls)
     , KnownSymbols ps, KnownNat (Length ps)
     )
  => Expr ps pctx decls n d a
  -> Proof (Expr ps pctx (ReplaceDecl target nv decls) n d a)
walkExprNonTarget = go
  where
    targetName = symbolVal (Proxy @target)

    go :: forall n' (d' :: DomTag ps n') a'.
          Expr ps pctx decls n' d' a'
       -> Proof (Expr ps pctx (ReplaceDecl target nv decls) n' d' a')

    go (Const k)   = Right (Const k)
    go (Pw op a b) = Pw op <$> go a <*> go b
    go (PMap op a) = PMap op <$> go a

    go (Var @name pn) =
      withReplaceDecl @ps @target @name @nv @decls
        (Proxy @target) pn
        (Right (Var pn))                 -- name ≠ target: lookup unchanged
        (Left (ImageOutOfBounds
          ("naked Var reference to reindexed variable " ++ targetName
           ++ " (must be wrapped in Dep)")
          (symbolVal pn) targetName))    -- name ~ target: reject naked Var

    -- Dep targeting the reindexed variable: compose map + verify at runtime.
    -- Constructive: builds a fresh Dep(Var) with the composed map's
    -- type-level constraints, verified by ISL.  No unsafeCoerce in user code —
    -- all trust lives in Sing.hs (withKnownConstraints) and Reflection.hs
    -- (islImageSubsetCheckS).
    go (Dep @_ @_ @_ @ni @no @mapCs @dOuter @_dInner @_ _mapP (Var @name @_ @_ @_ @_ pn))
      | symbolVal pn == targetName = do
          let ni_val = fromIntegral (natVal (Proxy @ni))
              srcStr = domToString @ps @ni @dOuter
              dstStr = domToString @ps @(DeclDims nv) @(DeclDomTag nv)
          -- 1. Compose old dep map with reverse of reindex map.
          (mapStr, mapConstrs) <-
            requireJust
              (ImageOutOfBounds
                 "reindex: dep access out of bounds after map composition"
                 srcStr dstStr)
              (composeAccess @ps @ni @no @(DeclDims nv)
                 @mapCs @mapExprs @dOuter @(DeclDomTag nv)
                 (Proxy @dOuter) (Proxy @(DeclDomTag nv)))
          -- 2. Fabricate KnownConstraints for the composed map (CPS).
          let someCs = liftConstraintsMap @ps @(ni + DeclDims nv)
                         ni_val mapConstrs
          withKnownConstraints someCs $ \(composedProxy :: Proxy composedCs) ->
            -- 3. Fabricate IslImageSubsetD evidence via runtime ISL check.
            --    Check at the pctx-fused shape so the 'Dict' matches
            --    'Dep's new obligation (the lifted pctx prefix and
            --    'LitPrepend' wrappers are ISL-equivalent to the bare
            --    check under any valid parameter assignment).
            requireC
              (ImageOutOfBounds
                 "reindex: image-subset re-check failed (internal)"
                 srcStr dstStr)
              (islImageSubsetCheckSPctx @ps @pctx @ni @(DeclDims nv)
                 @composedCs
                 @dOuter
                 @(DeclDomTag nv)
                 mapStr Proxy Proxy Proxy)
              $
              -- 4. Get Lookup evidence for Var in the new decl list.
              withReplaceDecl @ps @target @name @nv @decls
                (Proxy @target) pn
                (internalError "reindex: symbolVal/sameSymbol mismatch")
                (Right (Dep composedProxy (Var pn)))
    go (Dep mapP inner) = Dep mapP <$> go inner

    go (Reduce rop projP body) = Reduce rop projP <$> go body
    go (Case branches)     = Case <$> goBranches branches
    go (Restrict inner)    = Restrict <$> go inner

    goBranches :: forall n' (amb :: DomTag ps n') (bdoms :: [DomTag ps n']) a'.
                  Branches ps pctx decls n' amb bdoms a'
               -> Proof (Branches ps pctx (ReplaceDecl target nv decls) n' amb bdoms a')
    goBranches BNil = Right BNil
    goBranches (BCons pd body rest) =
      BCons pd <$> go body <*> goBranches rest

-- | Rewrite a @Dep mapP (Var \@target)@ node after reindexing.
--
-- ═══════════════════════════════════════════════════════════════════════
-- §3. Expression walker (target equation body)
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk the target equation's body.  Both @decls@ and ambient
-- dims\/domain change.  Only 'Const'\/'Pw'\/'PMap' are currently
-- supported.
walkExprTarget
  :: forall (target :: Symbol) (ps :: [Symbol])
            (pctx :: [TConstraint ps 0])
            (decls :: [VarDecl ps]) (nv :: VarDecl ps)
            oldN (oldD :: DomTag ps oldN)
            newN (newD :: DomTag ps newN)
            a.
     KnownSymbol target
  => Expr ps pctx decls oldN oldD a
  -> Either TransformError (Expr ps pctx (ReplaceDecl target nv decls) newN newD a)
walkExprTarget = go
  where
    targetName = symbolVal (Proxy @target)

    go :: forall oN (oD :: DomTag ps oN) nN (nD :: DomTag ps nN) a'.
          Expr ps pctx decls oN oD a'
       -> Either TransformError (Expr ps pctx (ReplaceDecl target nv decls) nN nD a')
    go (Const k)   = Right (Const k)
    go (Pw op a b) = Pw op <$> go a <*> go b
    go (PMap op a) = PMap op <$> go a
    go _ = Left (ImageOutOfBounds
      ("target body contains Var/Dep/Reduce/Case — "
       ++ "reindex v1 handles Const/Pw/PMap only")
      targetName targetName)


-- ═══════════════════════════════════════════════════════════════════════
-- §4. EqList walker
-- ═══════════════════════════════════════════════════════════════════════

walkEqList
  :: forall (target :: Symbol) (ps :: [Symbol])
            (pctx :: [TConstraint ps 0])
            (decls :: [VarDecl ps]) (nv :: VarDecl ps)
            (mapExprs :: [TExpr ps (DeclDims nv)])
            defined.
     ( KnownSymbol target
     , KnownDom ps (DeclDims nv) (DeclDomTag nv)
     , KnownNat (DeclDims nv)
     , KnownExprs ps (DeclDims nv) mapExprs
     , DeclType nv ~ DeclType (Lookup target decls)
     , KnownSymbols ps, KnownNat (Length ps)
     )
  => EqList ps pctx decls defined
  -> Either TransformError (EqList ps pctx (ReplaceDecl target nv decls) defined)
walkEqList EqNil = Right EqNil
walkEqList (eq :& rest) =
  (:&) <$> walkEq @target @ps @pctx @decls @nv @mapExprs eq
       <*> walkEqList @target @ps @pctx @decls @nv @mapExprs rest

walkEq
  :: forall (target :: Symbol) (ps :: [Symbol])
            (pctx :: [TConstraint ps 0])
            (decls :: [VarDecl ps]) (nv :: VarDecl ps)
            (mapExprs :: [TExpr ps (DeclDims nv)])
            (name :: Symbol).
     ( KnownSymbol target, KnownSymbol name
     , KnownDom ps (DeclDims nv) (DeclDomTag nv)
     , KnownNat (DeclDims nv)
     , KnownExprs ps (DeclDims nv) mapExprs
     , DeclType nv ~ DeclType (Lookup target decls)
     , KnownSymbols ps, KnownNat (Length ps)
     )
  => Equation ps pctx decls name
  -> Either TransformError (Equation ps pctx (ReplaceDecl target nv decls) name)
walkEq (Defines pn body) =
  withReplaceDecl @ps @target @name @nv @decls
    (Proxy @target) pn
    (Defines pn <$> walkExprNonTarget @target @ps @pctx @decls @nv @mapExprs body)
    (Defines pn <$>
       (walkExprTarget @target @ps @pctx @decls @nv body
         :: Proof (Expr ps pctx (ReplaceDecl target nv decls)
                       (DeclDims nv) (DeclDomTag nv) (DeclType nv))))


-- ═══════════════════════════════════════════════════════════════════════
-- §5. The reindex transform
-- ═══════════════════════════════════════════════════════════════════════

-- | Type synonym for the new VarDecl after reindexing.
type ReindexedVarDecl (ps :: [Symbol]) (target :: Symbol)
                      (newN :: Nat) (newDomCs :: [TConstraint ps newN])
                      (a :: Type) =
  'VarDecl @ps @target @newN @('Literal newDomCs) @a

reindex
  :: forall (target :: Symbol) ->
     forall (newN :: Nat) ->
     forall ps.
     forall (mapExprs :: [TExpr ps newN]) ->
     forall pctx inputs outputs locals
            (oldN :: Nat) (oldDomCs :: [TConstraint ps oldN])
            (a :: Type)
            (newDomCs :: [TConstraint ps newN]).
     ( KnownSymbol target
     , KnownSymbols ps
     , KnownNat newN, KnownNat oldN
     , KnownNat (Length ps)
     , Lookup target (inputs ++ outputs ++ locals)
         ~ 'VarDecl @ps @target @oldN @('Literal oldDomCs) @a
     , newDomCs ~ IslPreimageMultiAff ps newN oldN mapExprs oldDomCs
     , KnownDom ps newN ('Literal newDomCs)
     , KnownExprs ps newN mapExprs
     )
  => System ps pctx inputs outputs locals
  -> Either TransformError
            (System ps pctx inputs
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) outputs)
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) locals))
reindex (type target) (type newN) (type mapExprs) sys =
  case sys of
    System @_ @_ @_ @_ @_ @defined decls eqs ->
      reindexImpl @target @newN @_ @_ @_ @_ @_ @defined @_ @_ @_ @mapExprs @_ decls eqs

reindexImpl
  :: forall (target :: Symbol) (newN :: Nat)
            ps pctx inputs outputs locals (defined :: [Symbol])
            (oldN :: Nat) (oldDomCs :: [TConstraint ps oldN])
            (a :: Type)
            (mapExprs :: [TExpr ps newN])
            (newDomCs :: [TConstraint ps newN]).
     ( KnownSymbol target
     , KnownSymbols ps
     , KnownNat newN, KnownNat oldN
     , KnownNat (Length ps)
     , Lookup target (inputs ++ outputs ++ locals)
         ~ 'VarDecl @ps @target @oldN @('Literal oldDomCs) @a
     , newDomCs ~ IslPreimageMultiAff ps newN oldN mapExprs oldDomCs
     , KnownDom ps newN ('Literal newDomCs)
     , KnownExprs ps newN mapExprs
     , DefinesAllExactlyOnce (outputs ++ locals) defined
     , IslNonEmpty ps 0 pctx
     , KnownConstraints ps 0 pctx
     )
  => Decls ps inputs outputs locals
  -> EqList ps pctx (inputs ++ (outputs ++ locals)) defined
  -> Either TransformError
            (System ps pctx inputs
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) outputs)
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) locals))
reindexImpl decls eqs =
  withDefinesAllReplace @target @ps
    @(ReindexedVarDecl ps target newN newDomCs a)
    @outputs @locals @defined $
  withReplaceDeclConcat @target @ps
    @(ReindexedVarDecl ps target newN newDomCs a)
    @inputs @outputs @locals $
  let newOutputs = replaceDeclList @target @ps
                     @(ReindexedVarDecl ps target newN newDomCs a)
                     @outputs (dOutputs decls)
      newLocals  = replaceDeclList @target @ps
                     @(ReindexedVarDecl ps target newN newDomCs a)
                     @locals (dLocals decls)
      newDecls   = Decls (dInputs decls) newOutputs newLocals
  in System newDecls
       <$> walkEqList @target @ps @pctx
             @(inputs ++ (outputs ++ locals))
             @(ReindexedVarDecl ps target newN newDomCs a)
             @mapExprs eqs
