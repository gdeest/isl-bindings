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
-- live in the trusted seal modules ("Alpha.Core", "Isl.TypeLevel.Sing",
-- "Isl.TypeLevel.Reflection").
module Alpha.Transform.Reindex
  ( reindex
  , TransformError(..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits
  ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal, type (+) )
import System.IO.Unsafe (unsafePerformIO)

import Alpha.Core
import Alpha.Core.Lemmas
  (lookupReplaceDecl, replaceDeclList, definesAllReplace, replaceDeclConcat)
import Alpha.Transform.Types (TransformError(..))
import Alpha.Transform.Walk (composeAccess)
import Isl.Typed.Constraints (MapIx, Constraint)
import Isl.Typed.Params (KnownSymbols(..), Length)
import Isl.TypeLevel.Constraint
  ( IslPreimageMultiAff, TConstraint )
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Reflection
  ( Dict(..)
  , DomTag(..)
  , KnownDom
  , reflectDomString
  , islImageSubsetCheckS
  )
import Isl.TypeLevel.Sing
  ( KnownConstraints(..), KnownExprs(..)
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
  => Expr ps decls n d a
  -> Either TransformError (Expr ps (ReplaceDecl target nv decls) n d a)
walkExprNonTarget = go
  where
    targetName = symbolVal (Proxy @target)

    go :: forall n' (d' :: DomTag ps n') a'.
          Expr ps decls n' d' a'
       -> Either TransformError (Expr ps (ReplaceDecl target nv decls) n' d' a')

    go (Const k)   = Right (Const k)
    go (Pw op a b) = Pw op <$> go a <*> go b
    go (PMap op a) = PMap op <$> go a

    go (Var @name pn) =
      case lookupReplaceDecl @ps @target @name @nv @decls
             (Proxy @target) pn of
        Right Dict -> Right (Var pn)
        Left _     -> Left (ImageOutOfBounds
          ("naked Var reference to reindexed variable " ++ targetName
           ++ " (must be wrapped in Dep)")
          (symbolVal pn) targetName)

    -- Dep targeting the reindexed variable: compose map + verify at runtime.
    -- Constructive: builds a fresh Dep(Var) with the composed map's
    -- type-level constraints, verified by ISL.  No unsafeCoerce in user code —
    -- all trust lives in Sing.hs (withKnownConstraints) and Reflection.hs
    -- (islImageSubsetCheckS).
    go (Dep @_ @_ @ni @no @mapCs @dOuter @_dInner @_ _mapP (Var @name @_ @_ @_ pn))
      | symbolVal pn == targetName =
          let srcStr = reflectDomString @ps @ni @dOuter
              dstStr = reflectDomString @ps @(DeclDims nv) @(DeclDomTag nv)
              ni_val = fromIntegral (natVal (Proxy @ni))
          in case composeAccess @ps @ni @no @(DeclDims nv)
                    @mapCs @mapExprs srcStr dstStr of
               Nothing -> Left (ImageOutOfBounds
                 "reindex: dep access out of bounds after map composition"
                 srcStr dstStr)
               Just (mapStr, mapConstrs) ->
                 -- 1. Fabricate KnownConstraints for the composed map (CPS)
                 let someCs = liftConstraintsMap @ps @(ni + DeclDims nv)
                                ni_val mapConstrs
                 in withKnownConstraints someCs $ \(composedProxy :: Proxy composedCs) ->
                   -- 2. Fabricate IslImageSubsetD evidence via runtime ISL check
                   case islImageSubsetCheckS @ps @ni @(DeclDims nv) @composedCs
                          mapStr (Proxy @dOuter) (Proxy @(DeclDomTag nv)) of
                     Nothing -> Left (ImageOutOfBounds
                       "reindex: image-subset re-check failed (internal)"
                       srcStr dstStr)
                     Just Dict ->
                       -- 3. Get Lookup evidence for Var in the new decl list
                       case lookupReplaceDecl @ps @target @name @nv @decls
                              (Proxy @target) pn of
                         Left (Refl, Dict) ->
                           -- name ~ target: build Dep with composed map + new Var
                           Right (Dep composedProxy (Var pn))
                         Right _ ->
                           -- Impossible: symbolVal matched but sameSymbol didn't.
                           -- (sameSymbol is consistent with symbolVal.)
                           error "reindex: impossible — symbolVal/sameSymbol mismatch"
    go (Dep mapP inner) = Dep mapP <$> go inner

    go (Reduce rop projP body) = Reduce rop projP <$> go body
    go (Case branches)     = Case <$> goBranches branches

    goBranches :: forall n' (amb :: DomTag ps n') (bdoms :: [DomTag ps n']) a'.
                  Branches ps decls n' amb bdoms a'
               -> Either TransformError (Branches ps (ReplaceDecl target nv decls) n' amb bdoms a')
    goBranches BNil = Right BNil
    goBranches (BCons pd body rest) =
      BCons pd <$> go body <*> goBranches rest

-- | Rewrite a @Dep mapP (Var \@target)@ node after reindexing.
--
-- ═══════════════════════════════════════════════════════════════════════
-- §3. Expression walker (target equation body)
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk the target equation's body.  Both @decls@ and ambient
-- dims\/domain change.  Phase 2: only 'Const'/'Pw'/'PMap' supported.
walkExprTarget
  :: forall (target :: Symbol) (ps :: [Symbol])
            (decls :: [VarDecl ps]) (nv :: VarDecl ps)
            oldN (oldD :: DomTag ps oldN)
            newN (newD :: DomTag ps newN)
            a.
     KnownSymbol target
  => Expr ps decls oldN oldD a
  -> Either TransformError (Expr ps (ReplaceDecl target nv decls) newN newD a)
walkExprTarget = go
  where
    targetName = symbolVal (Proxy @target)

    go :: forall oN (oD :: DomTag ps oN) nN (nD :: DomTag ps nN) a'.
          Expr ps decls oN oD a'
       -> Either TransformError (Expr ps (ReplaceDecl target nv decls) nN nD a')
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
  => EqList ps decls defined
  -> Either TransformError (EqList ps (ReplaceDecl target nv decls) defined)
walkEqList EqNil = Right EqNil
walkEqList (eq :& rest) =
  (:&) <$> walkEq @target @ps @decls @nv @mapExprs eq
       <*> walkEqList @target @ps @decls @nv @mapExprs rest

walkEq
  :: forall (target :: Symbol) (ps :: [Symbol])
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
  => Equation ps decls name
  -> Either TransformError (Equation ps (ReplaceDecl target nv decls) name)
walkEq (Defines pn body) =
  case lookupReplaceDecl @ps @target @name @nv @decls
         (Proxy @target) pn of
    Left (Refl, Dict) ->
      Defines pn <$>
        (walkExprTarget @target @ps @decls @nv body
          :: Either TransformError
               (Expr ps (ReplaceDecl target nv decls)
                    (DeclDims nv) (DeclDomTag nv) (DeclType nv)))
    Right Dict ->
      Defines pn <$> walkExprNonTarget @target @ps @decls @nv @mapExprs body


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
     forall inputs outputs locals
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
  => System ps inputs outputs locals
  -> Either TransformError
            (System ps inputs
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) outputs)
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) locals))
reindex (type target) (type newN) (type mapExprs) sys =
  case sys of
    MkSystem @_ @_ @_ @_ @defined _ decls eqs ->
      reindexImpl @target @newN @_ @_ @_ @_ @defined @_ @_ @_ @mapExprs @_ decls eqs

reindexImpl
  :: forall (target :: Symbol) (newN :: Nat)
            ps inputs outputs locals (defined :: [Symbol])
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
     )
  => Decls ps inputs outputs locals
  -> EqList ps (inputs ++ (outputs ++ locals)) defined
  -> Either TransformError
            (System ps inputs
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) outputs)
               (ReplaceDecl target (ReindexedVarDecl ps target newN newDomCs a) locals))
reindexImpl decls eqs =
  case definesAllReplace @target @ps
         @(ReindexedVarDecl ps target newN newDomCs a)
         @outputs @locals @defined of
    Dict ->
      case replaceDeclConcat @target @ps
             @(ReindexedVarDecl ps target newN newDomCs a)
             @inputs @outputs @locals of
        Refl ->
          let newOutputs = replaceDeclList @target @ps
                             @(ReindexedVarDecl ps target newN newDomCs a)
                             @outputs (dOutputs decls)
              newLocals  = replaceDeclList @target @ps
                             @(ReindexedVarDecl ps target newN newDomCs a)
                             @locals (dLocals decls)
              newDecls   = Decls (dInputs decls) newOutputs newLocals
          in case walkEqList @target @ps
                    @(inputs ++ (outputs ++ locals))
                    @(ReindexedVarDecl ps target newN newDomCs a)
                    @mapExprs eqs of
               Left err -> Left err
               Right newEqs -> Right (MkSystem () newDecls newEqs)
