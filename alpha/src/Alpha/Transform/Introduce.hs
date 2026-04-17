{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Variable introduction transform: insert a local proxy variable
-- that copies an input through a reindex map, and redirect all
-- accesses to the input through the proxy.
--
-- Zero 'unsafeCoerce' in the walker — all phantom-type changes live
-- in "Alpha.Core.Lemmas" and the ISL seal modules.
module Alpha.Transform.Introduce
  ( introduce
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits
  ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal, sameSymbol, type (+) )

import Alpha.Core
import Alpha.Core.Lemmas
  ( withDefinesAllIntroduce, introduceDecls, introduceEqList
  , withIntroduce, withIntroduceDecl )
import Alpha.Transform.Types
  (TransformError(..), requireJust, requireC, internalError)
import Alpha.Transform.Walk (composeAccess)
import Isl.Typed.Params (KnownSymbols, Length)
import Isl.TypeLevel.Constraint
  (TConstraint, IslMultiAffToMap, IslPreimageMultiAff, IslNonEmpty, LiftPctxN)
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Reflection
  ( Append
  , DomTag(..)
  , IslImageSubsetD
  , KnownDom
  , LitPrepend
  , domToString
  , islImageSubsetCheckSPctx
  )
import Isl.TypeLevel.Sing
  ( KnownExprs(..)
  , liftConstraintsMap, withKnownConstraints
  )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

type IntroducedVarDecl ps proxy newN newDomCs a =
  'VarDecl @ps @proxy @newN @('Literal newDomCs) @a

introduce
  :: forall (source :: Symbol) (proxy :: Symbol) (newN :: Nat)
            (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (mapExprs :: [TExpr ps newN])
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps])
            (sourceN :: Nat) (sourceDomCs :: [TConstraint ps sourceN])
            (a :: Type) (newDomCs :: [TConstraint ps newN]).
     ( KnownSymbol source, KnownSymbol proxy
     , KnownNat sourceN, KnownNat newN
     , KnownSymbols ps, KnownNat (Length ps)
     , Lookup source (inputs ++ (outputs ++ locals))
         ~ 'VarDecl @ps @source @sourceN @('Literal sourceDomCs) @a
     , newDomCs ~ IslPreimageMultiAff ps newN sourceN mapExprs sourceDomCs
     , KnownDom ps sourceN ('Literal sourceDomCs)
     , KnownDom ps newN ('Literal newDomCs)
     , KnownExprs ps newN mapExprs
     , KnownConstraints ps (newN + sourceN)
         (IslMultiAffToMap ps newN sourceN mapExprs)
     , IslImageSubsetD ps newN sourceN
         (Append (LiftPctxN (newN + sourceN) pctx)
            (IslMultiAffToMap ps newN sourceN mapExprs))
         (LitPrepend (LiftPctxN newN pctx) ('Literal newDomCs))
         (LitPrepend (LiftPctxN sourceN pctx) ('Literal sourceDomCs))
     )
  => System ps pctx inputs outputs locals
  -> Either TransformError
            (System ps pctx inputs outputs
               (IntroducedVarDecl ps proxy newN newDomCs a ': locals))
introduce sys = case sys of
  System @_ @_ @_ @_ @_ @defined decls eqs ->
    introduceImpl @source @proxy @newN @ps @pctx @mapExprs
      @inputs @outputs @locals @defined
      @sourceN @sourceDomCs @a @newDomCs decls eqs

-- See the D32 staging: 'introduce' inherits its @pctx@ from the input
-- System's phantom; the plugin obligation in 'introduceImpl' fuses
-- pctx into the image-subset check automatically.


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Implementation
-- ═══════════════════════════════════════════════════════════════════════

type NewDecls ps inputs outputs locals proxy newN newDomCs a =
  inputs ++ (outputs ++ (IntroducedVarDecl ps proxy newN newDomCs a ': locals))

type OldDecls ps inputs outputs locals =
  inputs ++ (outputs ++ locals)

introduceImpl
  :: forall (source :: Symbol) (proxy :: Symbol) (newN :: Nat)
            (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (mapExprs :: [TExpr ps newN])
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) (defined :: [Symbol])
            (sourceN :: Nat) (sourceDomCs :: [TConstraint ps sourceN])
            (a :: Type) (newDomCs :: [TConstraint ps newN]).
     ( KnownSymbol source, KnownSymbol proxy
     , KnownNat sourceN, KnownNat newN
     , KnownSymbols ps, KnownNat (Length ps)
     , Lookup source (OldDecls ps inputs outputs locals)
         ~ 'VarDecl @ps @source @sourceN @('Literal sourceDomCs) @a
     , newDomCs ~ IslPreimageMultiAff ps newN sourceN mapExprs sourceDomCs
     , KnownDom ps sourceN ('Literal sourceDomCs)
     , KnownDom ps newN ('Literal newDomCs)
     , KnownExprs ps newN mapExprs
     , KnownConstraints ps (newN + sourceN)
         (IslMultiAffToMap ps newN sourceN mapExprs)
     , IslImageSubsetD ps newN sourceN
         (Append (LiftPctxN (newN + sourceN) pctx)
            (IslMultiAffToMap ps newN sourceN mapExprs))
         (LitPrepend (LiftPctxN newN pctx) ('Literal newDomCs))
         (LitPrepend (LiftPctxN sourceN pctx) ('Literal sourceDomCs))
     , DefinesAllExactlyOnce (outputs ++ locals) defined
     , IslNonEmpty ps 0 pctx
     )
  => Decls ps inputs outputs locals
  -> EqList ps pctx (OldDecls ps inputs outputs locals) defined
  -> Either TransformError
            (System ps pctx inputs outputs
               (IntroducedVarDecl ps proxy newN newDomCs a ': locals))
introduceImpl decls eqs
  -- Freshness check: proxy must not collide with any existing variable
  | proxyName `elem` allNames =
      Left (ImageOutOfBounds
        ("introduce: proxy name " ++ show proxyName
         ++ " collides with existing declaration")
        proxyName proxyName)
  | otherwise =
      withDefinesAllIntroduce @ps
        @(IntroducedVarDecl ps proxy newN newDomCs a)
        @proxy @outputs @locals @defined $
      withIntroduceDecl @ps @proxy @source
        @(IntroducedVarDecl ps proxy newN newDomCs a)
        @(OldDecls ps inputs outputs locals)
        @(NewDecls ps inputs outputs locals proxy newN newDomCs a)
        (Proxy @proxy) (Proxy @source)
        (withIntroduce @ps @proxy
           @(IntroducedVarDecl ps proxy newN newDomCs a)
           @inputs @outputs @locals
           (let proxyDecl = MkDecl
                  :: Decl ps (IntroducedVarDecl ps proxy newN newDomCs a)
                newDecls = introduceDecls proxyDecl decls

                sourceVar :: Expr ps pctx (NewDecls ps inputs outputs locals proxy newN newDomCs a)
                                 sourceN ('Literal sourceDomCs) a
                sourceVar = Var (Proxy @source)

                copyBody :: Expr ps pctx (NewDecls ps inputs outputs locals proxy newN newDomCs a)
                                newN ('Literal newDomCs) a
                copyBody = Dep (Proxy @(IslMultiAffToMap ps newN sourceN mapExprs))
                               sourceVar

                copyEq :: Equation ps pctx (NewDecls ps inputs outputs locals proxy newN newDomCs a) proxy
                copyEq = Defines (Proxy @proxy) copyBody

                transportedEqs = introduceEqList @ps @pctx
                  @(IntroducedVarDecl ps proxy newN newDomCs a)
                  @inputs @outputs @locals eqs

            in (\rewrittenEqs -> System newDecls (copyEq :& rewrittenEqs))
               <$> walkEqListIntroduce
                     @source @proxy @ps @pctx
                     @(OldDecls ps inputs outputs locals)
                     @(NewDecls ps inputs outputs locals proxy newN newDomCs a)
                     @(IntroducedVarDecl ps proxy newN newDomCs a)
                     @newN @newDomCs @mapExprs
                     transportedEqs))
        (Left (ImageOutOfBounds
          "introduce: source == proxy (names must differ)"
          proxyName proxyName))
  where
    proxyName = symbolVal (Proxy @proxy)
    allNames = declListNames (dInputs decls)
            ++ declListNames (dOutputs decls)
            ++ declListNames (dLocals decls)

declListNames :: forall ps ds. DeclList ps ds -> [String]
declListNames Nil = []
declListNames ((MkDecl :: Decl ps d) :> rest) =
  symbolVal (Proxy @(DeclName d)) : declListNames rest


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Expression walker (zero unsafeCoerce)
-- ═══════════════════════════════════════════════════════════════════════

walkExprIntroduce
  :: forall (source :: Symbol) (proxy :: Symbol) (ps :: [Symbol])
            (pctx :: [TConstraint ps 0])
            (oldDecls :: [VarDecl ps]) (newDecls :: [VarDecl ps])
            (newVD :: VarDecl ps)
            (newNo :: Nat) (newDomCs :: [TConstraint ps newNo])
            (mapExprs :: [TExpr ps newNo])
            n (d :: DomTag ps n) a.
     ( KnownSymbol source, KnownSymbol proxy
     , KnownNat newNo
     , KnownDom ps newNo ('Literal newDomCs)
     , KnownExprs ps newNo mapExprs
     , KnownSymbols ps, KnownNat (Length ps)
     , newVD ~ IntroducedVarDecl ps proxy newNo newDomCs
               (DeclType (Lookup source oldDecls))
     )
  => Expr ps pctx newDecls n d a
  -> Either TransformError (Expr ps pctx newDecls n d a)
walkExprIntroduce = go
  where
    sourceName = symbolVal (Proxy @source)

    go :: forall n' (d' :: DomTag ps n') a'.
          Expr ps pctx newDecls n' d' a'
       -> Either TransformError (Expr ps pctx newDecls n' d' a')

    go (Const k)   = Right (Const k)
    go (Pw op a b) = Pw op <$> go a <*> go b
    go (PMap op a) = PMap op <$> go a

    go (Var @_ pn) = Right (Var pn)

    -- Dep targeting source variable: compose maps, redirect to proxy.
    -- Three evidence steps:
    --   1. sameSymbol @name @source → name ~ source
    --   2. lookupIntroduceDecl @proxy @source → Lookup source newDecls ~ Lookup source oldDecls
    --   3. lookupIntroduceDecl @proxy @proxy → Lookup proxy newDecls ~ newVD
    -- Chain: DeclType (Lookup name newDecls) = DeclType (Lookup source newDecls)
    --        = DeclType (Lookup source oldDecls) = DeclType newVD (from walker constraint)
    go (Dep @_ @_ @_ @ni @no @mapCs @dOuter @_dInner @_ _mapP
           (Var @name @_ @_ @_ @_ pn))
      | symbolVal pn == sourceName =
          case sameSymbol (Proxy @name) (Proxy @source) of
            Nothing -> internalError "introduce: symbolVal/sameSymbol mismatch"
            Just Refl ->
              -- (1) name ~ source (from the Refl above)
              -- (2) Lookup source newDecls ~ Lookup source oldDecls
              withIntroduceDecl @ps @proxy @source
                @newVD @oldDecls @newDecls
                (Proxy @proxy) (Proxy @source)
                (do
                   let ni_val = fromIntegral (natVal (Proxy @ni)) :: Int
                       srcStr = domToString @ps @ni @dOuter
                       dstStr = domToString @ps @newNo @('Literal newDomCs)
                   (mapStr, mapConstrs) <-
                     requireJust
                       (ImageOutOfBounds
                          "introduce: dep access out of bounds after map composition"
                          srcStr dstStr)
                       (composeAccess @ps @ni @no @newNo @mapCs @mapExprs
                          @dOuter @('Literal newDomCs)
                          (Proxy @dOuter) (Proxy @('Literal newDomCs)))
                   let someCs = liftConstraintsMap @ps @(ni + newNo)
                                  ni_val mapConstrs
                   withKnownConstraints someCs $
                     \(composedProxy :: Proxy composedCs) ->
                       requireC
                         (ImageOutOfBounds
                            "introduce: image-subset re-check failed"
                            srcStr dstStr)
                         (islImageSubsetCheckSPctx @ps @pctx @ni @newNo
                            @composedCs @dOuter @('Literal newDomCs)
                            mapStr Proxy Proxy Proxy)
                         $
                         -- (3) Lookup proxy newDecls ~ newVD
                         withIntroduceDecl @ps @proxy @proxy
                           @newVD @oldDecls @newDecls
                           (Proxy @proxy) (Proxy @proxy)
                           (internalError "introduce: impossible — sameSymbol reflexivity")
                           (Right (Dep composedProxy (Var (Proxy @proxy)))))
                (internalError "introduce: source == proxy (names must differ)")

    go (Dep mapP inner) = Dep mapP <$> go inner
    go (Reduce rop projP body) = Reduce rop projP <$> go body
    go (Case branches)     = Case <$> goBranches branches

    goBranches :: forall n' (amb :: DomTag ps n') (bdoms :: [DomTag ps n']) a'.
                  Branches ps pctx newDecls n' amb bdoms a'
               -> Either TransformError (Branches ps pctx newDecls n' amb bdoms a')
    goBranches BNil = Right BNil
    goBranches (BCons pd body rest) =
      BCons pd <$> go body <*> goBranches rest


-- ═══════════════════════════════════════════════════════════════════════
-- §4. EqList walker
-- ═══════════════════════════════════════════════════════════════════════

walkEqListIntroduce
  :: forall (source :: Symbol) (proxy :: Symbol) (ps :: [Symbol])
            (pctx :: [TConstraint ps 0])
            (oldDecls :: [VarDecl ps]) (newDecls :: [VarDecl ps])
            (newVD :: VarDecl ps)
            (newNo :: Nat) (newDomCs :: [TConstraint ps newNo])
            (mapExprs :: [TExpr ps newNo])
            defined.
     ( KnownSymbol source, KnownSymbol proxy
     , KnownNat newNo
     , KnownDom ps newNo ('Literal newDomCs)
     , KnownExprs ps newNo mapExprs
     , newVD ~ IntroducedVarDecl ps proxy newNo newDomCs
               (DeclType (Lookup source oldDecls))
     , KnownSymbols ps, KnownNat (Length ps)
     )
  => EqList ps pctx newDecls defined
  -> Either TransformError (EqList ps pctx newDecls defined)
walkEqListIntroduce EqNil = Right EqNil
walkEqListIntroduce (Defines pn body :& rest) =
  (:&) <$> (Defines pn <$>
              walkExprIntroduce @source @proxy @ps @pctx @oldDecls @newDecls
                @newVD @newNo @newDomCs @mapExprs body)
       <*> walkEqListIntroduce @source @proxy @ps @pctx @oldDecls @newDecls
             @newVD @newNo @newDomCs @mapExprs rest
