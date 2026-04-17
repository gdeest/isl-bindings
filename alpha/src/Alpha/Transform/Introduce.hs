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
  ( definesAllIntroduce, introduceDecls, introduceEqList
  , lookupIntroduce, lookupIntroduceDecl )
import Alpha.Transform.Types (TransformError(..))
import Alpha.Transform.Walk (composeAccess)
import Isl.Typed.Constraints (MapIx, Constraint)
import Isl.Typed.Params (KnownSymbols, Length)
import Isl.TypeLevel.Constraint (TConstraint, IslMultiAffToMap, IslPreimageMultiAff)
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Reflection
  ( Dict(..)
  , DomTag(..)
  , IslImageSubsetD
  , KnownDom
  , reflectDomString
  , islImageSubsetCheckS
  )
import Isl.TypeLevel.Sing
  ( KnownConstraints(..), KnownExprs(..)
  , liftConstraintsMap, withKnownConstraints
  )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

type IntroducedVarDecl ps proxy newN newDomCs a =
  'VarDecl @ps @proxy @newN @('Literal newDomCs) @a

introduce
  :: forall (source :: Symbol) (proxy :: Symbol) (newN :: Nat)
            (ps :: [Symbol]) (mapExprs :: [TExpr ps newN])
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
         (IslMultiAffToMap ps newN sourceN mapExprs)
         ('Literal newDomCs) ('Literal sourceDomCs)
     )
  => System ps inputs outputs locals
  -> Either TransformError
            (System ps inputs outputs
               (IntroducedVarDecl ps proxy newN newDomCs a ': locals))
introduce sys = case sys of
  System @_ @_ @_ @_ @defined decls eqs ->
    introduceImpl @source @proxy @newN @ps @mapExprs
      @inputs @outputs @locals @defined
      @sourceN @sourceDomCs @a @newDomCs decls eqs


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Implementation
-- ═══════════════════════════════════════════════════════════════════════

type NewDecls ps inputs outputs locals proxy newN newDomCs a =
  inputs ++ (outputs ++ (IntroducedVarDecl ps proxy newN newDomCs a ': locals))

type OldDecls ps inputs outputs locals =
  inputs ++ (outputs ++ locals)

introduceImpl
  :: forall (source :: Symbol) (proxy :: Symbol) (newN :: Nat)
            (ps :: [Symbol]) (mapExprs :: [TExpr ps newN])
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
         (IslMultiAffToMap ps newN sourceN mapExprs)
         ('Literal newDomCs) ('Literal sourceDomCs)
     , DefinesAllExactlyOnce (outputs ++ locals) defined
     )
  => Decls ps inputs outputs locals
  -> EqList ps (OldDecls ps inputs outputs locals) defined
  -> Either TransformError
            (System ps inputs outputs
               (IntroducedVarDecl ps proxy newN newDomCs a ': locals))
introduceImpl decls eqs
  -- Freshness check: proxy must not collide with any existing variable
  | proxyName `elem` allNames =
      Left (ImageOutOfBounds
        ("introduce: proxy name " ++ show proxyName
         ++ " collides with existing declaration")
        proxyName proxyName)
  | otherwise =
  case definesAllIntroduce @ps
         @(IntroducedVarDecl ps proxy newN newDomCs a)
         @proxy @outputs @locals @defined of
    Dict ->
      case lookupIntroduceDecl @ps @proxy @source
             @(IntroducedVarDecl ps proxy newN newDomCs a)
             @(OldDecls ps inputs outputs locals)
             @(NewDecls ps inputs outputs locals proxy newN newDomCs a)
             (Proxy @proxy) (Proxy @source) of
        Right Dict ->
          case lookupIntroduce @ps @proxy
                 @(IntroducedVarDecl ps proxy newN newDomCs a)
                 @inputs @outputs @locals of
            Dict ->
              let proxyDecl = MkDecl
                    :: Decl ps (IntroducedVarDecl ps proxy newN newDomCs a)
                  newDecls = introduceDecls proxyDecl decls

                  sourceVar :: Expr ps (NewDecls ps inputs outputs locals proxy newN newDomCs a)
                                   sourceN ('Literal sourceDomCs) a
                  sourceVar = Var (Proxy @source)

                  copyBody :: Expr ps (NewDecls ps inputs outputs locals proxy newN newDomCs a)
                                  newN ('Literal newDomCs) a
                  copyBody = Dep (Proxy @(IslMultiAffToMap ps newN sourceN mapExprs))
                                 sourceVar

                  copyEq :: Equation ps (NewDecls ps inputs outputs locals proxy newN newDomCs a) proxy
                  copyEq = Defines (Proxy @proxy) copyBody

                  transportedEqs = introduceEqList @ps
                    @(IntroducedVarDecl ps proxy newN newDomCs a)
                    @inputs @outputs @locals eqs

              in case walkEqListIntroduce
                       @source @proxy @ps
                       @(OldDecls ps inputs outputs locals)
                       @(NewDecls ps inputs outputs locals proxy newN newDomCs a)
                       @(IntroducedVarDecl ps proxy newN newDomCs a)
                       @newN @newDomCs @mapExprs
                       transportedEqs of
                   Left err -> Left err
                   Right rewrittenEqs ->
                     Right (System newDecls (copyEq :& rewrittenEqs))

        Left _ -> Left (ImageOutOfBounds
          "introduce: source == proxy (names must differ)"
          proxyName proxyName)
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
  => Expr ps newDecls n d a
  -> Either TransformError (Expr ps newDecls n d a)
walkExprIntroduce = go
  where
    sourceName = symbolVal (Proxy @source)
    dstStr = reflectDomString @ps @newNo @('Literal newDomCs)

    go :: forall n' (d' :: DomTag ps n') a'.
          Expr ps newDecls n' d' a'
       -> Either TransformError (Expr ps newDecls n' d' a')

    go (Const k)   = Right (Const k)
    go (Pw op a b) = Pw op <$> go a <*> go b
    go (PMap op a) = PMap op <$> go a

    go (Var @name pn) = Right (Var pn)

    -- Dep targeting source variable: compose maps, redirect to proxy.
    -- Three evidence steps:
    --   1. sameSymbol @name @source → name ~ source
    --   2. lookupIntroduceDecl @proxy @source → Lookup source newDecls ~ Lookup source oldDecls
    --   3. lookupIntroduceDecl @proxy @proxy → Lookup proxy newDecls ~ newVD
    -- Chain: DeclType (Lookup name newDecls) = DeclType (Lookup source newDecls)
    --        = DeclType (Lookup source oldDecls) = DeclType newVD (from walker constraint)
    go (Dep @_ @_ @ni @no @mapCs @dOuter @_dInner @_ _mapP
           (Var @name @_ @_ @_ pn))
      | symbolVal pn == sourceName =
          case sameSymbol (Proxy @name) (Proxy @source) of
            Nothing -> error "introduce: symbolVal/sameSymbol mismatch"
            Just Refl ->
              -- (1) name ~ source
              case lookupIntroduceDecl @ps @proxy @source
                     @newVD @oldDecls @newDecls
                     (Proxy @proxy) (Proxy @source) of
                Left _ -> error "introduce: source == proxy (names must differ)"
                Right Dict ->
                  -- (2) Lookup source newDecls ~ Lookup source oldDecls
                  let srcStr = reflectDomString @ps @ni @dOuter
                      ni_val = fromIntegral (natVal (Proxy @ni))
                  in case composeAccess @ps @ni @no @newNo @mapCs @mapExprs
                            srcStr dstStr of
                       Nothing -> Left (ImageOutOfBounds
                         "introduce: dep access out of bounds after map composition"
                         srcStr dstStr)
                       Just (mapStr, mapConstrs) ->
                         let someCs = liftConstraintsMap @ps @(ni + newNo)
                                        ni_val mapConstrs
                         in withKnownConstraints someCs $
                              \(composedProxy :: Proxy composedCs) ->
                           case islImageSubsetCheckS @ps @ni @newNo @composedCs
                                  mapStr (Proxy @dOuter) (Proxy @('Literal newDomCs)) of
                             Nothing -> Left (ImageOutOfBounds
                               "introduce: image-subset re-check failed"
                               srcStr dstStr)
                             Just Dict ->
                               -- (3) Lookup proxy newDecls ~ newVD
                               case lookupIntroduceDecl @ps @proxy @proxy
                                      @newVD @oldDecls @newDecls
                                      (Proxy @proxy) (Proxy @proxy) of
                                 Left (Refl, Dict) ->
                                   Right (Dep composedProxy (Var (Proxy @proxy)))
                                 Right _ ->
                                   error "introduce: impossible — sameSymbol reflexivity"

    go (Dep mapP inner) = Dep mapP <$> go inner
    go (Reduce rop projP body) = Reduce rop projP <$> go body
    go (Case branches)     = Case <$> goBranches branches

    goBranches :: forall n' (amb :: DomTag ps n') (bdoms :: [DomTag ps n']) a'.
                  Branches ps newDecls n' amb bdoms a'
               -> Either TransformError (Branches ps newDecls n' amb bdoms a')
    goBranches BNil = Right BNil
    goBranches (BCons pd body rest) =
      BCons pd <$> go body <*> goBranches rest


-- ═══════════════════════════════════════════════════════════════════════
-- §4. EqList walker
-- ═══════════════════════════════════════════════════════════════════════

walkEqListIntroduce
  :: forall (source :: Symbol) (proxy :: Symbol) (ps :: [Symbol])
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
  => EqList ps newDecls defined
  -> Either TransformError (EqList ps newDecls defined)
walkEqListIntroduce EqNil = Right EqNil
walkEqListIntroduce (Defines pn body :& rest) =
  (:&) <$> (Defines pn <$>
              walkExprIntroduce @source @proxy @ps @oldDecls @newDecls
                @newVD @newNo @newDomCs @mapExprs body)
       <*> walkEqListIntroduce @source @proxy @ps @oldDecls @newDecls
             @newVD @newNo @newDomCs @mapExprs rest
