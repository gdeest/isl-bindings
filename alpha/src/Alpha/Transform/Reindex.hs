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
-- The walker uses 'lookupReplaceDecl' from "Alpha.Core" to justify
-- each 'Var' and 'Equation' node.  'Dep' nodes referencing the
-- target variable are reconstructed constructively: the old dep map
-- is composed with the reverse of the reindex map via ISL, the
-- composed map's constraints are lifted to type-level singletons
-- ('withKnownConstraints'), and the image-subset obligation is
-- verified at runtime ('islImageSubsetCheckS').
--
-- Zero 'unsafeCoerce' in the walker — all phantom-type changes
-- live in the trusted seal modules:
--
--   * "Alpha.Core" ('lookupReplaceDecl', 'replaceDeclList',
--     'definesAllReplace')
--   * "Isl.TypeLevel.Sing" ('withKnownConstraints', 'liftConstraintsMap')
--   * "Isl.TypeLevel.Reflection" ('islImageSubsetCheckS')
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
import Alpha.Transform.Types (TransformError(..))
import Isl.Typed.Constraints (Conjunction(..), MapIx, Constraint)
import qualified Isl.Typed.Constraints as TC
import Isl.Monad (runIslT, Ur(..))
import Isl.Linear (query_, queryM_, urWrap, Both(..))
import qualified Isl.Linear as Isl
import Isl.Typed.Params (KnownSymbols(..), Length)
import qualified Isl.Types as Isl
import qualified Isl.Map as RawM
import qualified Isl.Set as RawS
import qualified Isl.BasicMap as RawBM
import qualified Isl.MultiAff as RawMA
import Isl.TypeLevel.Constraint
  ( IslPreimageMultiAff, TConstraint )
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Reflection
  ( Dict(..)
  , DomTag(..)
  , KnownDom
  , reflectDomString
  , IslImageSubsetD
  , islImageSubsetCheckS
  )
import Isl.TypeLevel.Sing
  ( KnownConstraints(..), KnownExprs(..), SBasicMap(..), SMultiAff(..)
  , evalSBasicMap, evalSMultiAff
  , liftConstraintsMap, withKnownConstraints
  )


-- Axioms for ReplaceDecl (replaceDeclList, definesAllReplace,
-- replaceDeclConcat, lookupReplaceDecl) live in Alpha.Core.


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Runtime ISL verification (pure, via unsafePerformIO)
-- ═══════════════════════════════════════════════════════════════════════

-- | Compose a reindexed Dep's map and verify its image-subset obligation.
--
-- Given:
--   * Old dep map (ni → oldNo) as KnownConstraints singleton
--   * Reindex multi-aff (newNo → oldNo) as KnownExprs singleton
--   * Source domain (ni-dim) as ISL string (from reflectDomString)
--   * Destination domain (newNo-dim) as ISL string
--
-- Computes:
--   1. Reverse the reindex multi-aff's map: oldNo → newNo
--   2. Compose: old dep map ; reversed reindex = ni → newNo
--   3. Decompose the composed map to extract its constraints and ISL string
--   4. Image of source domain under composed map
--   5. Check image ⊆ destination domain
--
-- Returns @Just (mapStr, constraints)@ on success (the composed map's ISL
-- string and constraint list), or @Nothing@ if the image-subset check fails.
--
-- All map operations are direct ISL object operations — no string
-- intermediaries for maps.  Domains come as strings from
-- reflectDomString (the existing KnownDom API).
composeDepReindex
  :: forall ps ni oldNo newNo
            (mapCs :: [TConstraint ps (ni + oldNo)])
            (mapExprs :: [TExpr ps newNo]).
     ( KnownNat ni, KnownNat oldNo, KnownNat newNo
     , KnownSymbols ps, KnownNat (Length ps)
     , KnownConstraints ps (ni + oldNo) mapCs
     , KnownExprs ps newNo mapExprs
     )
  => String  -- ^ source domain ISL text (ni-dim)
  -> String  -- ^ destination domain ISL text (newNo-dim)
  -> Maybe (String, [Constraint MapIx])
composeDepReindex srcStr dstStr = unsafePerformIO $ runIslT $ Isl.do
  -- 1. Build old dep map from KnownConstraints singleton
  oldMap <- evalSBasicMap @ps @ni @oldNo
              (MkSBasicMap (knownConstraints @ps @(ni + oldNo) @mapCs))
  -- 2. Build reindex multi-aff, convert to map, reverse
  reindexMA <- evalSMultiAff @ps @newNo @oldNo
                 (MkSMultiAff (knownExprs @ps @newNo @mapExprs))
  fwdMap    <- RawM.fromMultiAff reindexMA
  revMap    <- RawM.reverse fwdMap
  -- 3. Compose: old dep (ni → oldNo) ; reverse reindex (oldNo → newNo)
  composed  <- RawM.applyRange oldMap revMap
  -- 4. Extract the composed map's string and constraints
  let nParams = fromIntegral (natVal (Proxy @(Length ps)))
      nIn = fromIntegral (natVal (Proxy @ni))
      nNewNo = fromIntegral (natVal (Proxy @newNo))
  let !(composed1, composed2) = Isl.dup composed
  Ur (composedStr, constrs) <- queryM_ composed1 (\composedRef -> Isl.do
    let !str = RawM.toStr composedRef
    conjs <- TC.decomposeMap nIn nNewNo nParams composedRef
    Ur cs <- urWrap conjs
    let constrs' = case cs of
          [Conjunction c] -> c
          _ -> error "composeDepReindex: non-basic composed map"
    Isl.pure (Ur (str, constrs')))
  -- 5. Apply composed map to source domain, check image ⊆ destination
  src <- RawS.readFromStr srcStr
  img <- RawS.apply src composed2
  dst <- RawS.readFromStr dstStr
  Ur b <- queryM_ img (\imgRef ->
    Isl.query_ dst (\dstRef -> RawS.isSubset imgRef dstRef))
  Isl.pure (Ur (if b then Just (composedStr, constrs) else Nothing))


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Expression walker (non-target equations)
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

    go (Const k)  = Right (Const k)
    go (Pw f a b) = Pw f <$> go a <*> go b
    go (PMap f a) = PMap f <$> go a

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
          in case composeDepReindex @ps @ni @no @(DeclDims nv)
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

    go (Reduce projP body) = Reduce projP <$> go body
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
    go (Const k)  = Right (Const k)
    go (Pw f a b) = Pw f <$> go a <*> go b
    go (PMap f a) = PMap f <$> go a
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
walkEq @target @ps @decls @nv @mapExprs (Defines @name pn body) =
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
reindexImpl @target @newN @ps @inputs @outputs @locals @defined
            @oldN @oldDomCs @a @mapExprs @newDomCs decls eqs =
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
