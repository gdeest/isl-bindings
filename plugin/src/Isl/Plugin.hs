{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | GHC typechecker plugin for compile-time polyhedral reasoning.
--
-- Activate with:
--
-- @
-- {\-# OPTIONS_GHC -fplugin=Isl.Plugin #-\}
-- @
--
-- The plugin intercepts proof obligations expressed as class constraints
-- from "Isl.TypeLevel.Constraint" ('IslSubset', 'IslNonEmpty', etc.) and
-- solves them at compile time by building ISL objects programmatically
-- and calling the ISL C library.
module Isl.Plugin (plugin) where

import GHC.Plugins
  ( Plugin(..), defaultPlugin, purePlugin
  , TyCon, getOccString
  , mkModuleName, mkOccName, tcName, dataName
  , tyConDataCons
  )
import GHC.Tc.Plugin
  ( TcPluginM
  , tcPluginTrace, tcPluginIO
  , findImportedModule, lookupOrig
  , tcLookupClass, tcLookupDataCon
  )
import GHC.Tc.Types
  ( TcPlugin(..), TcPluginSolveResult(..) )
import GHC.Tc.Types.Constraint (Ct, CtEvidence, ctPred, ctEvidence, ctEvPred)
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm, evDataConApp )
import GHC.Core.Class (Class, classTyCon)
import GHC.Core.Predicate (classifyPredType, Pred(..))
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Core.Type (splitTyConApp_maybe)
import GHC.Core.DataCon (promoteDataCon)
import GHC.Types.Unique.FM (emptyUFM)
import GHC.Unit.Finder (FindResult(..))
import GHC.Unit.Module (Module)
import GHC.Types.PkgQual (PkgQual(NoPkgQual))
import GHC.Utils.Outputable (text, (<+>))
import GHC.Data.FastString (unpackFS)

import Data.Reflection (give)
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)

import qualified Isl.Types as Isl
import qualified Isl.BasicSet.AutoGen as BS
import qualified Isl.Set.AutoGen as S
import qualified Isl.Constraint.AutoGen as C
import qualified Isl.LocalSpace.AutoGen as LS
import qualified Isl.Space.AutoGen as Space
import Isl.HighLevel.Constraints
  ( Expr(..), Constraint(..), SetIx(..), expandExpr )

-- * Plugin entry point

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just islTcPlugin
  , pluginRecompile = purePlugin
  }

-- * Plugin environment

data IslPluginEnv = IslPluginEnv
  { -- ISL context, kept alive for the plugin's lifetime
    envCtxPtr     :: !(Isl.Ctx)
    -- Proof obligation classes
  , envSubsetClass   :: !Class
  , envNonEmptyClass :: !Class
  , envEqualClass    :: !Class
    -- Promoted data constructors for walking type-level constraint trees
  , envTEq    :: !TyCon
  , envTGe    :: !TyCon
  , envTDim   :: !TyCon
  , envTParam :: !TyCon
  , envTConst :: !TyCon
  , envTAdd   :: !TyCon
  , envTMul   :: !TyCon
  , envPos    :: !TyCon
  , envNeg    :: !TyCon
  }

islTcPlugin :: TcPlugin
islTcPlugin = TcPlugin
  { tcPluginInit    = initPlugin
  , tcPluginSolve   = solveIsl
  , tcPluginRewrite = \_ -> emptyUFM
  , tcPluginStop    = stopPlugin
  }

-- * Initialization

initPlugin :: TcPluginM IslPluginEnv
initPlugin = do
  tcPluginTrace "isl-plugin" (text "Initializing ISL typechecker plugin")

  -- Allocate ISL context
  ctxPtr <- tcPluginIO Isl.c_ctx_alloc
  let envCtxPtr = Isl.Ctx ctxPtr

  -- Find our modules
  constraintMod <- resolveModule "Isl.TypeLevel.Constraint"
  exprMod       <- resolveModule "Isl.TypeLevel.Expr"

  -- Look up proof obligation classes
  envSubsetClass   <- lookupClass constraintMod "IslSubset"
  envNonEmptyClass <- lookupClass constraintMod "IslNonEmpty"
  envEqualClass    <- lookupClass constraintMod "IslEqual"

  -- Look up promoted data constructors for type-level expressions
  envTEq    <- lookupPromDC constraintMod "TEq"
  envTGe    <- lookupPromDC constraintMod "TGe"
  envTDim   <- lookupPromDC exprMod "TDim"
  envTParam <- lookupPromDC exprMod "TParam"
  envTConst <- lookupPromDC exprMod "TConst"
  envTAdd   <- lookupPromDC exprMod "TAdd"
  envTMul   <- lookupPromDC exprMod "TMul"
  envPos    <- lookupPromDC exprMod "Pos"
  envNeg    <- lookupPromDC exprMod "Neg"

  tcPluginTrace "isl-plugin" (text "Plugin initialized — ISL context allocated")
  pure IslPluginEnv{..}

stopPlugin :: IslPluginEnv -> TcPluginM ()
stopPlugin env = do
  let Isl.Ctx ptr = envCtxPtr env
  tcPluginIO $ Isl.c_ctx_free ptr
  tcPluginTrace "isl-plugin" (text "ISL context freed")

-- * Module / name resolution helpers

resolveModule :: String -> TcPluginM Module
resolveModule name = do
  result <- findImportedModule (mkModuleName name) NoPkgQual
  case result of
    Found _ m -> pure m
    _         -> error $ "isl-plugin: could not find module " ++ name

lookupClass :: Module -> String -> TcPluginM Class
lookupClass md name = do
  n <- lookupOrig md (mkOccName tcName name)
  tcLookupClass n

lookupPromDC :: Module -> String -> TcPluginM TyCon
lookupPromDC md name = do
  n <- lookupOrig md (mkOccName dataName name)
  dc <- tcLookupDataCon n
  pure (promoteDataCon dc)

-- * Solver dispatch

solveIsl :: IslPluginEnv -> EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult
solveIsl env _evBinds _givens wanteds = do
  let classified = mapMaybe (classifyWanted env) wanteds
  if null classified
    then pure $ TcPluginOk [] []
    else do
      results <- mapM (solveOne env) classified
      let solved = [(ev, ct) | Just (ev, ct) <- results]
      pure $ TcPluginOk solved []

-- * Wanted constraint classification

-- | Parsed form of an IslSubset / IslNonEmpty / IslEqual wanted constraint,
-- with the raw GHC types for ps, n, and the constraint lists.
data IslWanted
  = WantedSubset   !Ct !Type !Type !Type !Type   -- ps, n, cs1, cs2
  | WantedNonEmpty !Ct !Type !Type !Type          -- ps, n, cs
  | WantedEqual    !Ct !Type !Type !Type !Type    -- ps, n, cs1, cs2

classifyWanted :: IslPluginEnv -> Ct -> Maybe IslWanted
classifyWanted IslPluginEnv{..} ct =
  case classifyPredType (ctPred ct) of
    ClassPred cls args
      | cls == envSubsetClass,   [ps, n, cs1, cs2] <- args ->
          Just $ WantedSubset ct ps n cs1 cs2
      | cls == envNonEmptyClass, [ps, n, cs] <- args ->
          Just $ WantedNonEmpty ct ps n cs
      | cls == envEqualClass,    [ps, n, cs1, cs2] <- args ->
          Just $ WantedEqual ct ps n cs1 cs2
    _ -> Nothing

-- * Solving individual constraints

solveOne :: IslPluginEnv -> IslWanted -> TcPluginM (Maybe (EvTerm, Ct))
solveOne env = \case
  WantedSubset ct psTy nTy cs1Ty cs2Ty ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let mcs1 = mapM (reifyTConstraint env paramNames) (unfoldTypeList cs1Ty)
          mcs2 = mapM (reifyTConstraint env paramNames) (unfoldTypeList cs2Ty)
      case (mcs1, mcs2) of
        (Just cs1, Just cs2) -> do
          result <- tcPluginIO $ pure $
            islCheckSubset (envCtxPtr env) (length paramNames) nDims paramNames cs1 cs2
          traceResult "Subset" result
          if result then Just . (, ct) <$> makeEvidence ct else pure Nothing
        _ -> traceReifyFail >> pure Nothing

  WantedNonEmpty ct psTy nTy csTy ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let mcs = mapM (reifyTConstraint env paramNames) (unfoldTypeList csTy)
      case mcs of
        Just cs -> do
          result <- tcPluginIO $ pure $
            islCheckNonEmpty (envCtxPtr env) (length paramNames) nDims paramNames cs
          traceResult "Non-empty" result
          if result then Just . (, ct) <$> makeEvidence ct else pure Nothing
        _ -> traceReifyFail >> pure Nothing

  WantedEqual ct psTy nTy cs1Ty cs2Ty ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let mcs1 = mapM (reifyTConstraint env paramNames) (unfoldTypeList cs1Ty)
          mcs2 = mapM (reifyTConstraint env paramNames) (unfoldTypeList cs2Ty)
      case (mcs1, mcs2) of
        (Just cs1, Just cs2) -> do
          result <- tcPluginIO $ pure $
            islCheckEqual (envCtxPtr env) (length paramNames) nDims paramNames cs1 cs2
          traceResult "Equal" result
          if result then Just . (, ct) <$> makeEvidence ct else pure Nothing
        _ -> traceReifyFail >> pure Nothing

-- | Extract param names and nDims from GHC types, then call continuation.
withReified :: IslPluginEnv -> Type -> Type -> ([String] -> Int -> TcPluginM a) -> TcPluginM a
withReified _env psTy nTy k =
  let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
      nDims = extractNat nTy
  in case nDims of
    Just nd -> k paramNames nd
    Nothing -> error "isl-plugin: could not extract dimension count from type"

traceResult :: String -> Bool -> TcPluginM ()
traceResult label result =
  tcPluginTrace "isl-plugin" $
    text label <+> text "→" <+> text (if result then "PROVED ✓" else "FAILED ✗")

traceReifyFail :: TcPluginM ()
traceReifyFail =
  tcPluginTrace "isl-plugin" (text "Could not reify type-level constraints (stuck type families?)")

-- * Evidence construction

makeEvidence :: Ct -> TcPluginM EvTerm
makeEvidence ct = do
  let pred_ = ctEvPred (ctEvidence ct)
  case classifyPredType pred_ of
    ClassPred cls args ->
      let [dc] = tyConDataCons (classTyCon cls)
      in pure $ evDataConApp dc args []
    _ -> error "isl-plugin: makeEvidence called on non-class predicate"

-- * ISL operations via the bindings

-- | Build an ISL BasicSet from value-level constraints, programmatically.
-- Same approach as 'Isl.HighLevel.BasicSet.toBasicSet' but using concrete
-- integer values instead of type-class constraints.
buildBasicSet
  :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> Isl.BasicSet
buildBasicSet ctx nParams nDims paramNames constraints =
  let space0 = give ctx $ Space.setAlloc (fromIntegral nParams) (fromIntegral nDims)
      space  = foldl (\sp (i, name) -> give ctx $ Space.setDimName sp Isl.islDimParam i name)
                     space0 (zip [0..] paramNames)
      univ   = give ctx $ BS.universe space
  in foldl (addOneConstraint ctx) univ constraints

addOneConstraint :: Isl.Ctx -> Isl.BasicSet -> Constraint SetIx -> Isl.BasicSet
addOneConstraint ctx bs constraint =
  let sp     = give ctx $ BS.getSpace bs
      ls     = give ctx $ LS.fromSpace sp
      (coeffs, constant) = case constraint of
        InequalityConstraint e -> expandExpr e
        EqualityConstraint e   -> expandExpr e
      emptyC = case constraint of
        InequalityConstraint _ -> give ctx $ C.inequalityAlloc ls
        EqualityConstraint _   -> give ctx $ C.equalityAlloc ls
      withCoeffs = foldl (setCoeff ctx) emptyC coeffs
      finalC     = give ctx $ C.setConstantSi withCoeffs (fromIntegral constant)
  in give ctx $ BS.addConstraint bs finalC

setCoeff :: Isl.Ctx -> Isl.Constraint -> (Integer, SetIx) -> Isl.Constraint
setCoeff ctx c (coeff, ix) =
  let (dimType, pos) = case ix of
        SetDim i   -> (Isl.islDimSet, i)
        SetParam i -> (Isl.islDimParam, i)
  in give ctx $ C.setCoefficientSi c dimType (fromIntegral pos) (fromIntegral coeff)

-- | Build a Set from constraints (BasicSet → Set promotion).
buildSet :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> Isl.Set
buildSet ctx nParams nDims paramNames constraints =
  give ctx $ S.fromBasicSet (buildBasicSet ctx nParams nDims paramNames constraints)

-- ISL property checks

islCheckSubset :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> [Constraint SetIx] -> Bool
islCheckSubset ctx nParams nDims paramNames cs1 cs2 =
  let s1 = buildSet ctx nParams nDims paramNames cs1
      s2 = buildSet ctx nParams nDims paramNames cs2
  in give ctx $ S.isSubset s1 s2

islCheckNonEmpty :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> Bool
islCheckNonEmpty ctx nParams nDims paramNames cs =
  let s = buildSet ctx nParams nDims paramNames cs
  in give ctx $ not (S.isEmpty s)

islCheckEqual :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> [Constraint SetIx] -> Bool
islCheckEqual ctx nParams nDims paramNames cs1 cs2 =
  let s1 = buildSet ctx nParams nDims paramNames cs1
      s2 = buildSet ctx nParams nDims paramNames cs2
  in give ctx $ S.isEqual s1 s2

-- * Reifying GHC types to value-level data

-- | Extract a Nat literal from a GHC Type.
extractNat :: Type -> Maybe Int
extractNat (LitTy (NumTyLit n)) = Just (fromIntegral n)
extractNat _                     = Nothing

-- | Extract a Symbol literal from a GHC Type.
extractSymbol :: Type -> Maybe String
extractSymbol (LitTy (StrTyLit fs)) = Just (unpackFS fs)
extractSymbol _                      = Nothing

-- | Reify a type-level TConstraint to a value-level Constraint SetIx.
-- The sugar operators (>=., <=., ==.) expand to TEq/TGe applied to
-- TAdd/TMul trees, so by the time we see them the constraints are
-- already in normalized (e ≥ 0 / e = 0) form.
reifyTConstraint :: IslPluginEnv -> [String] -> Type -> Maybe (Constraint SetIx)
reifyTConstraint env paramNames ty = case splitTyConApp_maybe ty of
  Just (tc, [expr])
    | tc == envTEq env -> EqualityConstraint   <$> reifyTExpr env paramNames expr
    | tc == envTGe env -> InequalityConstraint <$> reifyTExpr env paramNames expr
  _ -> Nothing

-- | Reify a type-level TExpr to a value-level Expr SetIx.
reifyTExpr :: IslPluginEnv -> [String] -> Type -> Maybe (Expr SetIx)
reifyTExpr env paramNames ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envTDim env, [nTy] <- args ->
        Ix . SetDim <$> extractNat nTy

    | tc == envTParam env, [sTy] <- args -> do
        name <- extractSymbol sTy
        idx  <- elemIndex name paramNames
        Just $ Ix (SetParam idx)

    | tc == envTConst env, [zTy] <- args ->
        Constant <$> reifyZ env zTy

    | tc == envTAdd env, [a, b] <- args ->
        Add <$> reifyTExpr env paramNames a <*> reifyTExpr env paramNames b

    | tc == envTMul env, [k, a] <- args ->
        Mul <$> reifyZ env k <*> reifyTExpr env paramNames a

  _ -> Nothing

-- | Reify a type-level Z (Pos n / Neg n) to an Integer.
reifyZ :: IslPluginEnv -> Type -> Maybe Integer
reifyZ env ty = case splitTyConApp_maybe ty of
  Just (tc, [nTy])
    | tc == envPos env -> fromIntegral <$> extractNat nTy
    | tc == envNeg env -> negate . fromIntegral <$> extractNat nTy
  _ -> Nothing

-- * Promoted list unfolding

-- | Unfold a promoted list @'[a, b, c]@ into its element types.
unfoldTypeList :: Type -> [Type]
unfoldTypeList ty = case splitTyConApp_maybe ty of
  Just (tc, [_k, x, xs])
    | getOccString tc == ":" -> x : unfoldTypeList xs
  Just (tc, _)
    | getOccString tc == "'[]" -> []
    | getOccString tc == "[]"  -> []
  _ -> []
