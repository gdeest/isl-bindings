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
-- from "Isl.TypeLevel.Constraint" ('IslSubset', 'IslNonEmpty', 'IslEqual',
-- 'IslMapSubset', 'IslMapEqual', 'IslRangeOf', 'IslImageSubset') and
-- solves them at compile time by building ISL objects programmatically
-- and calling the ISL C library.
module Isl.Plugin (plugin) where

import GHC.Plugins
  ( Plugin(..), defaultPlugin, purePlugin
  , TyCon, getOccString
  , mkModuleName, mkOccName, tcName, dataName
  , tyConDataCons
  , mkTyConApp, mkNumLitTy
  , Role(Nominal)
  )
import GHC.Tc.Plugin
  ( TcPluginM
  , tcPluginTrace, tcPluginIO
  , findImportedModule, lookupOrig
  , tcLookupClass, tcLookupDataCon, tcLookupTyCon
  )
import GHC.Tc.Types
  ( TcPlugin(..), TcPluginSolveResult(..)
  , TcPluginRewriter, TcPluginRewriteResult(..)
  , RewriteEnv
  )
import GHC.Tc.Types.Constraint (Ct, CtEvidence, ctPred, ctEvidence, ctEvPred)
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm, evDataConApp )
import GHC.Core.Class (Class, classTyCon)
import GHC.Core.Predicate (classifyPredType, Pred(..))
import GHC.Core.TyCo.Rep (Type(..), TyLit(..), UnivCoProvenance(..))
import GHC.Core.Type (splitTyConApp_maybe, mkStrLitTy)
import GHC.Core.Coercion (mkUnivCo)
import GHC.Core.Reduction (Reduction(..))
import GHC.Core.DataCon (promoteDataCon)
import GHC.Builtin.Types (promotedConsDataCon, promotedNilDataCon)
import GHC.Types.Unique.FM (UniqFM, listToUFM)
import GHC.Unit.Finder (FindResult(..))
import GHC.Unit.Module (Module)
import GHC.Types.PkgQual (PkgQual(NoPkgQual))
import GHC.Utils.Outputable (text, (<+>))
import GHC.Data.FastString (unpackFS, mkFastString)

import qualified Data.Reflection
import Data.Reflection (give)
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)
import System.IO.Unsafe (unsafePerformIO)

import qualified Isl.Types as Isl
import qualified Isl.BasicSet.AutoGen as BS
import qualified Isl.BasicMap.AutoGen as BM
import qualified Isl.Set.AutoGen as S
import qualified Isl.Map.AutoGen as M
import qualified Isl.Constraint.AutoGen as C
import qualified Isl.LocalSpace.AutoGen as LS
import qualified Isl.Space.AutoGen as Space
import Isl.HighLevel.Constraints
  ( Expr(..), Constraint(..), SetIx(..), MapIx(..), expandExpr )
import qualified Isl.Foreach as Foreach
import qualified Isl.Aff.AutoGen as Aff
import Isl.Instances ()

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
    -- Set proof obligation classes
  , envSubsetClass   :: !Class
  , envNonEmptyClass :: !Class
  , envEqualClass    :: !Class
    -- Map proof obligation classes
  , envMapSubsetClass   :: !Class
  , envMapEqualClass    :: !Class
  , envRangeOfClass     :: !Class
  , envImageSubsetClass :: !Class
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
  , envTFloorDiv :: !TyCon
    -- Bounded index wrappers
  , envMkIdx  :: !TyCon
  , envMkPIdx :: !TyCon
    -- Data type TyCons (for building lifted types)
  , envTConstraintTC :: !TyCon
    -- Type family TyCons (for rewriting)
  , envIslIntersectSet  :: !TyCon
  , envIslComplementSet :: !TyCon
  , envIslDifferenceSet :: !TyCon
  , envIslApply         :: !TyCon
  , envIslDomainTF      :: !TyCon
  , envIslRangeTF       :: !TyCon
  , envIslCompose       :: !TyCon
  , envIslReverseMap    :: !TyCon
  , envIslProjectOut    :: !TyCon
  , envIslFromString    :: !TyCon
  , envIslToString      :: !TyCon
  , envIslMapToString   :: !TyCon
  }

islTcPlugin :: TcPlugin
islTcPlugin = TcPlugin
  { tcPluginInit    = initPlugin
  , tcPluginSolve   = solveIsl
  , tcPluginRewrite = makeRewriters
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

  -- Look up set proof obligation classes
  envSubsetClass   <- lookupClass constraintMod "IslSubset"
  envNonEmptyClass <- lookupClass constraintMod "IslNonEmpty"
  envEqualClass    <- lookupClass constraintMod "IslEqual"

  -- Look up map proof obligation classes
  envMapSubsetClass   <- lookupClass constraintMod "IslMapSubset"
  envMapEqualClass    <- lookupClass constraintMod "IslMapEqual"
  envRangeOfClass     <- lookupClass constraintMod "IslRangeOf"
  envImageSubsetClass <- lookupClass constraintMod "IslImageSubset"

  -- Look up promoted data constructors for type-level expressions
  envTEq    <- lookupPromDC constraintMod "TEq"
  envTGe    <- lookupPromDC constraintMod "TGe"
  envTDim   <- lookupPromDC exprMod "TDim"
  envTParam <- lookupPromDC exprMod "TParam"
  envTConst <- lookupPromDC exprMod "TConst"
  envTAdd   <- lookupPromDC exprMod "TAdd"
  envTMul   <- lookupPromDC exprMod "TMul"
  envTFloorDiv <- lookupPromDC exprMod "TFloorDiv"
  envPos    <- lookupPromDC exprMod "Pos"
  envNeg    <- lookupPromDC exprMod "Neg"

  -- Bounded index wrappers
  envMkIdx  <- lookupPromDC exprMod "MkIdx"
  envMkPIdx <- lookupPromDC exprMod "MkPIdx"

  -- Data type TyCons (for building lifted types)
  envTConstraintTC <- lookupTF constraintMod "TConstraint"

  -- Type family TyCons (for rewriting)
  envIslIntersectSet  <- lookupTF constraintMod "IslIntersectSet"
  envIslComplementSet <- lookupTF constraintMod "IslComplementSet"
  envIslDifferenceSet <- lookupTF constraintMod "IslDifferenceSet"
  envIslApply         <- lookupTF constraintMod "IslApply"
  envIslDomainTF      <- lookupTF constraintMod "IslDomainTF"
  envIslRangeTF       <- lookupTF constraintMod "IslRangeTF"
  envIslCompose       <- lookupTF constraintMod "IslCompose"
  envIslReverseMap    <- lookupTF constraintMod "IslReverseMap"
  envIslProjectOut    <- lookupTF constraintMod "IslProjectOut"
  envIslFromString    <- lookupTF constraintMod "IslFromString"
  envIslToString      <- lookupTF constraintMod "IslToString"
  envIslMapToString   <- lookupTF constraintMod "IslMapToString"

  tcPluginTrace "isl-plugin" (text "Plugin initialized — ISL context allocated")
  pure IslPluginEnv{..}

stopPlugin :: IslPluginEnv -> TcPluginM ()
stopPlugin _env = do
  -- Intentionally NOT freeing the ISL context here.
  -- Objects built during compilation (in buildSet/buildMap) may still
  -- reference it. The OS reclaims all memory when GHC exits.
  -- Freeing it would trigger "isl_ctx not freed as objects still reference it".
  tcPluginTrace "isl-plugin" (text "Plugin stopped")

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

lookupTF :: Module -> String -> TcPluginM TyCon
lookupTF md name = do
  n <- lookupOrig md (mkOccName tcName name)
  tcLookupTyCon n

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

data IslWanted
  -- Set obligations
  = WantedSubset     !Ct !Type !Type !Type !Type         -- ps, n, cs1, cs2
  | WantedNonEmpty   !Ct !Type !Type !Type               -- ps, n, cs
  | WantedEqual      !Ct !Type !Type !Type !Type         -- ps, n, cs1, cs2
  -- Map obligations
  | WantedMapSubset  !Ct !Type !Type !Type !Type !Type   -- ps, ni, no, cs1, cs2
  | WantedMapEqual   !Ct !Type !Type !Type !Type !Type   -- ps, ni, no, cs1, cs2
  | WantedRangeOf    !Ct !Type !Type !Type !Type !Type   -- ps, ni, no, mapCs, rangeCs
  | WantedImageSubset !Ct !Type !Type !Type !Type !Type !Type -- ps, ni, no, mapCs, srcCs, dstCs

classifyWanted :: IslPluginEnv -> Ct -> Maybe IslWanted
classifyWanted IslPluginEnv{..} ct =
  case classifyPredType (ctPred ct) of
    ClassPred cls args
      -- Set obligations
      | cls == envSubsetClass, [ps, n, cs1, cs2] <- args ->
          Just $ WantedSubset ct ps n cs1 cs2
      | cls == envNonEmptyClass, [ps, n, cs] <- args ->
          Just $ WantedNonEmpty ct ps n cs
      | cls == envEqualClass, [ps, n, cs1, cs2] <- args ->
          Just $ WantedEqual ct ps n cs1 cs2
      -- Map obligations
      | cls == envMapSubsetClass, [ps, ni, no, cs1, cs2] <- args ->
          Just $ WantedMapSubset ct ps ni no cs1 cs2
      | cls == envMapEqualClass, [ps, ni, no, cs1, cs2] <- args ->
          Just $ WantedMapEqual ct ps ni no cs1 cs2
      | cls == envRangeOfClass, [ps, ni, no, mapCs, rangeCs] <- args ->
          Just $ WantedRangeOf ct ps ni no mapCs rangeCs
      | cls == envImageSubsetClass -> case args of
          [ps, ni, no, mapCs, srcCs, dstCs] ->
            Just $ WantedImageSubset ct ps ni no mapCs srcCs dstCs
          _ -> Nothing
    _ -> Nothing

-- * Solving individual constraints

solveOne :: IslPluginEnv -> IslWanted -> TcPluginM (Maybe (EvTerm, Ct))
solveOne env = \case

  -- === Set obligations ===

  WantedSubset ct psTy nTy cs1Ty cs2Ty ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let nParams = length paramNames
          mcs1 = reifyConstraintList env paramNames (unfoldTypeList cs1Ty)
          mcs2 = reifyConstraintList env paramNames (unfoldTypeList cs2Ty)
      case (mcs1, mcs2) of
        (Just cs1, Just cs2) -> do
          let ctx = envCtxPtr env
              s1 = buildSet ctx nParams nDims paramNames cs1
              s2 = buildSet ctx nParams nDims paramNames cs2
              result = give ctx $ S.isSubset s1 s2
          if result
            then traceProved "Subset" >> (Just . (, ct) <$> makeEvidence ct)
            else do
              traceFailed "Subset" $
                "\n  LHS: " ++ islSetToStr ctx s1 ++
                "\n  is NOT a subset of" ++
                "\n  RHS: " ++ islSetToStr ctx s2
              pure Nothing
        _ -> traceReifyFail >> pure Nothing

  WantedNonEmpty ct psTy nTy csTy ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let mcs = reifyConstraintList env paramNames (unfoldTypeList csTy)
      case mcs of
        Just cs -> do
          let ctx = envCtxPtr env
              s = buildSet ctx (length paramNames) nDims paramNames cs
              result = give ctx $ not (S.isEmpty s)
          if result
            then traceProved "NonEmpty" >> (Just . (, ct) <$> makeEvidence ct)
            else do
              traceFailed "NonEmpty" $
                "\n  Set is empty: " ++ islSetToStr ctx s
              pure Nothing
        _ -> traceReifyFail >> pure Nothing

  WantedEqual ct psTy nTy cs1Ty cs2Ty ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let nParams = length paramNames
          mcs1 = reifyConstraintList env paramNames (unfoldTypeList cs1Ty)
          mcs2 = reifyConstraintList env paramNames (unfoldTypeList cs2Ty)
      case (mcs1, mcs2) of
        (Just cs1, Just cs2) -> do
          let ctx = envCtxPtr env
              s1 = buildSet ctx nParams nDims paramNames cs1
              s2 = buildSet ctx nParams nDims paramNames cs2
              result = give ctx $ S.isEqual s1 s2
          if result
            then traceProved "Equal" >> (Just . (, ct) <$> makeEvidence ct)
            else do
              traceFailed "Equal" $
                "\n  LHS: " ++ islSetToStr ctx s1 ++
                "\n  is NOT equal to" ++
                "\n  RHS: " ++ islSetToStr ctx s2
              pure Nothing
        _ -> traceReifyFail >> pure Nothing

  -- === Map obligations ===

  WantedMapSubset ct psTy niTy noTy cs1Ty cs2Ty ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let nParams = length paramNames
          mcs1 = reifyMapConstraintList env paramNames nIn (unfoldTypeList cs1Ty)
          mcs2 = reifyMapConstraintList env paramNames nIn (unfoldTypeList cs2Ty)
      case (mcs1, mcs2) of
        (Just cs1, Just cs2) -> do
          let ctx = envCtxPtr env
              m1 = buildMap ctx nParams nIn nOut paramNames cs1
              m2 = buildMap ctx nParams nIn nOut paramNames cs2
              result = give ctx $ M.isSubset m1 m2
          if result
            then traceProved "MapSubset" >> (Just . (, ct) <$> makeEvidence ct)
            else do
              traceFailed "MapSubset" $
                "\n  LHS: " ++ islMapToStr ctx m1 ++
                "\n  is NOT a subset of" ++
                "\n  RHS: " ++ islMapToStr ctx m2
              pure Nothing
        _ -> traceReifyFail >> pure Nothing

  WantedMapEqual ct psTy niTy noTy cs1Ty cs2Ty ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let nParams = length paramNames
          mcs1 = reifyMapConstraintList env paramNames nIn (unfoldTypeList cs1Ty)
          mcs2 = reifyMapConstraintList env paramNames nIn (unfoldTypeList cs2Ty)
      case (mcs1, mcs2) of
        (Just cs1, Just cs2) -> do
          let ctx = envCtxPtr env
              m1 = buildMap ctx nParams nIn nOut paramNames cs1
              m2 = buildMap ctx nParams nIn nOut paramNames cs2
              result = give ctx $ M.isEqual m1 m2
          if result
            then traceProved "MapEqual" >> (Just . (, ct) <$> makeEvidence ct)
            else do
              traceFailed "MapEqual" $
                "\n  LHS: " ++ islMapToStr ctx m1 ++
                "\n  is NOT equal to" ++
                "\n  RHS: " ++ islMapToStr ctx m2
              pure Nothing
        _ -> traceReifyFail >> pure Nothing

  WantedRangeOf ct psTy niTy noTy mapCsTy rangeCsTy ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let nParams = length paramNames
          mMapCs   = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
          mRangeCs = reifyConstraintList env paramNames (unfoldTypeList rangeCsTy)
      case (mMapCs, mRangeCs) of
        (Just mapCs, Just rangeCs) -> do
          let ctx = envCtxPtr env
              m       = buildMap ctx nParams nIn nOut paramNames mapCs
              rng     = give ctx $ M.range m
              expected = buildSet ctx nParams nOut paramNames rangeCs
              result  = give ctx $ S.isEqual rng expected
          if result
            then traceProved "RangeOf" >> (Just . (, ct) <$> makeEvidence ct)
            else do
              traceFailed "RangeOf" $
                "\n  Map:      " ++ islMapToStr ctx m ++
                "\n  Range:    " ++ islSetToStr ctx rng ++
                "\n  Expected: " ++ islSetToStr ctx expected
              pure Nothing
        _ -> traceReifyFail >> pure Nothing

  WantedImageSubset ct psTy niTy noTy mapCsTy srcCsTy dstCsTy ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let nParams = length paramNames
          mMapCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
          mSrcCs = reifyConstraintList env paramNames (unfoldTypeList srcCsTy)
          mDstCs = reifyConstraintList env paramNames (unfoldTypeList dstCsTy)
      case (mMapCs, mSrcCs, mDstCs) of
        (Just mapCs, Just srcCs, Just dstCs) -> do
          let ctx = envCtxPtr env
              m     = buildMap ctx nParams nIn nOut paramNames mapCs
              src   = buildSet ctx nParams nIn paramNames srcCs
              image = give ctx $ S.apply src m
              dst   = buildSet ctx nParams nOut paramNames dstCs
              result = give ctx $ S.isSubset image dst
          if result
            then traceProved "ImageSubset" >> (Just . (, ct) <$> makeEvidence ct)
            else do
              traceFailed "ImageSubset" $
                "\n  Map:    " ++ islMapToStr ctx m ++
                "\n  Source: " ++ islSetToStr ctx src ++
                "\n  Image:  " ++ islSetToStr ctx image ++
                "\n  Target: " ++ islSetToStr ctx dst ++
                "\n  Image is NOT a subset of Target"
              pure Nothing
        _ -> traceReifyFail >> pure Nothing

-- * Reification helpers

-- | Extract param names and nDims for set obligations.
withReified :: IslPluginEnv -> Type -> Type -> ([String] -> Int -> TcPluginM a) -> TcPluginM a
withReified _env psTy nTy k =
  let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
  in case extractNat nTy of
    Just nd -> k paramNames nd
    Nothing -> error "isl-plugin: could not extract dimension count from type"

-- | Extract param names, nIn, and nOut for map obligations.
withReifiedMap :: IslPluginEnv -> Type -> Type -> Type -> ([String] -> Int -> Int -> TcPluginM a) -> TcPluginM a
withReifiedMap _env psTy niTy noTy k =
  let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
  in case (extractNat niTy, extractNat noTy) of
    (Just ni, Just no) -> k paramNames ni no
    _ -> error "isl-plugin: could not extract map dimensions from types"

-- | Reify a list of type-level constraints to set constraints.
reifyConstraintList :: IslPluginEnv -> [String] -> [Type] -> Maybe [Constraint SetIx]
reifyConstraintList env paramNames = mapM (reifyTConstraint env paramNames)

-- | Reify a list of type-level constraints to map constraints.
-- Dims 0..nIn-1 become InDim, nIn..nIn+nOut-1 become OutDim.
reifyMapConstraintList :: IslPluginEnv -> [String] -> Int -> [Type] -> Maybe [Constraint MapIx]
reifyMapConstraintList env paramNames nIn = mapM (reifyTConstraintMap env paramNames nIn)

traceProved :: String -> TcPluginM ()
traceProved label =
  tcPluginTrace "isl-plugin" $ text label <+> text "→ PROVED ✓"

traceFailed :: String -> String -> TcPluginM ()
traceFailed label detail =
  tcPluginTrace "isl-plugin" $ text label <+> text "→ FAILED ✗" <+> text detail

traceReifyFail :: TcPluginM ()
traceReifyFail =
  tcPluginTrace "isl-plugin" (text "Could not reify type-level constraints (stuck type families?)")

-- | Convert an ISL set to its string representation for diagnostics.
islSetToStr :: Isl.Ctx -> Isl.Set -> String
islSetToStr ctx s = give ctx $ S.toStr s

-- | Convert an ISL map to its string representation for diagnostics.
islMapToStr :: Isl.Ctx -> Isl.Map -> String
islMapToStr ctx m = give ctx $ M.toStr m

-- * Evidence construction

makeEvidence :: Ct -> TcPluginM EvTerm
makeEvidence ct = do
  let pred_ = ctEvPred (ctEvidence ct)
  case classifyPredType pred_ of
    ClassPred cls args ->
      let [dc] = tyConDataCons (classTyCon cls)
      in pure $ evDataConApp dc args []
    _ -> error "isl-plugin: makeEvidence called on non-class predicate"

-- * Type family rewriting

makeRewriters :: IslPluginEnv -> UniqFM TyCon TcPluginRewriter
makeRewriters env = listToUFM
  [ (envIslApply env,         rewriteApply env)
  , (envIslIntersectSet env,  rewriteIntersectSet env)
  , (envIslComplementSet env, rewriteComplementSet env)
  , (envIslDifferenceSet env, rewriteDifferenceSet env)
  , (envIslDomainTF env,      rewriteDomain env)
  , (envIslRangeTF env,       rewriteRange env)
  , (envIslCompose env,       rewriteCompose env)
  , (envIslReverseMap env,    rewriteReverseMap env)
  , (envIslProjectOut env,   rewriteProjectOut env)
  , (envIslFromString env,   rewriteFromString env)
  , (envIslToString env,     rewriteToString env)
  , (envIslMapToString env,  rewriteMapToString env)
  ]

-- | Helper: run a set computation and lift the result to a type.
-- @tfTyCon@ and @origArgs@ are used to reconstruct the original type family
-- application for the coercion: @co :: TF args ~ result@.
rewriteSetResult :: IslPluginEnv -> TyCon -> [Type]
  -> [String] -> Int -> Type -> Type
  -> (Isl.Ctx -> Isl.Set) -> TcPluginM TcPluginRewriteResult
rewriteSetResult env tfTyCon origArgs paramNames nDims psTy nTy mkSet = do
  result <- tcPluginIO $ pure $ decomposeIslSet env paramNames nDims (mkSet (envCtxPtr env))
  let resultTy = liftDisjunction env paramNames psTy nTy result
      origTy = mkTyConApp tfTyCon origArgs
      co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
  pure $ TcPluginRewriteTo (Reduction co resultTy) []

-- | Rewrite IslApply ps ni no mapCs setCs
rewriteApply :: IslPluginEnv -> TcPluginRewriter
rewriteApply env _rewriteEnv _givens args = case args of
  [psTy, niTy, noTy, mapCsTy, setCsTy] -> do
    let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
        mNi = extractNat niTy
        mNo = extractNat noTy
    case (mNi, mNo) of
      (Just nIn, Just nOut) -> do
        let mMapCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
            mSetCs = reifyConstraintList env paramNames (unfoldTypeList setCsTy)
        case (mMapCs, mSetCs) of
          (Just mapCs, Just setCs) -> do
            let nParams = length paramNames
            rewriteSetResult env (envIslApply env) args paramNames nOut psTy noTy $ \ctx ->
              let m   = buildMap ctx nParams nIn nOut paramNames mapCs
                  s   = buildSet ctx nParams nIn paramNames setCs
              in give ctx $ S.apply s m
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslIntersectSet ps n cs1 cs2
rewriteIntersectSet :: IslPluginEnv -> TcPluginRewriter
rewriteIntersectSet env _re _givens args = case args of
  [psTy, nTy, cs1Ty, cs2Ty] ->
    case extractNat nTy of
      Just nDims -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs1 = reifyConstraintList env paramNames (unfoldTypeList cs1Ty)
            mCs2 = reifyConstraintList env paramNames (unfoldTypeList cs2Ty)
        case (mCs1, mCs2) of
          (Just cs1, Just cs2) ->
            rewriteSetResult env (envIslIntersectSet env) args paramNames nDims psTy nTy $ \ctx ->
              let s1 = buildSet ctx (length paramNames) nDims paramNames cs1
                  s2 = buildSet ctx (length paramNames) nDims paramNames cs2
              in give ctx $ S.intersect s1 s2
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslComplementSet ps n cs
rewriteComplementSet :: IslPluginEnv -> TcPluginRewriter
rewriteComplementSet env _re _givens args = case args of
  [psTy, nTy, csTy] ->
    case extractNat nTy of
      Just nDims -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs = reifyConstraintList env paramNames (unfoldTypeList csTy)
        case mCs of
          Just cs ->
            rewriteSetResult env (envIslComplementSet env) args paramNames nDims psTy nTy $ \ctx ->
              let s = buildSet ctx (length paramNames) nDims paramNames cs
              in give ctx $ S.complement s
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslDifferenceSet ps n cs1 cs2
rewriteDifferenceSet :: IslPluginEnv -> TcPluginRewriter
rewriteDifferenceSet env _re _givens args = case args of
  [psTy, nTy, cs1Ty, cs2Ty] ->
    case extractNat nTy of
      Just nDims -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs1 = reifyConstraintList env paramNames (unfoldTypeList cs1Ty)
            mCs2 = reifyConstraintList env paramNames (unfoldTypeList cs2Ty)
        case (mCs1, mCs2) of
          (Just cs1, Just cs2) ->
            rewriteSetResult env (envIslDifferenceSet env) args paramNames nDims psTy nTy $ \ctx ->
              let s1 = buildSet ctx (length paramNames) nDims paramNames cs1
                  s2 = buildSet ctx (length paramNames) nDims paramNames cs2
              in give ctx $ S.subtract s1 s2
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslDomainTF ps ni no mapCs
rewriteDomain :: IslPluginEnv -> TcPluginRewriter
rewriteDomain env _re _givens args = case args of
  [psTy, niTy, _noTy, mapCsTy] ->
    case (extractNat niTy, extractNat _noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
        case mCs of
          Just cs ->
            rewriteSetResult env (envIslDomainTF env) args paramNames nIn psTy niTy $ \ctx ->
              let m = buildMap ctx (length paramNames) nIn nOut paramNames cs
              in give ctx $ M.domain m
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslRangeTF ps ni no mapCs
rewriteRange :: IslPluginEnv -> TcPluginRewriter
rewriteRange env _re _givens args = case args of
  [psTy, _niTy, noTy, mapCsTy] ->
    case (extractNat _niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
        case mCs of
          Just cs ->
            rewriteSetResult env (envIslRangeTF env) args paramNames nOut psTy noTy $ \ctx ->
              let m = buildMap ctx (length paramNames) nIn nOut paramNames cs
              in give ctx $ M.range m
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslCompose ps ni nk no m1Cs m2Cs
rewriteCompose :: IslPluginEnv -> TcPluginRewriter
rewriteCompose env _re _givens args = case args of
  [psTy, niTy, nkTy, noTy, m1CsTy, m2CsTy] ->
    case (extractNat niTy, extractNat nkTy, extractNat noTy) of
      (Just nIn, Just nK, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            nParams = length paramNames
            mM1Cs = reifyMapConstraintList env paramNames nK (unfoldTypeList m1CsTy)
            mM2Cs = reifyMapConstraintList env paramNames nIn (unfoldTypeList m2CsTy)
        case (mM1Cs, mM2Cs) of
          (Just m1Cs, Just m2Cs) -> do
            -- Result is in (nIn + nOut) space; use nIn for the dim split
            let resultNDims = nIn + nOut
                resultNTy = mkNumLitTy (fromIntegral resultNDims)
            result <- tcPluginIO $ pure $
              let m1 = buildMap (envCtxPtr env) nParams nK nOut paramNames m1Cs
                  m2 = buildMap (envCtxPtr env) nParams nIn nK paramNames m2Cs
                  composed = give (envCtxPtr env) $ M.applyRange m2 m1
              in decomposeIslMap env paramNames nIn nOut composed
            let combinedNTy = mkNumLitTy (fromIntegral (nIn + nOut))
                resultTy = liftMapDisjunction env paramNames nIn psTy combinedNTy result
                origTy = mkTyConApp (envIslCompose env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslReverseMap ps ni no mapCs
rewriteReverseMap :: IslPluginEnv -> TcPluginRewriter
rewriteReverseMap env _re _givens args = case args of
  [psTy, niTy, noTy, mapCsTy] ->
    case (extractNat niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
        case mCs of
          Just cs -> do
            result <- tcPluginIO $ pure $
              let m = buildMap (envCtxPtr env) (length paramNames) nIn nOut paramNames cs
                  rev = give (envCtxPtr env) $ M.reverse m
              in decomposeIslMap env paramNames nOut nIn rev
            let combinedNTy = mkNumLitTy (fromIntegral (nOut + nIn))
                resultTy = liftMapDisjunction env paramNames nOut psTy combinedNTy result
                origTy = mkTyConApp (envIslReverseMap env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslProjectOut ps n nResult first count cs
rewriteProjectOut :: IslPluginEnv -> TcPluginRewriter
rewriteProjectOut env _re _givens args = case args of
  [psTy, nTy, nResultTy, firstTy, countTy, csTy] ->
    case (extractNat nTy, extractNat nResultTy, extractNat firstTy, extractNat countTy) of
      (Just nDims, Just nResult, Just first, Just count) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs = reifyConstraintList env paramNames (unfoldTypeList csTy)
        case mCs of
          Just cs ->
            rewriteSetResult env (envIslProjectOut env) args paramNames nResult psTy nResultTy $ \ctx ->
              let s = buildSet ctx (length paramNames) nDims paramNames cs
              in give ctx $ S.projectOut s Isl.islDimSet (fromIntegral first) (fromIntegral count)
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslFromString ps n str
-- Parses an ISL set from its string representation.
rewriteFromString :: IslPluginEnv -> TcPluginRewriter
rewriteFromString env _re _givens args = case args of
  [psTy, nTy, strTy] ->
    case (extractNat nTy, extractSymbol strTy) of
      (Just nDims, Just str) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
        rewriteSetResult env (envIslFromString env) args paramNames nDims psTy nTy $ \ctx ->
          give ctx $ S.readFromStr str
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslToString ps n cs → Symbol
rewriteToString :: IslPluginEnv -> TcPluginRewriter
rewriteToString env _re _givens args = case args of
  [psTy, nTy, csTy] ->
    case extractNat nTy of
      Just nDims -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs = reifyConstraintList env paramNames (unfoldTypeList csTy)
        case mCs of
          Just cs -> do
            let ctx = envCtxPtr env
                s = buildSet ctx (length paramNames) nDims paramNames cs
                str = islSetToStr ctx s
                resultTy = mkStrLitTy (mkFastString str)
                origTy = mkTyConApp (envIslToString env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslMapToString ps ni no cs → Symbol
rewriteMapToString :: IslPluginEnv -> TcPluginRewriter
rewriteMapToString env _re _givens args = case args of
  [psTy, niTy, noTy, csTy] ->
    case (extractNat niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList csTy)
        case mCs of
          Just cs -> do
            let ctx = envCtxPtr env
                m = buildMap ctx (length paramNames) nIn nOut paramNames cs
                str = islMapToStr ctx m
                resultTy = mkStrLitTy (mkFastString str)
                origTy = mkTyConApp (envIslMapToString env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- * Decomposing ISL results back to value-level constraints

decomposeIslSet :: IslPluginEnv -> [String] -> Int -> Isl.Set -> [[Constraint SetIx]]
decomposeIslSet _env paramNames nDims set =
  let nParams = length paramNames
      ctx = envCtxPtr _env
      !(ref, _) = Isl.borrow set (\r -> r)
  in unsafePerformIO $ Foreach.setForeachBasicSet ref $ \bs -> do
       -- Get number of div dimensions (must use basicSetDim, not spaceDim)
       let !(bsRef, _) = Isl.borrow bs (\r -> r)
       nDivs <- Foreach.basicSetDim bsRef Isl.islDimDiv

       -- Extract div definitions: each div is floor(aff / d)
       let divExprs = [give ctx $ extractSetDiv bs nDims nParams i | i <- [0..nDivs-1]]

       constraints <- Foreach.basicSetForeachConstraint bsRef $ \c -> do
         isEq <- Foreach.constraintIsEquality c
         -- Regular dim + param coefficients
         coeffs <- mapM (\i -> do
           v <- Foreach.constraintGetCoefficientSi c Isl.islDimSet i
           pure (v, Ix (SetDim i))) [0..nDims-1]
         paramCoeffs <- mapM (\i -> do
           v <- Foreach.constraintGetCoefficientSi c Isl.islDimParam i
           pure (v, Ix (SetParam i))) [0..nParams-1]
         -- Div coefficients: substitute with FloorDiv expressions
         divCoeffs <- mapM (\i -> do
           v <- Foreach.constraintGetCoefficientSi c Isl.islDimDiv i
           pure (v, divExprs !! i)) [0..nDivs-1]
         constant <- Foreach.constraintGetConstantSi c
         Foreach.constraintFree c
         let allTerms = [(v, e) | (v, e) <- coeffs ++ paramCoeffs ++ divCoeffs, v /= 0]
             expr = rebuildExprWithDivs allTerms constant
         pure $ if isEq then EqualityConstraint expr else InequalityConstraint expr
       Foreach.basicSetFree bs
       pure constraints

-- | Extract the definition of a div dimension from a BasicSet as a FloorDiv Expr.
-- div_i = floor(aff / d) where aff is an affine expression over dims + params.
-- Must be called inside @give ctx@.
extractSetDiv :: Data.Reflection.Given Isl.Ctx => Isl.BasicSet -> Int -> Int -> Int -> Expr SetIx
extractSetDiv bs nDims nParams divIdx =
  let aff = BS.getDiv bs (fromIntegral divIdx)
      denom = unsafePerformIO $ Foreach.valGetNumSi $ Aff.getDenominatorVal aff
      -- ISL quirk: isl_aff from basic set uses isl_dim_in for set dimensions
      dimCoeffs = [(unsafePerformIO $ Foreach.valGetNumSi $ Aff.getCoefficientVal aff Isl.islDimIn (fromIntegral i), Ix (SetDim i)) | i <- [0..nDims-1]]
      paramCoeffs = [(unsafePerformIO $ Foreach.valGetNumSi $ Aff.getCoefficientVal aff Isl.islDimParam (fromIntegral i), Ix (SetParam i)) | i <- [0..nParams-1]]
      constVal = unsafePerformIO $ Foreach.valGetNumSi $ Aff.getConstantVal aff
      allTerms = [(v, e) | (v, e) <- dimCoeffs ++ paramCoeffs, v /= 0]
      innerExpr = rebuildExprWithDivs allTerms constVal
  in FloorDiv innerExpr denom

decomposeIslMap :: IslPluginEnv -> [String] -> Int -> Int -> Isl.Map -> [[Constraint MapIx]]
decomposeIslMap _env paramNames nIn nOut m =
  let nParams = length paramNames
      ctx = envCtxPtr _env
      !(ref, _) = Isl.borrow m (\r -> r)
  in unsafePerformIO $ Foreach.mapForeachBasicMap ref $ \bm -> do
       let !(bmRef, _) = Isl.borrow bm (\r -> r)
       nDivs <- Foreach.basicMapDim bmRef Isl.islDimDiv

       -- Extract div definitions for map space
       let divExprs = [give ctx $ extractMapDiv bm nIn nOut nParams i | i <- [0..nDivs-1]]

       constraints <- Foreach.basicMapForeachConstraint bmRef $ \c -> do
         isEq <- Foreach.constraintIsEquality c
         inCoeffs <- mapM (\i -> do
           v <- Foreach.constraintGetCoefficientSi c Isl.islDimIn i
           pure (v, Ix (InDim i))) [0..nIn-1]
         outCoeffs <- mapM (\i -> do
           v <- Foreach.constraintGetCoefficientSi c Isl.islDimOut i
           pure (v, Ix (OutDim i))) [0..nOut-1]
         paramCoeffs <- mapM (\i -> do
           v <- Foreach.constraintGetCoefficientSi c Isl.islDimParam i
           pure (v, Ix (MapParam i))) [0..nParams-1]
         divCoeffs <- mapM (\i -> do
           v <- Foreach.constraintGetCoefficientSi c Isl.islDimDiv i
           pure (v, divExprs !! i)) [0..nDivs-1]
         constant <- Foreach.constraintGetConstantSi c
         Foreach.constraintFree c
         let allTerms = [(v, e) | (v, e) <- inCoeffs ++ outCoeffs ++ paramCoeffs ++ divCoeffs, v /= 0]
             expr = rebuildExprWithDivs allTerms constant
         pure $ if isEq then EqualityConstraint expr else InequalityConstraint expr
       Foreach.basicMapFree bm
       pure constraints

-- | Extract a div definition from a BasicMap.
extractMapDiv :: Data.Reflection.Given Isl.Ctx => Isl.BasicMap -> Int -> Int -> Int -> Int -> Expr MapIx
extractMapDiv bm nIn nOut nParams divIdx =
  let aff = BM.getDiv bm (fromIntegral divIdx)
      denom = unsafePerformIO $ Foreach.valGetNumSi $ Aff.getDenominatorVal aff
      inCoeffs = [(unsafePerformIO $ Foreach.valGetNumSi $ Aff.getCoefficientVal aff Isl.islDimIn (fromIntegral i), Ix (InDim i)) | i <- [0..nIn-1]]
      outCoeffs = [(unsafePerformIO $ Foreach.valGetNumSi $ Aff.getCoefficientVal aff Isl.islDimOut (fromIntegral i), Ix (OutDim i)) | i <- [0..nOut-1]]
      paramCoeffs = [(unsafePerformIO $ Foreach.valGetNumSi $ Aff.getCoefficientVal aff Isl.islDimParam (fromIntegral i), Ix (MapParam i)) | i <- [0..nParams-1]]
      constVal = unsafePerformIO $ Foreach.valGetNumSi $ Aff.getConstantVal aff
      allTerms = [(v, e) | (v, e) <- inCoeffs ++ outCoeffs ++ paramCoeffs, v /= 0]
      innerExpr = rebuildExprWithDivs allTerms constVal
  in FloorDiv innerExpr denom

-- | Rebuild an Expr from (coefficient, expression) pairs and a constant.
-- Handles both simple variables (Ix) and complex expressions (FloorDiv).
rebuildExprWithDivs :: [(Integer, Expr ix)] -> Integer -> Expr ix
rebuildExprWithDivs coeffs constant =
  let terms = [if c == 1 then e else if c == -1 then Mul (-1) e else Mul c e
              | (c, e) <- coeffs]
      constTerm = [Constant constant | constant /= 0]
      allTerms = terms ++ constTerm
  in case allTerms of
    []     -> Constant 0
    [t]    -> t
    (t:ts) -> foldl Add t ts

-- * Lifting value-level constraints to GHC types

-- | Lift a disjunction (list of conjunctions) to a promoted list of lists type.
-- Result kind: [[TConstraint ps n]]
liftDisjunction :: IslPluginEnv -> [String] -> Type -> Type -> [[Constraint SetIx]] -> Type
liftDisjunction env paramNames psTy nTy css =
  let elemKind = mkTyConApp (envTConstraintTC env) [psTy, nTy]
      listKind = mkPromotedListTy elemKind []
  in mkPromotedListTy listKind
       [mkPromotedListTy elemKind
          (map (liftConstraint env paramNames psTy nTy) cs)
       | cs <- css]

-- | Lift a map disjunction. Map constraints use flat dim indexing (0..ni+no-1).
-- The nTy parameter should be the combined (ni+no) type.
liftMapDisjunction :: IslPluginEnv -> [String] -> Int -> Type -> Type -> [[Constraint MapIx]] -> Type
liftMapDisjunction env paramNames nIn psTy combinedNTy css =
  let elemKind = mkTyConApp (envTConstraintTC env) [psTy, combinedNTy]
      listKind = mkPromotedListTy elemKind []
  in mkPromotedListTy listKind
       [mkPromotedListTy elemKind
          (map (liftMapConstraint env paramNames nIn psTy combinedNTy) cs)
       | cs <- css]

-- | Lift a single set constraint to a type.
liftConstraint :: IslPluginEnv -> [String] -> Type -> Type -> Constraint SetIx -> Type
liftConstraint env pn psTy nTy (EqualityConstraint e) =
  mkTyConApp (envTEq env) [psTy, nTy, liftExpr env pn psTy nTy e]
liftConstraint env pn psTy nTy (InequalityConstraint e) =
  mkTyConApp (envTGe env) [psTy, nTy, liftExpr env pn psTy nTy e]

-- | Lift a map constraint: convert MapIx to flat dim index, then lift.
liftMapConstraint :: IslPluginEnv -> [String] -> Int -> Type -> Type -> Constraint MapIx -> Type
liftMapConstraint env pn nIn psTy nTy (EqualityConstraint e) =
  mkTyConApp (envTEq env) [psTy, nTy, liftMapExpr env pn nIn psTy nTy e]
liftMapConstraint env pn nIn psTy nTy (InequalityConstraint e) =
  mkTyConApp (envTGe env) [psTy, nTy, liftMapExpr env pn nIn psTy nTy e]

-- | Lift a set expression to a type-level TExpr.
liftExpr :: IslPluginEnv -> [String] -> Type -> Type -> Expr SetIx -> Type
liftExpr env paramNames psTy nTy = go
  where
    go (Ix (SetDim d)) =
      mkTyConApp (envTDim env) [psTy, nTy,
        mkTyConApp (envMkIdx env) [nTy, mkNumLitTy (fromIntegral d)]]
    go (Ix (SetParam p)) =
      mkTyConApp (envTParam env) [psTy, nTy,
        mkTyConApp (envMkPIdx env) [psTy, mkStrLitTy (mkFastString (paramNames !! p))]]
    go (Constant k) =
      mkTyConApp (envTConst env) [psTy, nTy, liftZ env k]
    go (Add a b) =
      mkTyConApp (envTAdd env) [psTy, nTy, go a, go b]
    go (Mul k a) =
      mkTyConApp (envTMul env) [psTy, nTy, liftZ env k, go a]
    go (FloorDiv a d) =
      mkTyConApp (envTFloorDiv env) [psTy, nTy, go a, liftZ env d]

-- | Lift a map expression: MapIx → flat dim index in combined (ni+no) space.
liftMapExpr :: IslPluginEnv -> [String] -> Int -> Type -> Type -> Expr MapIx -> Type
liftMapExpr env paramNames nIn psTy nTy = go
  where
    go (Ix (InDim d)) =
      mkTyConApp (envTDim env) [psTy, nTy,
        mkTyConApp (envMkIdx env) [nTy, mkNumLitTy (fromIntegral d)]]
    go (Ix (OutDim d)) =
      mkTyConApp (envTDim env) [psTy, nTy,
        mkTyConApp (envMkIdx env) [nTy, mkNumLitTy (fromIntegral (nIn + d))]]
    go (Ix (MapParam p)) =
      mkTyConApp (envTParam env) [psTy, nTy,
        mkTyConApp (envMkPIdx env) [psTy, mkStrLitTy (mkFastString (paramNames !! p))]]
    go (Constant k) =
      mkTyConApp (envTConst env) [psTy, nTy, liftZ env k]
    go (Add a b) =
      mkTyConApp (envTAdd env) [psTy, nTy, go a, go b]
    go (Mul k a) =
      mkTyConApp (envTMul env) [psTy, nTy, liftZ env k, go a]
    go (FloorDiv a d) =
      mkTyConApp (envTFloorDiv env) [psTy, nTy, go a, liftZ env d]

-- | Lift an integer to a type-level Z.
liftZ :: IslPluginEnv -> Integer -> Type
liftZ env k
  | k >= 0    = mkTyConApp (envPos env) [mkNumLitTy k]
  | otherwise = mkTyConApp (envNeg env) [mkNumLitTy (negate k)]

-- | Build a promoted list type @'[x1, x2, ...]@ from element kind and elements.
mkPromotedListTy :: Type -> [Type] -> Type
mkPromotedListTy k = foldr (\x xs -> mkTyConApp promotedConsDataCon [k, x, xs])
                           (mkTyConApp promotedNilDataCon [k])

-- * Building ISL objects

-- ** Sets

buildBasicSet
  :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> Isl.BasicSet
buildBasicSet ctx nParams nDims paramNames constraints =
  let space0 = give ctx $ Space.setAlloc (fromIntegral nParams) (fromIntegral nDims)
      space  = foldl (\sp (i, name) -> give ctx $ Space.setDimName sp Isl.islDimParam i name)
                     space0 (zip [0..] paramNames)
      univ   = give ctx $ BS.universe space
  in foldl (addOneSetConstraint ctx) univ constraints

buildSet :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> Isl.Set
buildSet ctx nParams nDims paramNames constraints =
  give ctx $ S.fromBasicSet (buildBasicSet ctx nParams nDims paramNames constraints)

addOneSetConstraint :: Isl.Ctx -> Isl.BasicSet -> Constraint SetIx -> Isl.BasicSet
addOneSetConstraint ctx bs constraint =
  let sp     = give ctx $ BS.getSpace bs
      ls     = give ctx $ LS.fromSpace sp
      (coeffs, constant) = case constraint of
        InequalityConstraint e -> expandExpr e
        EqualityConstraint e   -> expandExpr e
      emptyC = case constraint of
        InequalityConstraint _ -> give ctx $ C.inequalityAlloc ls
        EqualityConstraint _   -> give ctx $ C.equalityAlloc ls
      withCoeffs = foldl (setSetCoeff ctx) emptyC coeffs
      finalC     = give ctx $ C.setConstantSi withCoeffs (fromIntegral constant)
  in give ctx $ BS.addConstraint bs finalC

setSetCoeff :: Isl.Ctx -> Isl.Constraint -> (Integer, SetIx) -> Isl.Constraint
setSetCoeff ctx c (coeff, ix) =
  let (dimType, pos) = case ix of
        SetDim i   -> (Isl.islDimSet, i)
        SetParam i -> (Isl.islDimParam, i)
  in give ctx $ C.setCoefficientSi c dimType (fromIntegral pos) (fromIntegral coeff)

-- ** Maps

buildBasicMap
  :: Isl.Ctx -> Int -> Int -> Int -> [String] -> [Constraint MapIx] -> Isl.BasicMap
buildBasicMap ctx nParams nIn nOut paramNames constraints =
  let space0 = give ctx $ Space.alloc (fromIntegral nParams) (fromIntegral nIn) (fromIntegral nOut)
      space  = foldl (\sp (i, name) -> give ctx $ Space.setDimName sp Isl.islDimParam i name)
                     space0 (zip [0..] paramNames)
      univ   = give ctx $ BM.universe space
  in foldl (addOneMapConstraint ctx) univ constraints

buildMap :: Isl.Ctx -> Int -> Int -> Int -> [String] -> [Constraint MapIx] -> Isl.Map
buildMap ctx nParams nIn nOut paramNames constraints =
  give ctx $ M.fromBasicMap (buildBasicMap ctx nParams nIn nOut paramNames constraints)

addOneMapConstraint :: Isl.Ctx -> Isl.BasicMap -> Constraint MapIx -> Isl.BasicMap
addOneMapConstraint ctx bm constraint =
  let sp     = give ctx $ BM.getSpace bm
      ls     = give ctx $ LS.fromSpace sp
      (coeffs, constant) = case constraint of
        InequalityConstraint e -> expandExpr e
        EqualityConstraint e   -> expandExpr e
      emptyC = case constraint of
        InequalityConstraint _ -> give ctx $ C.inequalityAlloc ls
        EqualityConstraint _   -> give ctx $ C.equalityAlloc ls
      withCoeffs = foldl (setMapCoeff ctx) emptyC coeffs
      finalC     = give ctx $ C.setConstantSi withCoeffs (fromIntegral constant)
  in give ctx $ BM.addConstraint bm finalC

setMapCoeff :: Isl.Ctx -> Isl.Constraint -> (Integer, MapIx) -> Isl.Constraint
setMapCoeff ctx c (coeff, ix) =
  let (dimType, pos) = case ix of
        InDim i    -> (Isl.islDimIn, i)
        OutDim i   -> (Isl.islDimOut, i)
        MapParam i -> (Isl.islDimParam, i)
  in give ctx $ C.setCoefficientSi c dimType (fromIntegral pos) (fromIntegral coeff)

-- (ISL property check functions removed — inlined into solveOne with richer error messages)

-- * Reifying GHC types to value-level data

extractNat :: Type -> Maybe Int
extractNat (LitTy (NumTyLit n)) = Just (fromIntegral n)
extractNat _                     = Nothing

extractSymbol :: Type -> Maybe String
extractSymbol (LitTy (StrTyLit fs)) = Just (unpackFS fs)
extractSymbol _                      = Nothing

-- | Reify a type-level TConstraint to a value-level Constraint SetIx.
reifyTConstraint :: IslPluginEnv -> [String] -> Type -> Maybe (Constraint SetIx)
reifyTConstraint env paramNames ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envTEq env, expr <- lastArg args ->
        EqualityConstraint   <$> reifyTExpr env paramNames expr
    | tc == envTGe env, expr <- lastArg args ->
        InequalityConstraint <$> reifyTExpr env paramNames expr
  _ -> Nothing

-- | Reify a type-level TConstraint to a value-level Constraint MapIx.
-- Dims 0..nIn-1 → InDim, nIn.. → OutDim.
reifyTConstraintMap :: IslPluginEnv -> [String] -> Int -> Type -> Maybe (Constraint MapIx)
reifyTConstraintMap env paramNames nIn ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envTEq env, expr <- lastArg args ->
        EqualityConstraint   <$> reifyTExprMap env paramNames nIn expr
    | tc == envTGe env, expr <- lastArg args ->
        InequalityConstraint <$> reifyTExprMap env paramNames nIn expr
  _ -> Nothing

-- | Reify a type-level TExpr to a value-level Expr SetIx.
reifyTExpr :: IslPluginEnv -> [String] -> Type -> Maybe (Expr SetIx)
reifyTExpr env paramNames ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envTDim env, idxTy <- lastArg args ->
        case unwrapIdx env idxTy of
          Just nTy -> Ix . SetDim <$> extractNat nTy
          Nothing  -> Nothing

    | tc == envTParam env, pidxTy <- lastArg args ->
        case unwrapPIdx env pidxTy of
          Just sTy -> do
            name <- extractSymbol sTy
            idx  <- elemIndex name paramNames
            Just $ Ix (SetParam idx)
          Nothing -> Nothing

    | tc == envTConst env, zTy <- lastArg args ->
        Constant <$> reifyZ env zTy

    | tc == envTAdd env, (a, b) <- lastTwoArgs args ->
        Add <$> reifyTExpr env paramNames a <*> reifyTExpr env paramNames b

    | tc == envTMul env, (k, a) <- lastTwoArgs args ->
        Mul <$> reifyZ env k <*> reifyTExpr env paramNames a

    | tc == envTFloorDiv env, (a, d) <- lastTwoArgs args ->
        FloorDiv <$> reifyTExpr env paramNames a <*> reifyZ env d

  _ -> Nothing

-- | Reify a type-level TExpr to a value-level Expr MapIx.
-- Dim index d → InDim d if d < nIn, OutDim (d - nIn) otherwise.
reifyTExprMap :: IslPluginEnv -> [String] -> Int -> Type -> Maybe (Expr MapIx)
reifyTExprMap env paramNames nIn ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envTDim env, idxTy <- lastArg args ->
        case unwrapIdx env idxTy of
          Just nTy -> do
            d <- extractNat nTy
            Just $ Ix $ if d < nIn then InDim d else OutDim (d - nIn)
          Nothing -> Nothing

    | tc == envTParam env, pidxTy <- lastArg args ->
        case unwrapPIdx env pidxTy of
          Just sTy -> do
            name <- extractSymbol sTy
            idx  <- elemIndex name paramNames
            Just $ Ix (MapParam idx)
          Nothing -> Nothing

    | tc == envTConst env, zTy <- lastArg args ->
        Constant <$> reifyZ env zTy

    | tc == envTAdd env, (a, b) <- lastTwoArgs args ->
        Add <$> reifyTExprMap env paramNames nIn a <*> reifyTExprMap env paramNames nIn b

    | tc == envTMul env, (k, a) <- lastTwoArgs args ->
        Mul <$> reifyZ env k <*> reifyTExprMap env paramNames nIn a

    | tc == envTFloorDiv env, (a, d) <- lastTwoArgs args ->
        FloorDiv <$> reifyTExprMap env paramNames nIn a <*> reifyZ env d

  _ -> Nothing

unwrapIdx :: IslPluginEnv -> Type -> Maybe Type
unwrapIdx env ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envMkIdx env -> Just (lastArg args)
  _ -> Nothing

unwrapPIdx :: IslPluginEnv -> Type -> Maybe Type
unwrapPIdx env ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envMkPIdx env -> Just (lastArg args)
  _ -> Nothing

reifyZ :: IslPluginEnv -> Type -> Maybe Integer
reifyZ env ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | tc == envPos env -> fromIntegral <$> extractNat (lastArg args)
    | tc == envNeg env -> negate . fromIntegral <$> extractNat (lastArg args)
  _ -> Nothing

-- * Promoted list / arg helpers

lastArg :: [Type] -> Type
lastArg [] = error "isl-plugin: lastArg on empty list"
lastArg xs = last xs

lastTwoArgs :: [Type] -> (Type, Type)
lastTwoArgs xs = case reverse xs of
  (b : a : _) -> (a, b)
  _            -> error "isl-plugin: lastTwoArgs needs at least 2 args"

-- | Unfold a promoted list @'[a, b, c]@ into its element types.
-- Strips coercion casts (GHC inserts these when list elements have
-- kinds involving non-injective type families like @+@).
unfoldTypeList :: Type -> [Type]
unfoldTypeList (CastTy ty _) = unfoldTypeList ty  -- strip casts
unfoldTypeList ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | getOccString tc == ":" , length args >= 3 ->
        let (x, xs) = lastTwoArgs args
        in x : unfoldTypeList xs
  Just (tc, _)
    | getOccString tc == "'[]" -> []
    | getOccString tc == "[]"  -> []
  _ -> []
