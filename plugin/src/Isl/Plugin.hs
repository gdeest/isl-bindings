{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}

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
  , unsafeTcPluginTcM
  )
import GHC.Tc.Types.Constraint (Ct, CtEvidence, ctPred, ctEvidence, ctEvPred, ctLoc, ctLocSpan)
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm, evDataConApp )
import GHC.Core.Class (Class, classTyCon)
import GHC.Core.Predicate (classifyPredType, Pred(..))
import GHC.Core.TyCo.Rep (Type(..), TyLit(..), UnivCoProvenance(..))
import GHC.Core.Type (splitTyConApp_maybe, mkStrLitTy)
import GHC.Core.Coercion (mkUnivCo)
import GHC.Core.Reduction (Reduction(..))
import GHC.Core (CoreExpr)
import GHC.Core.Make (mkCoreConApps)
import GHC.Core.DataCon (promoteDataCon, dataConRepArity)
import GHC.Builtin.Types (listTyCon, promotedConsDataCon, promotedNilDataCon, promotedTupleDataCon, unitDataCon)
import Language.Haskell.Syntax.Basic (Boxity(Boxed))
import GHC.Types.Unique.FM (UniqFM, listToUFM)
import GHC.Unit.Finder (FindResult(..))
import GHC.Unit.Module (Module)
import GHC.Types.PkgQual (PkgQual(NoPkgQual))
import GHC.Utils.Outputable (text, (<+>))
import GHC.Data.FastString (unpackFS, mkFastString)
import GHC.Builtin.Names
  ( typeErrorTextDataConName
  , typeErrorVAppendDataConName
  )
import GHC.Tc.Errors.Types (TcRnMessage(TcRnUserTypeError))
import GHC.Tc.Utils.Monad (addErrAt)
import GHC.Types.SrcLoc (SrcSpan(..), srcSpanStartLine, srcSpanStartCol)
import qualified GHC.Data.Strict as Strict

import Data.Maybe (mapMaybe)
import Data.List (elemIndex)
import Data.Foldable (foldlM)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Set as Set

import qualified Isl.Types as Isl
import qualified Isl.Linear as IslL
import Isl.Linear (Both(..), queryM_, urWrap)
import qualified Isl.Unsafe as IslU
import Control.DeepSeq (NFData, rnf)
import Isl.Monad (IslT, Ur(..), withIslCtx)
import qualified Isl.BasicSet as BS
import qualified Isl.BasicMap as BM
import qualified Isl.Set as S
import qualified Isl.Map as M
import qualified Isl.Constraint as C
import qualified Isl.LocalSpace as LS
import qualified Isl.Space as Space
import qualified Isl.Aff as Aff
import qualified Isl.MultiAff as MA
import qualified Isl.PwAff as PA
import qualified Isl.Val as Val
import Isl.Typed.Constraints
  ( Expr(..), Constraint(..), SetIx(..), MapIx(..), expandExpr, exprToSetAff
  , rebuildExprWithDivs, extractSetDivs, extractMapDivs
  , extractSetConstraint, extractMapConstraint
  , addSetConstraint, addMapConstraint )

-- * Helpers for running ISL operations from the plugin

-- | Run an ISL computation with a pre-allocated context.
-- Uses 'withIslCtx' which enforces 'Ur' return type and 'NFData'.
runRawIsl :: NFData a => Isl.Ctx -> IslT IO (Ur a) -> a
runRawIsl ctx m = unsafePerformIO $ withIslCtx ctx m

-- | Run a single consuming ISL action and unwrap the result.
-- Convenience wrapper that lifts the action's result into 'Ur'.
runRawIsl1 :: NFData a => Isl.Ctx -> IslT IO a -> a
runRawIsl1 ctx action = runRawIsl ctx (IslL.do { r <- action; urWrap r })

-- | Produce @n@ independent copies of a 'Dupable' resource by chaining
-- @isl_X_copy@ via 'IslU.dup'.  Each returned element has its own
-- refcounted ISL pointer and may be independently consumed.
--
-- Used by 'WantedPartitions' to give the pairwise-disjointness loop
-- fresh per-pair borrow/consume handles without re-invoking
-- 'buildSet' — see the CSE-safety explainer at that handler.  The
-- 'unsafePerformIO' is sound here: @isl_X_copy@ is an O(1) refcount
-- bump with no observable ordering constraints.
duplicateN :: Isl.Dupable a => Int -> a -> [a]
duplicateN !n !s
  | n <= 1 = [s]
  | otherwise =
      let !(a, b) = unsafePerformIO (IslU.dup s)
      in a : duplicateN (n - 1) b

-- | Construct a SetRef from an owned Set (for PureQuery functions).
setRef :: Isl.Set -> Isl.SetRef
setRef (Isl.Set p) = Isl.SetRef p

-- | Construct a MapRef from an owned Map.
mapRef :: Isl.Map -> Isl.MapRef
mapRef (Isl.Map p) = Isl.MapRef p

-- | Construct a BasicSetRef from an owned BasicSet.
basicSetRef :: Isl.BasicSet -> Isl.BasicSetRef
basicSetRef (Isl.BasicSet p) = Isl.BasicSetRef p

-- | Construct a BasicMapRef from an owned BasicMap.
basicMapRef :: Isl.BasicMap -> Isl.BasicMapRef
basicMapRef (Isl.BasicMap p) = Isl.BasicMapRef p

-- | Construct a ConstraintRef from an owned Constraint.
constraintRef :: Isl.Constraint -> Isl.ConstraintRef
constraintRef (Isl.Constraint p) = Isl.ConstraintRef p

-- | Construct a ValRef from an owned Val.
valRef :: Isl.Val -> Isl.ValRef
valRef (Isl.Val p) = Isl.ValRef p

-- | Construct an AffRef from an owned Aff.
affRef :: Isl.Aff -> Isl.AffRef
affRef (Isl.Aff p) = Isl.AffRef p

-- | Construct a MultiAffRef from an owned MultiAff.
multiAffRef :: Isl.MultiAff -> Isl.MultiAffRef
multiAffRef (Isl.MultiAff p) = Isl.MultiAffRef p

-- | Construct a PwAffRef from an owned PwAff.
pwAffRef :: Isl.PwAff -> Isl.PwAffRef
pwAffRef (Isl.PwAff p) = Isl.PwAffRef p

-- * Plugin entry point

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just islTcPlugin
  , pluginRecompile = purePlugin
  }

-- * Plugin environment

data IslPluginEnv = IslPluginEnv
  { -- ISL context, kept alive for the plugin's lifetime
    envCtxPtr        :: !(Isl.Ctx)
    -- Dedup set: source locations where we've already emitted errors
  , envEmittedErrors :: !(IORef (Set.Set (Int, Int)))
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
    -- Coverage proof obligation classes
  , envCoversClass     :: !Class
  , envPartitionsClass :: !Class
    -- Multi-aff proof obligation class
  , envMultiAffEqualClass :: !Class
    -- Multi-aff type family TyCons
  , envIslMultiAffToMap     :: !TyCon
  , envIslApplyMultiAff     :: !TyCon
  , envIslPreimageMultiAff  :: !TyCon
  , envIslComposeMultiAff   :: !TyCon
  , envIslMultiAffToString  :: !TyCon
  , envIslMultiAffFromString :: !TyCon
    -- PwAff type family TyCons
  , envIslSetDimMax :: !TyCon
  , envIslSetDimMin :: !TyCon
    -- Union-aware type family TyCons
  , envIslIntersectSetU  :: !TyCon
  , envIslComplementSetU :: !TyCon
  , envIslDifferenceSetU :: !TyCon
  , envIslApplyU         :: !TyCon
  , envIslDomainTFU      :: !TyCon
  , envIslRangeTFU       :: !TyCon
  , envIslComposeU       :: !TyCon
  , envIslReverseMapU    :: !TyCon
  , envIslProjectOutU    :: !TyCon
  , envIslToStringU      :: !TyCon
  , envIslMapToStringU   :: !TyCon
    -- Union-aware proof obligation classes
  , envSubsetUClass   :: !Class
  , envEqualUClass    :: !Class
  , envNonEmptyUClass :: !Class
    -- Branch-grouped proof obligation classes
  , envPartitionsUClass  :: !Class
  , envCoversUClass      :: !Class
  , envImageSubsetUClass :: !Class
    -- TExpr data type TyCon (for building lifted expression lists)
  , envTExprTC :: !TyCon
    -- TypeError infrastructure (for custom error messages)
  , envTypeErrorText  :: !TyCon   -- 'Text promoted data constructor
  , envTypeErrorVApp  :: !TyCon   -- ':$$: promoted data constructor
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

  ctxPtr <- tcPluginIO Isl.c_ctx_alloc
  let envCtxPtr = Isl.Ctx ctxPtr
  envEmittedErrors <- tcPluginIO $ newIORef Set.empty

  constraintMod <- resolveModule "Isl.TypeLevel.Constraint"
  exprMod       <- resolveModule "Isl.TypeLevel.Expr"

  -- Proof obligation classes
  [ envSubsetClass, envNonEmptyClass, envEqualClass
    , envMapSubsetClass, envMapEqualClass, envRangeOfClass, envImageSubsetClass
    , envCoversClass, envPartitionsClass
    , envMultiAffEqualClass
    , envSubsetUClass, envEqualUClass, envNonEmptyUClass
    , envPartitionsUClass, envCoversUClass, envImageSubsetUClass
    ] <- lookupClasses constraintMod
          [ "IslSubset", "IslNonEmpty", "IslEqual"
          , "IslMapSubset", "IslMapEqual", "IslRangeOf", "IslImageSubset"
          , "IslCovers", "IslPartitions"
          , "IslMultiAffEqual"
          , "IslSubsetU", "IslEqualU", "IslNonEmptyU"
          , "IslPartitionsU", "IslCoversU", "IslImageSubsetU"
          ]

  -- Promoted data constructors for walking type-level expression/constraint trees
  [envTEq, envTGe] <- lookupPromDCs constraintMod ["TEq", "TGe"]
  [ envTDim, envTParam, envTConst, envTAdd, envTMul, envTFloorDiv
    , envPos, envNeg, envMkIdx, envMkPIdx
    ] <- lookupPromDCs exprMod
          [ "TDim", "TParam", "TConst", "TAdd", "TMul", "TFloorDiv"
          , "Pos", "Neg", "MkIdx", "MkPIdx"
          ]

  -- Type family and data type TyCons
  [ envTConstraintTC
    , envIslIntersectSet, envIslComplementSet, envIslDifferenceSet
    , envIslApply, envIslDomainTF, envIslRangeTF, envIslCompose, envIslReverseMap, envIslProjectOut
    , envIslFromString, envIslToString, envIslMapToString
    , envIslMultiAffToMap, envIslApplyMultiAff, envIslPreimageMultiAff
    , envIslComposeMultiAff, envIslMultiAffToString, envIslMultiAffFromString
    , envIslSetDimMax, envIslSetDimMin
    , envIslIntersectSetU, envIslComplementSetU, envIslDifferenceSetU
    , envIslApplyU, envIslDomainTFU, envIslRangeTFU, envIslComposeU, envIslReverseMapU, envIslProjectOutU
    , envIslToStringU, envIslMapToStringU
    ] <- lookupTFs constraintMod
          [ "TConstraint"
          , "IslIntersectSet", "IslComplementSet", "IslDifferenceSet"
          , "IslApply", "IslDomainTF", "IslRangeTF", "IslCompose", "IslReverseMap", "IslProjectOut"
          , "IslFromString", "IslToString", "IslMapToString"
          , "IslMultiAffToMap", "IslApplyMultiAff", "IslPreimageMultiAff"
          , "IslComposeMultiAff", "IslMultiAffToString", "IslMultiAffFromString"
          , "IslSetDimMax", "IslSetDimMin"
          , "IslIntersectSetU", "IslComplementSetU", "IslDifferenceSetU"
          , "IslApplyU", "IslDomainTFU", "IslRangeTFU", "IslComposeU", "IslReverseMapU", "IslProjectOutU"
          , "IslToStringU", "IslMapToStringU"
          ]
  [envTExprTC] <- lookupTFs exprMod ["TExpr"]

  -- TypeError infrastructure
  envTypeErrorText <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
  envTypeErrorVApp <- promoteDataCon <$> tcLookupDataCon typeErrorVAppendDataConName

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

lookupClasses :: Module -> [String] -> TcPluginM [Class]
lookupClasses md = mapM (lookupClass md)

lookupPromDCs :: Module -> [String] -> TcPluginM [TyCon]
lookupPromDCs md = mapM (lookupPromDC md)

lookupTFs :: Module -> [String] -> TcPluginM [TyCon]
lookupTFs md = mapM (lookupTF md)

-- * Solver dispatch

solveIsl :: IslPluginEnv -> EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult
solveIsl env _evBinds _givens wanteds = do
  let classified = mapMaybe (classifyWanted env) wanteds
  if null classified
    then pure $ TcPluginOk [] []
    else do
      results <- mapM (solveOne env) classified
      let solved_    = [(ev, ct) | Solved ev ct <- results]
          insoluble_ = [ct       | Failed ct     <- results]
      pure $ TcPluginSolveResult
        { tcPluginInsolubleCts = insoluble_
        , tcPluginSolvedCts    = solved_
        , tcPluginNewCts       = []
        }

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
  -- Coverage obligations
  | WantedCovers     !Ct !Type !Type !Type !Type             -- ps, n, fullDom, branches
  | WantedPartitions !Ct !Type !Type !Type !Type             -- ps, n, fullDom, branches
  -- Union-aware set obligations
  | WantedSubsetU   !Ct !Type !Type !Type !Type              -- ps, n, css1, css2
  | WantedEqualU    !Ct !Type !Type !Type !Type              -- ps, n, css1, css2
  | WantedNonEmptyU !Ct !Type !Type !Type                    -- ps, n, css
  -- Branch-grouped obligations
  | WantedPartitionsU  !Ct !Type !Type !Type !Type           -- ps, n, fullDom, branches
  | WantedCoversU      !Ct !Type !Type !Type !Type           -- ps, n, fullDom, branches
  | WantedImageSubsetU !Ct !Type !Type !Type !Type !Type !Type  -- ps, ni, no, mapCs, srcCss, dstCs
  -- Multi-aff obligations
  | WantedMultiAffEqual !Ct !Type !Type !Type !Type !Type    -- ps, ni, no, es1, es2

classifyWanted :: IslPluginEnv -> Ct -> Maybe IslWanted
classifyWanted env@IslPluginEnv{..} ct =
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
      -- Coverage obligations
      | cls == envCoversClass, [ps, n, fullDom, branches] <- args ->
          Just $ WantedCovers ct ps n fullDom branches
      | cls == envPartitionsClass, [ps, n, fullDom, branches] <- args ->
          Just $ WantedPartitions ct ps n fullDom branches
      -- Union-aware set obligations
      | cls == envSubsetUClass, [ps, n, css1, css2] <- args ->
          Just $ WantedSubsetU ct ps n css1 css2
      | cls == envEqualUClass, [ps, n, css1, css2] <- args ->
          Just $ WantedEqualU ct ps n css1 css2
      | cls == envNonEmptyUClass, [ps, n, css] <- args ->
          Just $ WantedNonEmptyU ct ps n css
      -- Branch-grouped obligations
      | cls == envPartitionsUClass, [ps, n, fullDom, branches] <- args ->
          Just $ WantedPartitionsU ct ps n fullDom branches
      | cls == envCoversUClass, [ps, n, fullDom, branches] <- args ->
          Just $ WantedCoversU ct ps n fullDom branches
      | cls == envImageSubsetUClass -> case args of
          [ps, ni, no, mapCs, srcCss, dstCs] ->
            Just $ WantedImageSubsetU ct ps ni no mapCs srcCss dstCs
          _ -> Nothing
      -- Multi-aff obligations
      | cls == envMultiAffEqualClass, [ps, ni, no, es1, es2] <- args ->
          Just $ WantedMultiAffEqual ct ps ni no es1 es2
    _ -> Nothing

-- * Solving individual constraints

-- | Result of solving a single wanted:
-- Nothing = deferred (type families stuck, metavars present)
-- Right (ev, ct) = solved (ISL check passed)
-- Left (origCt, errCt) = ISL check failed; origCt goes insoluble
--   (suppresses "No instance"), errCt goes to new work (GHC reduces
--   the TypeError and reports the custom message).
data SolveResult
  = Solved EvTerm Ct
  | Failed Ct
  | Deferred

solvedWith :: Ct -> TcPluginM SolveResult
solvedWith ct = do
  ev <- makeEvidence ct
  pure (Solved ev ct)

failedWith :: IslPluginEnv -> Ct -> [String] -> TcPluginM SolveResult
failedWith env ct !lines_ = do
  -- Deep-force all strings before passing to error emission
  tcPluginIO $ evaluate (rnf lines_)
  emitTypeError env ct lines_
  -- Mark the original constraint as insoluble.  GHC will also report
  -- its own "No instance" error for it — the custom error appears
  -- first and is actionable; the raw dump is noise but harmless.
  -- We can't suppress it without fake evidence (unsound with
  -- -fdefer-type-errors).
  pure (Failed ct)

solveOne :: IslPluginEnv -> IslWanted -> TcPluginM SolveResult
-- N.B. Returns 'Deferred' for stuck type families or metavars.
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
              result = S.isSubset (setRef s1) (setRef s2)
          if result
            then traceProved "Subset" >> (solvedWith ct)
            else do
              !lhsStr  <- mkStr $ islSetToStr s1
              !rhsStr  <- mkStr $ islSetToStr s2
              !diffStr <- mkStr $ islSetToStr $ runRawIsl1 ctx $
                S.subtract (buildSet ctx nParams nDims paramNames cs1)
                           (buildSet ctx nParams nDims paramNames cs2)
              traceFailed "Subset" $
                "\n  LHS: " ++ lhsStr ++
                "\n  is NOT a subset of" ++
                "\n  RHS: " ++ rhsStr
              failedWith env ct
                [ "IslSubset failed: set is not a subset"
                , "  LHS:           " ++ lhsStr
                , "  RHS:           " ++ rhsStr
                , "  Counterexample:" ++ diffStr
                , "  (points in LHS but not in RHS)"
                ]
        _ -> traceReifyFail >> pure Deferred

  WantedNonEmpty ct psTy nTy csTy ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let mcs = reifyConstraintList env paramNames (unfoldTypeList csTy)
      case mcs of
        Just cs -> do
          let ctx = envCtxPtr env
              s = buildSet ctx (length paramNames) nDims paramNames cs
              result = not (S.isEmpty (setRef s))
          if result
            then traceProved "NonEmpty" >> (solvedWith ct)
            else do
              !setStr <- tcPluginIO $ evaluate $ islSetToStr s
              traceFailed "NonEmpty" $
                "\n  Set is empty: " ++ setStr
              failedWith env ct
                [ "IslNonEmpty failed: set is empty"
                , "  Set: " ++ setStr
                ]
        _ -> traceReifyFail >> pure Deferred

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
              result = S.isEqual (setRef s1) (setRef s2)
          if result
            then traceProved "Equal" >> (solvedWith ct)
            else do
              !lhsStr   <- mkStr $ islSetToStr s1
              !rhsStr   <- mkStr $ islSetToStr s2
              !diff1Str <- mkStr $ islSetToStr $ runRawIsl1 ctx $
                S.subtract (buildSet ctx nParams nDims paramNames cs1)
                           (buildSet ctx nParams nDims paramNames cs2)
              !diff2Str <- mkStr $ islSetToStr $ runRawIsl1 ctx $
                S.subtract (buildSet ctx nParams nDims paramNames cs2)
                           (buildSet ctx nParams nDims paramNames cs1)
              traceFailed "Equal" $
                "\n  LHS: " ++ lhsStr ++
                "\n  is NOT equal to" ++
                "\n  RHS: " ++ rhsStr
              failedWith env ct
                [ "IslEqual failed: sets are not equal"
                , "  LHS:          " ++ lhsStr
                , "  RHS:          " ++ rhsStr
                , "  In LHS \\ RHS: " ++ diff1Str
                , "  In RHS \\ LHS: " ++ diff2Str
                ]
        _ -> traceReifyFail >> pure Deferred

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
              result = M.isSubset (mapRef m1) (mapRef m2)
          if result
            then traceProved "MapSubset" >> (solvedWith ct)
            else do
              !lhsStr <- mkStr $ islMapToStr m1
              !rhsStr <- mkStr $ islMapToStr m2
              traceFailed "MapSubset" $
                "\n  LHS: " ++ lhsStr ++
                "\n  is NOT a subset of" ++
                "\n  RHS: " ++ rhsStr
              failedWith env ct
                [ "IslMapSubset failed: map is not a subset"
                , "  LHS: " ++ lhsStr
                , "  RHS: " ++ rhsStr
                ]
        _ -> traceReifyFail >> pure Deferred

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
              result = M.isEqual (mapRef m1) (mapRef m2)
          if result
            then traceProved "MapEqual" >> (solvedWith ct)
            else do
              !lhsStr <- mkStr $ islMapToStr m1
              !rhsStr <- mkStr $ islMapToStr m2
              traceFailed "MapEqual" $
                "\n  LHS: " ++ lhsStr ++
                "\n  is NOT equal to" ++
                "\n  RHS: " ++ rhsStr
              failedWith env ct
                [ "IslMapEqual failed: maps are not equal"
                , "  LHS: " ++ lhsStr
                , "  RHS: " ++ rhsStr
                ]
        _ -> traceReifyFail >> pure Deferred

  WantedRangeOf ct psTy niTy noTy mapCsTy rangeCsTy ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let nParams = length paramNames
          mMapCs   = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
          mRangeCs = reifyConstraintList env paramNames (unfoldTypeList rangeCsTy)
      case (mMapCs, mRangeCs) of
        (Just mapCs, Just rangeCs) -> do
          let ctx  = envCtxPtr env
              rebuildMap = buildMap ctx nParams nIn nOut paramNames mapCs
              rng     = runRawIsl1 ctx $ M.range rebuildMap
              expected = buildSet ctx nParams nOut paramNames rangeCs
              result  = S.isEqual (setRef rng) (setRef expected)
          if result
            then traceProved "RangeOf" >> (solvedWith ct)
            else do
              -- Rebuild map for display (M.range consumed the original)
              !mapStr <- mkStr $ islMapToStr (buildMap ctx nParams nIn nOut paramNames mapCs)
              !rngStr <- mkStr $ islSetToStr rng
              !expStr <- mkStr $ islSetToStr expected
              traceFailed "RangeOf" $
                "\n  Map:      " ++ mapStr ++
                "\n  Range:    " ++ rngStr ++
                "\n  Expected: " ++ expStr
              failedWith env ct
                [ "IslRangeOf failed: range does not match expected set"
                , "  Map:      " ++ mapStr
                , "  Range:    " ++ rngStr
                , "  Expected: " ++ expStr
                ]
        _ -> traceReifyFail >> pure Deferred

  WantedImageSubset ct psTy niTy noTy mapCsTy srcCsTy dstCsTy ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let nParams = length paramNames
          mMapCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
          mSrcCs = reifyConstraintList env paramNames (unfoldTypeList srcCsTy)
          mDstCs = reifyConstraintList env paramNames (unfoldTypeList dstCsTy)
      case (mMapCs, mSrcCs, mDstCs) of
        (Just mapCs, Just srcCs, Just dstCs) -> do
          let ctx   = envCtxPtr env
              -- S.apply consumes both src and m, so rebuild fresh
              -- copies for display and counterexample if needed.
              image = runRawIsl1 ctx $
                S.apply (buildSet ctx nParams nIn paramNames srcCs)
                        (buildMap ctx nParams nIn nOut paramNames mapCs)
              dst   = buildSet ctx nParams nOut paramNames dstCs
              result = S.isSubset (setRef image) (setRef dst)
          if result
            then traceProved "ImageSubset" >> (solvedWith ct)
            else do
              -- Force ISL-to-string conversions in IO to prevent
              -- lazy thunks capturing freed ISL objects.
              !mapStr <- mkStr $ islMapToStr (buildMap ctx nParams nIn nOut paramNames mapCs)
              !srcStr <- mkStr $ islSetToStr (buildSet ctx nParams nIn paramNames srcCs)
              !imgStr <- mkStr $ islSetToStr image
              !dstStr <- mkStr $ islSetToStr dst
              !diffStr <- mkStr $ islSetToStr $ runRawIsl1 ctx $
                S.subtract
                  (runRawIsl1 ctx $ S.apply
                    (buildSet ctx nParams nIn paramNames srcCs)
                    (buildMap ctx nParams nIn nOut paramNames mapCs))
                  (buildSet ctx nParams nOut paramNames dstCs)
              traceFailed "ImageSubset" $
                "\n  Map:    " ++ mapStr ++
                "\n  Source: " ++ srcStr ++
                "\n  Image:  " ++ imgStr ++
                "\n  Target: " ++ dstStr ++
                "\n  Diff:   " ++ diffStr
              failedWith env ct
                [ "IslImageSubset failed: image of source under map is not contained in target"
                , "  Map:           " ++ mapStr
                , "  Source:        " ++ srcStr
                , "  Image:         " ++ imgStr
                , "  Target:        " ++ dstStr
                , "  Counterexample:" ++ diffStr
                , "  (points in image but not in target)"
                ]
        _ -> traceReifyFail >> pure Deferred

  -- === Coverage obligations ===

  WantedCovers ct psTy nTy fullDomTy branchesTy ->
    -- Guard: if branches type is still a metavariable, defer.
    case branchesTy of
      TyVarTy _ -> pure Deferred
      _ -> withReified env psTy nTy $ \paramNames nDims -> do
        let nParams = length paramNames
            mFullDom  = reifyDisjunction env paramNames fullDomTy
            mBranches = reifyDisjunction env paramNames branchesTy
        case (mFullDom, mBranches) of
          -- TDirect case: doms ~ '[], no branches needed — expression covers
          -- the whole domain by construction.
          (Just _fullDisj, Just []) ->
            traceProved "Covers (direct)" >> (solvedWith ct)
          (Just fullDisj, Just branchDisj) -> do
            let ctx = envCtxPtr env
                buildUnion disj = case [buildSet ctx nParams nDims paramNames conj | conj <- disj] of
                  []     -> runRawIsl1 ctx $ S.fromBasicSet (buildBasicSet ctx nParams nDims paramNames [])
                  [x]    -> x
                  (x:xs) -> foldl (\acc s -> runRawIsl1 ctx $ S.union acc s) x xs
                fullDomSet  = buildUnion fullDisj
                branchesSet = buildUnion branchDisj
                result = S.isSubset (setRef fullDomSet) (setRef branchesSet)
            if result
              then traceProved "Covers" >> (solvedWith ct)
              else do
                !domStr  <- mkStr $ islSetToStr fullDomSet
                !brnStr  <- mkStr $ islSetToStr branchesSet
                -- Rebuild sets for subtract (which consumes both arguments)
                !uncStr  <- mkStr $ islSetToStr $ runRawIsl1 ctx $
                  S.subtract (buildUnion fullDisj) (buildUnion branchDisj)
                traceFailed "Covers" $
                  "\n  Domain:    " ++ domStr ++
                  "\n  Branches:  " ++ brnStr ++
                  "\n  Uncovered: " ++ uncStr
                failedWith env ct
                  [ "IslCovers failed: branches do not cover the domain"
                  , "  Domain:    " ++ domStr
                  , "  Branches:  " ++ brnStr
                  , "  Uncovered: " ++ uncStr
                  ]
          _ -> traceReifyFail >> pure Deferred

  -- Partitions = IslCovers + pairwise disjointness within the
  -- ambient.  Used by 'Alpha.Core.Case' to enforce that each point
  -- in the recurrence domain is defined by exactly one branch — see
  -- v5.2 in @doc/alpha-implementation.md@ (retires D22).
  --
  -- Implementation subtlety (CSE-safety, inherited from the retired
  -- D22 @WantedBranchFit@ handler): 'buildSet' is a pure function
  -- wrapping 'unsafePerformIO', so GHC's common-subexpression
  -- elimination happily shares @buildSet ctx … cs@ across
  -- syntactically-identical call sites.  The pairwise-disjointness
  -- loop consumes each branch set in an 'S.intersect', so reusing
  -- the same 'buildSet' expression across iterations would collapse
  -- to a single 'Isl.Set' pointer and UAF on the second iteration
  -- (ISL then segfaults with @isl_space.c:813: dim has no id@ or
  -- a malloc-arena-corruption abort).
  --
  -- The fix is to build each branch and the ambient ONCE and use
  -- 'IslL.dup' (= @isl_set_copy@, O(1), refcounted) via 'duplicateN'
  -- to produce a list of independent pointers — one per consume site.
  -- Do NOT "simplify" this by rebuilding via 'buildSet' inside the
  -- loop; it will crash.  See D22 for the original diagnosis.
  WantedPartitions ct psTy nTy fullDomTy branchesTy ->
    case branchesTy of
      TyVarTy _ -> pure Deferred
      _ -> withReified env psTy nTy $ \paramNames nDims -> do
        let nParams = length paramNames
            mFullDom  = reifyDisjunction env paramNames fullDomTy
            mBranches = reifyDisjunction env paramNames branchesTy
        case (mFullDom, mBranches) of
          (Just _fullDisj, Just []) ->
            -- No branches declared — vacuously disjoint; coverage
            -- holds only if the full domain is empty.  Treat as a
            -- trivial accept to match 'WantedCovers' 'TDirect'.
            traceProved "Partitions (direct)" >>
            (solvedWith ct)
          (Just fullDisj, Just branchDisj) -> do
            let ctx     = envCtxPtr env
                buildSetsFromDisj disj =
                  [buildSet ctx nParams nDims paramNames conj | conj <- disj]
                buildUnion sets = case sets of
                  []     -> runRawIsl1 ctx $ S.fromBasicSet (buildBasicSet ctx nParams nDims paramNames [])
                  [x]    -> x
                  (x:xs) -> foldl (\acc s -> runRawIsl1 ctx $ S.union acc s) x xs

                -- Build each raw branch ONCE.  Dup enough copies for
                -- the coverage union (1), a possible failure-path
                -- rebuild (1), and the @n-1@ disjointness pairs each
                -- branch participates in.  Total: @n + 1@ copies per
                -- branch (pads safely for the @n < 2@ trivial case).
                rawBranches     = buildSetsFromDisj branchDisj
                n               = length rawBranches
                nPerBranchCopy  = n + 1
                branchCopies    = map (duplicateN nPerBranchCopy) rawBranches
                splitCopies (x:y:rest) = (x, y, rest)
                splitCopies _ = error "duplicateN: impossible (n+1 >= 2)"
                (unionInputs, failureUnionInputs, disjStacks) =
                  unzip3 (map splitCopies branchCopies)

                -- fullDom copies: 1 borrowed for the coverage
                -- subset-check, 1 consumed on the failure path, and
                -- one consumed per disjointness pair.
                nPairs         = (n * (n - 1)) `div` 2
                nFullDomCopies = 2 + nPairs
                rawFullDom     = buildUnion (buildSetsFromDisj fullDisj)
                fullDomCopies  = duplicateN nFullDomCopies rawFullDom
                fullDomCoverage = fullDomCopies !! 0
                fullDomFailure  = fullDomCopies !! 1
                fullDomForPairs = drop 2 fullDomCopies

                branchesCoverage = buildUnion unionInputs
                coverage = S.isSubset (setRef fullDomCoverage) (setRef branchesCoverage)
            if not coverage
              then do
                let branchesFailure = buildUnion failureUnionInputs
                    uncovered = runRawIsl1 ctx $ S.subtract fullDomFailure branchesFailure
                !domStr <- mkStr $ islSetToStr fullDomCoverage
                !brnStr <- mkStr $ islSetToStr branchesCoverage
                !uncStr <- mkStr $ islSetToStr uncovered
                traceFailed "Partitions" $
                  "\n  Domain:    " ++ domStr ++
                  "\n  Branches:  " ++ brnStr ++
                  "\n  Uncovered: " ++ uncStr
                failedWith env ct
                  [ "IslPartitions failed: branches do not cover the domain"
                  , "  Domain:    " ++ domStr
                  , "  Branches:  " ++ brnStr
                  , "  Uncovered: " ++ uncStr
                  ]
              else
                if n < 2
                  then traceProved "Partitions" >> (solvedWith ct)
                  else do
                    let pairs = [(i, j) | i <- [0 .. n - 2], j <- [i + 1 .. n - 1]]
                        replaceAt idx v xs = take idx xs ++ [v] ++ drop (idx + 1) xs
                        takeCopy stacks idx = case stacks !! idx of
                          (x : xs) -> (x, replaceAt idx xs stacks)
                          []       -> error "isl-plugin: WantedPartitions: branch copy stack exhausted"
                        go _ _ [] = Nothing
                        go _ [] _ = error "isl-plugin: WantedPartitions: ran out of fullDom copies"
                        go stacks (fd : fdRest) ((i, j) : pRest) =
                          let (bi, stacks')  = takeCopy stacks  i
                              (bj, stacks'') = takeCopy stacks' j
                              inter1 = runRawIsl1 ctx $ S.intersect bi bj
                              inter2 = runRawIsl1 ctx $ S.intersect inter1 fd
                          in if S.isEmpty (setRef inter2)
                               then go stacks'' fdRest pRest
                               else Just (i, j, inter2)
                    case go disjStacks fullDomForPairs pairs of
                      Nothing -> traceProved "Partitions" >> (solvedWith ct)
                      Just (i, j, overlap) -> do
                        !ovlStr <- tcPluginIO $ evaluate $ islSetToStr overlap
                        traceFailed "Partitions" $
                          "\n  Branches " ++ show i ++ " and " ++ show j ++
                          " overlap within the ambient." ++
                          "\n  Overlap: " ++ ovlStr
                        failedWith env ct
                          [ "IslPartitions failed: branches " ++ show i ++ " and " ++ show j ++ " overlap"
                          , "  Overlap: " ++ ovlStr
                          ]
          _ -> traceReifyFail >> pure Deferred

  -- === Union-aware set obligations ===

  WantedSubsetU ct psTy nTy css1Ty css2Ty ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let nParams = length paramNames
          mCss1 = reifyDisjunction env paramNames css1Ty
          mCss2 = reifyDisjunction env paramNames css2Ty
      case (mCss1, mCss2) of
        (Just css1, Just css2) -> do
          let ctx = envCtxPtr env
              s1 = buildUnionSetFromDisj ctx nParams nDims paramNames css1
              s2 = buildUnionSetFromDisj ctx nParams nDims paramNames css2
              result = S.isSubset (setRef s1) (setRef s2)
          if result
            then traceProved "SubsetU" >> (solvedWith ct)
            else do
              !lhsStr  <- mkStr $ islSetToStr s1
              !rhsStr  <- mkStr $ islSetToStr s2
              !diffStr <- mkStr $ islSetToStr $ runRawIsl1 ctx $
                S.subtract (buildUnionSetFromDisj ctx nParams nDims paramNames css1)
                           (buildUnionSetFromDisj ctx nParams nDims paramNames css2)
              traceFailed "SubsetU" $
                "\n  LHS: " ++ lhsStr ++
                "\n  is NOT a subset of" ++
                "\n  RHS: " ++ rhsStr
              failedWith env ct
                [ "IslSubsetU failed: set is not a subset"
                , "  LHS:           " ++ lhsStr
                , "  RHS:           " ++ rhsStr
                , "  Counterexample:" ++ diffStr
                ]
        _ -> traceReifyFail >> pure Deferred

  WantedEqualU ct psTy nTy css1Ty css2Ty ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let nParams = length paramNames
          mCss1 = reifyDisjunction env paramNames css1Ty
          mCss2 = reifyDisjunction env paramNames css2Ty
      case (mCss1, mCss2) of
        (Just css1, Just css2) -> do
          let ctx = envCtxPtr env
              s1 = buildUnionSetFromDisj ctx nParams nDims paramNames css1
              s2 = buildUnionSetFromDisj ctx nParams nDims paramNames css2
              result = S.isEqual (setRef s1) (setRef s2)
          if result
            then traceProved "EqualU" >> (solvedWith ct)
            else do
              !lhsStr <- mkStr $ islSetToStr s1
              !rhsStr <- mkStr $ islSetToStr s2
              traceFailed "EqualU" $
                "\n  LHS: " ++ lhsStr ++
                "\n  is NOT equal to" ++
                "\n  RHS: " ++ rhsStr
              failedWith env ct
                [ "IslEqualU failed: sets are not equal"
                , "  LHS: " ++ lhsStr
                , "  RHS: " ++ rhsStr
                ]
        _ -> traceReifyFail >> pure Deferred

  WantedNonEmptyU ct psTy nTy cssTy ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let nParams = length paramNames
          mCss = reifyDisjunction env paramNames cssTy
      case mCss of
        Just css -> do
          let ctx = envCtxPtr env
              s = buildUnionSetFromDisj ctx nParams nDims paramNames css
              result = not (S.isEmpty (setRef s))
          if result
            then traceProved "NonEmptyU" >> (solvedWith ct)
            else do
              !setStr <- tcPluginIO $ evaluate $ islSetToStr s
              traceFailed "NonEmptyU" $
                "\n  Set: " ++ setStr ++
                "\n  is EMPTY"
              failedWith env ct
                [ "IslNonEmptyU failed: set is empty"
                , "  Set: " ++ setStr
                ]
        _ -> traceReifyFail >> pure Deferred

  -- === Branch-grouped obligations ===

  WantedPartitionsU ct psTy nTy fullDomTy branchesTy ->
    case branchesTy of
      TyVarTy _ -> pure Deferred
      _ -> withReified env psTy nTy $ \paramNames nDims -> do
        let nParams = length paramNames
            mFullDom  = reifyDisjunction env paramNames fullDomTy
            mBranches = reifyGroupedDisjunction env paramNames branchesTy
        tcPluginTrace "isl-plugin" (text $ "PartitionsU: fullDom=" ++ show (fmap length mFullDom)
                                       ++ " branches=" ++ show (fmap (map length) mBranches))
        case (mFullDom, mBranches) of
          (Just _fullDisj, Just []) ->
            traceProved "PartitionsU (direct)" >> (solvedWith ct)
          (Just fullDisj, Just branchGroups) -> do
            tcPluginTrace "isl-plugin" (text $ "PartitionsU: " ++ show (length branchGroups) ++ " branches")
            -- Mirror the WantedPartitions solver structure exactly,
            -- but build each branch as a union set (for LiteralU support).
            let ctx = envCtxPtr env
                rawBranches = [buildUnionSetFromDisj ctx nParams nDims paramNames disj | disj <- branchGroups]
                n = length rawBranches
                nPerBranchCopy = n + 1
                branchCopies = map (duplicateN nPerBranchCopy) rawBranches
                splitCopies (x:y:rest) = (x, y, rest)
                splitCopies _ = error "duplicateN: impossible (n+1 >= 2)"
                (unionInputs, failureUnionInputs, disjStacks) =
                  unzip3 (map splitCopies branchCopies)
                nPairs = (n * (n - 1)) `div` 2
                nFullDomCopies = 2 + nPairs
                rawFullDom = buildUnionSetFromDisj ctx nParams nDims paramNames fullDisj
                fullDomCopies = duplicateN nFullDomCopies rawFullDom
                fullDomCoverage = fullDomCopies !! 0
                fullDomFailure = fullDomCopies !! 1
                fullDomForPairs = drop 2 fullDomCopies
                buildUnionFromList = \case
                  []     -> buildUnionSetFromDisj ctx nParams nDims paramNames []
                  [x]    -> x
                  (x:xs) -> foldl (\acc s -> runRawIsl1 ctx $ S.union acc s) x xs
                branchesCoverage = buildUnionFromList unionInputs
                coverage = S.isSubset (setRef fullDomCoverage) (setRef branchesCoverage)
            if not coverage
              then do
                let branchesFailure = buildUnionFromList failureUnionInputs
                    uncovered = runRawIsl1 ctx $ S.subtract fullDomFailure branchesFailure
                !domStr <- mkStr $ islSetToStr fullDomCoverage
                !brnStr <- mkStr $ islSetToStr branchesCoverage
                !uncStr <- mkStr $ islSetToStr uncovered
                traceFailed "PartitionsU" $
                  "\n  Domain:    " ++ domStr ++
                  "\n  Branches:  " ++ brnStr ++
                  "\n  Uncovered: " ++ uncStr
                failedWith env ct
                  [ "IslPartitionsU failed: branches do not cover the domain"
                  , "  Domain:    " ++ domStr
                  , "  Branches:  " ++ brnStr
                  , "  Uncovered: " ++ uncStr
                  ]
              else if n < 2
                then traceProved "PartitionsU" >> (solvedWith ct)
                else do
                  let pairs = [(i, j) | i <- [0 .. n - 2], j <- [i + 1 .. n - 1]]
                      replaceAt idx v xs = take idx xs ++ [v] ++ drop (idx + 1) xs
                      takeCopy stacks idx = case stacks !! idx of
                        (x : xs) -> (x, replaceAt idx xs stacks)
                        []       -> error "isl-plugin: PartitionsU: exhausted dup copies"
                      go [] _ _ = pure True
                      go ((i, j) : rest) stacks (fd : fds) = do
                        let (bi, stacks1) = takeCopy stacks i
                            (bj, stacks2) = takeCopy stacks1 j
                            inter = runRawIsl1 ctx $ S.intersect bi bj
                            withFD = runRawIsl1 ctx $ S.intersect inter fd
                        if S.isEmpty (setRef withFD)
                          then go rest stacks2 fds
                          else pure False
                      go _ _ [] = error "isl-plugin: PartitionsU: exhausted fullDom copies"
                  allDisjoint <- go pairs disjStacks fullDomForPairs
                  if allDisjoint
                    then traceProved "PartitionsU" >> (solvedWith ct)
                    else do
                      traceFailed "PartitionsU" "\n  Branches are not pairwise disjoint"
                      failedWith env ct
                        [ "IslPartitionsU failed: branches are not pairwise disjoint" ]
          _ -> traceReifyFail >> pure Deferred

  WantedCoversU ct psTy nTy fullDomTy branchesTy ->
    withReified env psTy nTy $ \paramNames nDims -> do
      let nParams = length paramNames
          mFullDom  = reifyDisjunction env paramNames fullDomTy
          mBranches = reifyGroupedDisjunction env paramNames branchesTy
      case (mFullDom, mBranches) of
        (Just fullDisj, Just branchGroups) -> do
          let ctx = envCtxPtr env
              fullDomSet = buildUnionSetFromDisj ctx nParams nDims paramNames fullDisj
              allBranchSets = [buildUnionSetFromDisj ctx nParams nDims paramNames disj | disj <- branchGroups]
              branchesUnion = case allBranchSets of
                []     -> buildUnionSetFromDisj ctx nParams nDims paramNames []
                [x]    -> x
                (x:xs) -> foldl (\acc s -> runRawIsl1 ctx $ S.union acc s) x xs
              result = S.isSubset (setRef fullDomSet) (setRef branchesUnion)
          if result
            then traceProved "CoversU" >> (solvedWith ct)
            else do
              let fullDomSet' = buildUnionSetFromDisj ctx nParams nDims paramNames fullDisj
                  allBranchSets' = [buildUnionSetFromDisj ctx nParams nDims paramNames disj | disj <- branchGroups]
                  branchesUnion' = case allBranchSets' of
                    []     -> buildUnionSetFromDisj ctx nParams nDims paramNames []
                    [x]    -> x
                    (x:xs) -> foldl (\acc s -> runRawIsl1 ctx $ S.union acc s) x xs
                  uncovered = runRawIsl1 ctx $ S.subtract fullDomSet' branchesUnion'
              !domStr <- mkStr $ islSetToStr fullDomSet
              !brnStr <- mkStr $ islSetToStr branchesUnion
              !uncStr <- mkStr $ islSetToStr uncovered
              traceFailed "CoversU" $
                "\n  Domain:    " ++ domStr ++
                "\n  Branches:  " ++ brnStr
              failedWith env ct
                [ "IslCoversU failed: branches do not cover the domain"
                , "  Domain:    " ++ domStr
                , "  Branches:  " ++ brnStr
                , "  Uncovered: " ++ uncStr
                ]
        _ -> traceReifyFail >> pure Deferred

  WantedImageSubsetU ct psTy niTy noTy mapCsTy srcCssTy dstCsTy ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let nParams = length paramNames
          mMapCs = reifyMapConstraintList env paramNames nIn (unfoldTypeList mapCsTy)
          mSrcCss = reifyDisjunction env paramNames srcCssTy
          mDstCs = reifyConstraintList env paramNames (unfoldTypeList dstCsTy)
      case (mMapCs, mSrcCss, mDstCs) of
        (Just mapCs, Just srcCss, Just dstCs) -> do
          let ctx   = envCtxPtr env
              image = runRawIsl1 ctx $
                S.apply (buildUnionSetFromDisj ctx nParams nIn paramNames srcCss)
                        (buildMap ctx nParams nIn nOut paramNames mapCs)
              dst   = buildSet ctx nParams nOut paramNames dstCs
              result = S.isSubset (setRef image) (setRef dst)
          if result
            then traceProved "ImageSubsetU" >> (solvedWith ct)
            else do
              !mapStr  <- mkStr $ islMapToStr (buildMap ctx nParams nIn nOut paramNames mapCs)
              !srcStr  <- mkStr $ islSetToStr (buildUnionSetFromDisj ctx nParams nIn paramNames srcCss)
              !imgStr  <- mkStr $ islSetToStr image
              !dstStr  <- mkStr $ islSetToStr dst
              !diffStr <- mkStr $ islSetToStr $ runRawIsl1 ctx $
                S.subtract
                  (runRawIsl1 ctx $ S.apply
                    (buildUnionSetFromDisj ctx nParams nIn paramNames srcCss)
                    (buildMap ctx nParams nIn nOut paramNames mapCs))
                  (buildSet ctx nParams nOut paramNames dstCs)
              traceFailed "ImageSubsetU" $
                "\n  Map:    " ++ mapStr ++
                "\n  Source: " ++ srcStr ++
                "\n  Image:  " ++ imgStr ++
                "\n  Target: " ++ dstStr ++
                "\n  Diff:   " ++ diffStr
              failedWith env ct
                [ "IslImageSubsetU failed: image of source under map is not a subset of target"
                , "  Map:           " ++ mapStr
                , "  Source:        " ++ srcStr
                , "  Image:         " ++ imgStr
                , "  Target:        " ++ dstStr
                , "  Counterexample:" ++ diffStr
                ]
        _ -> traceReifyFail >> pure Deferred

  -- === Multi-aff obligations ===

  WantedMultiAffEqual ct psTy niTy noTy es1Ty es2Ty ->
    withReifiedMap env psTy niTy noTy $ \paramNames nIn nOut -> do
      let mEs1 = reifyTExprList env paramNames (unfoldTypeList es1Ty)
          mEs2 = reifyTExprList env paramNames (unfoldTypeList es2Ty)
      case (mEs1, mEs2) of
        (Just es1, Just es2) -> do
          let ctx = envCtxPtr env
              ma1 = buildMultiAff ctx (length paramNames) nIn nOut paramNames es1
              ma2 = buildMultiAff ctx (length paramNames) nIn nOut paramNames es2
              result = MA.plainIsEqual (multiAffRef ma1) (multiAffRef ma2)
          if result
            then traceProved "MultiAffEqual" >> (solvedWith ct)
            else do
              !lhsStr <- mkStr $ MA.toStr (multiAffRef ma1)
              !rhsStr <- mkStr $ MA.toStr (multiAffRef ma2)
              traceFailed "MultiAffEqual" $
                "\n  LHS: " ++ lhsStr ++
                "\n  is NOT equal to" ++
                "\n  RHS: " ++ rhsStr
              failedWith env ct
                [ "IslMultiAffEqual failed: multi-affine functions are not equal"
                , "  LHS: " ++ lhsStr
                , "  RHS: " ++ rhsStr
                ]
        _ -> traceReifyFail >> pure Deferred

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

-- | Reify a list of type-level TExpr to value-level [Expr SetIx].
reifyTExprList :: IslPluginEnv -> [String] -> [Type] -> Maybe [Expr SetIx]
reifyTExprList env paramNames = mapM (reifyTExpr env paramNames)

-- | Reify a disjunction (list of conjunctions) from type-level to value-level.
-- Returns 'Nothing' if the type is a stuck type family application.
reifyDisjunction :: IslPluginEnv -> [String] -> Type -> Maybe [[Constraint SetIx]]
reifyDisjunction env paramNames ty = do
  outerList <- unfoldTypeListMaybe ty
  mapM (\conjTy -> reifyConstraintList env paramNames (unfoldTypeList conjTy)) outerList

-- | Reify a grouped disjunction (list of disjunctions) from type-level to value-level.
-- Parses @[[[TConstraint ps n]]]@ — a list of per-branch disjunctions.
reifyGroupedDisjunction :: IslPluginEnv -> [String] -> Type -> Maybe [[[Constraint SetIx]]]
reifyGroupedDisjunction env paramNames ty = do
  outerList <- unfoldTypeListMaybe ty
  mapM (\branchTy -> reifyDisjunction env paramNames branchTy) outerList

-- | Reify a map disjunction from type-level to value-level.
reifyMapDisjunction :: IslPluginEnv -> [String] -> Int -> Type -> Maybe [[Constraint MapIx]]
reifyMapDisjunction env paramNames nIn ty = do
  outerList <- unfoldTypeListMaybe ty
  mapM (\conjTy -> reifyMapConstraintList env paramNames nIn (unfoldTypeList conjTy)) outerList

-- | Build an ISL union set from a disjunction of conjunctions.
-- Empty disjunction = empty set.
buildUnionSetFromDisj :: Isl.Ctx -> Int -> Int -> [String] -> [[Constraint SetIx]] -> Isl.Set
buildUnionSetFromDisj ctx nParams nDims paramNames disj =
  case [buildSet ctx nParams nDims paramNames conj | conj <- disj] of
    []     -> runRawIsl ctx $ IslL.do
                space0 <- Space.setAlloc (fromIntegral nParams) (fromIntegral nDims)
                space  <- IslL.foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                                 space0 (zip [0..] paramNames)
                s <- S.empty space
                urWrap s
    [x]    -> x
    (x:xs) -> foldl (\acc s -> runRawIsl1 ctx $ S.union acc s) x xs

-- | Build an ISL union map from a disjunction of conjunctions.
-- Empty disjunction = empty map.
buildUnionMapFromDisj :: Isl.Ctx -> Int -> Int -> Int -> [String] -> [[Constraint MapIx]] -> Isl.Map
buildUnionMapFromDisj ctx nParams nIn nOut paramNames disj =
  case [buildMap ctx nParams nIn nOut paramNames conj | conj <- disj] of
    []     -> runRawIsl ctx $ IslL.do
                space0 <- Space.alloc (fromIntegral nParams) (fromIntegral nIn) (fromIntegral nOut)
                space  <- IslL.foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                                 space0 (zip [0..] paramNames)
                m <- M.empty space
                urWrap m
    [x]    -> x
    (x:xs) -> foldl (\acc m -> runRawIsl1 ctx $ M.union acc m) x xs

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
-- Uses a bang pattern to force evaluation before the ISL object
-- can be freed by a consuming operation or CSE sharing.
islSetToStr :: Isl.Set -> String
islSetToStr s = let !r = S.toStr (setRef s) in r

-- | Convert an ISL map to its string representation for diagnostics.
islMapToStr :: Isl.Map -> String
islMapToStr m = let !r = M.toStr (mapRef m) in r

-- | Force a diagnostic string in IO.  ISL-to-string conversions must be
-- evaluated before their underlying ISL object is consumed downstream,
-- otherwise the lazy thunk captures a freed pointer (see
-- @feedback_isl_laziness_uaf@).
mkStr :: String -> TcPluginM String
mkStr = tcPluginIO . evaluate

-- * Custom type error construction

-- | Build an 'ErrorMessage' type from a list of lines.
-- Each line becomes @'Text "..."@ and lines are joined with @':$$:'@.
mkErrorMsgTy :: IslPluginEnv -> [String] -> Type
mkErrorMsgTy env lines_ = case lines_ of
    []     -> textTy ""
    [l]    -> textTy l
    (l:ls) -> foldl vappend (textTy l) (map textTy ls)
  where
    textTy s = mkTyConApp (envTypeErrorText env) [mkStrLitTy (mkFastString s)]
    vappend a b = mkTyConApp (envTypeErrorVApp env) [a, b]

-- | Emit a custom type error at the constraint's source location.
-- Uses 'TcRnUserTypeError' via 'unsafeTcPluginTcM' to produce
-- GHC's native TypeError rendering.
emitTypeError :: IslPluginEnv -> Ct -> [String] -> TcPluginM ()
emitTypeError env ct lines_ = do
  let rss = ctLocSpan (ctLoc ct)
      key = (srcSpanStartLine rss, srcSpanStartCol rss)
  alreadyEmitted <- tcPluginIO $ atomicModifyIORef' (envEmittedErrors env) $ \s ->
    if Set.member key s then (s, True) else (Set.insert key s, False)
  if alreadyEmitted then pure () else do
    let msgTy = mkErrorMsgTy env lines_
        span  = RealSrcSpan rss Strict.Nothing
    unsafeTcPluginTcM $ addErrAt span (TcRnUserTypeError msgTy)

-- * Evidence construction

makeEvidence :: Ct -> TcPluginM EvTerm
makeEvidence ct = do
  let pred_ = ctEvPred (ctEvidence ct)
  case classifyPredType pred_ of
    ClassPred cls args ->
      let [dc] = tyConDataCons (classTyCon cls)
          -- Each plugin-checked class now carries a single nullary
          -- method returning () (see D15).  In Core, the class
          -- dictionary's data constructor therefore takes one value
          -- argument of type ().  We supply () as that method body.
          -- Empty classes (no methods) still work — the list is just
          -- empty in that case.
          arity = dataConRepArity dc
          methodArgs = replicate arity unitCoreExpr
      in pure $ evDataConApp dc args methodArgs
    _ -> error "isl-plugin: makeEvidence called on non-class predicate"

-- | Core expression for @()@, used as a method body for the
-- nullary plugin-class methods (D15).
unitCoreExpr :: CoreExpr
unitCoreExpr = mkCoreConApps unitDataCon []

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
    -- Multi-aff rewriters
  , (envIslMultiAffToMap env,      rewriteMultiAffToMap env)
  , (envIslApplyMultiAff env,      rewriteApplyMultiAff env)
  , (envIslPreimageMultiAff env,   rewritePreimageMultiAff env)
  , (envIslComposeMultiAff env,    rewriteComposeMultiAff env)
  , (envIslMultiAffToString env,   rewriteMultiAffToString env)
  , (envIslMultiAffFromString env, rewriteMultiAffFromString env)
    -- PwAff rewriters
  , (envIslSetDimMax env, rewriteSetDimMax env)
  , (envIslSetDimMin env, rewriteSetDimMin env)
    -- Union-aware rewriters
  , (envIslIntersectSetU env,  rewriteIntersectSetU env)
  , (envIslComplementSetU env, rewriteComplementSetU env)
  , (envIslDifferenceSetU env, rewriteDifferenceSetU env)
  , (envIslApplyU env,         rewriteApplyU env)
  , (envIslDomainTFU env,      rewriteDomainU env)
  , (envIslRangeTFU env,       rewriteRangeU env)
  , (envIslComposeU env,       rewriteComposeU env)
  , (envIslReverseMapU env,    rewriteReverseMapU env)
  , (envIslProjectOutU env,    rewriteProjectOutU env)
  , (envIslToStringU env,      rewriteToStringU env)
  , (envIslMapToStringU env,   rewriteMapToStringU env)
  ]

-- | Helper: run a set computation and lift the result to a type.
-- @tfTyCon@ and @origArgs@ are used to reconstruct the original type family
-- application for the coercion: @co :: TF args ~ result@.
rewriteSetResult :: IslPluginEnv -> TyCon -> [Type]
  -> [String] -> Int -> Type -> Type
  -> (Isl.Ctx -> Isl.Set) -> TcPluginM TcPluginRewriteResult
rewriteSetResult env tfTyCon origArgs paramNames nDims psTy nTy mkSet = do
  result <- tcPluginIO $ pure $ decomposeIslSet env paramNames nDims (setRef (mkSet (envCtxPtr env)))
  let resultTy = liftDisjunction env paramNames psTy nTy result
      origTy = mkTyConApp tfTyCon origArgs
      co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
  pure $ TcPluginRewriteTo (Reduction co resultTy) []

-- | Helper: run a map computation and lift the result to a type.
-- Result is flat [TConstraint ps (ni+no)].
rewriteMapResult :: IslPluginEnv -> TyCon -> [Type]
  -> [String] -> Int -> Int -> Type
  -> (Isl.Ctx -> Isl.Map) -> TcPluginM TcPluginRewriteResult
rewriteMapResult env tfTyCon origArgs paramNames nIn nOut psTy mkMap = do
  let ctx = envCtxPtr env
      result = decomposeIslMap env paramNames nIn nOut (mapRef (mkMap ctx))
      combinedNTy = mkNumLitTy (fromIntegral (nIn + nOut))
      resultTy = liftMapDisjunction env paramNames nIn psTy combinedNTy result
      origTy = mkTyConApp tfTyCon origArgs
      co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
  pure $ TcPluginRewriteTo (Reduction co resultTy) []

-- | Reify + build pair for a type-level set argument.  Instantiate with
-- 'conjSet' for non-U rewriters (single conjunction) or 'disjSet' for U
-- rewriters (disjunction of conjunctions).
data SetSource a = SetSource
  { ssReify :: IslPluginEnv -> [String] -> Type -> Maybe a
  , ssBuild :: Isl.Ctx -> Int -> Int -> [String] -> a -> Isl.Set
  }

conjSet :: SetSource [Constraint SetIx]
conjSet = SetSource
  (\env pn ty -> reifyConstraintList env pn (unfoldTypeList ty))
  buildSet

disjSet :: SetSource [[Constraint SetIx]]
disjSet = SetSource reifyDisjunction buildUnionSetFromDisj

-- | Reify + build pair for a type-level map argument.
data MapSource a = MapSource
  { msReify :: IslPluginEnv -> [String] -> Int -> Type -> Maybe a
  , msBuild :: Isl.Ctx -> Int -> Int -> Int -> [String] -> a -> Isl.Map
  }

conjMap :: MapSource [Constraint MapIx]
conjMap = MapSource
  (\env pn nIn ty -> reifyMapConstraintList env pn nIn (unfoldTypeList ty))
  buildMap

disjMap :: MapSource [[Constraint MapIx]]
disjMap = MapSource reifyMapDisjunction buildUnionMapFromDisj

-- | Rewrite a unary set TF @(ps, n, cs)@ using @op@ to compute the result set.
rewriteUnarySetTF
  :: SetSource a
  -> (IslPluginEnv -> TyCon)
  -> (Isl.Set %1 -> IslT IO Isl.Set)
  -> IslPluginEnv -> TcPluginRewriter
rewriteUnarySetTF src tfOf op env _re _givens args = case args of
  [psTy, nTy, csTy]
    | Just nDims <- extractNat nTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just cs <- ssReify src env paramNames csTy ->
        rewriteSetResult env (tfOf env) args paramNames nDims psTy nTy $ \ctx ->
          runRawIsl1 ctx $ op (ssBuild src ctx nParams nDims paramNames cs)
  _ -> pure TcPluginNoRewrite

-- | Rewrite a binary set TF @(ps, n, cs1, cs2)@.
rewriteBinarySetTF
  :: SetSource a
  -> (IslPluginEnv -> TyCon)
  -> (Isl.Set %1 -> Isl.Set %1 -> IslT IO Isl.Set)
  -> IslPluginEnv -> TcPluginRewriter
rewriteBinarySetTF src tfOf op env _re _givens args = case args of
  [psTy, nTy, c1Ty, c2Ty]
    | Just nDims <- extractNat nTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just c1 <- ssReify src env paramNames c1Ty
    , Just c2 <- ssReify src env paramNames c2Ty ->
        rewriteSetResult env (tfOf env) args paramNames nDims psTy nTy $ \ctx ->
          runRawIsl1 ctx $
            op (ssBuild src ctx nParams nDims paramNames c1)
               (ssBuild src ctx nParams nDims paramNames c2)
  _ -> pure TcPluginNoRewrite

-- | Rewrite a Domain/Range-like TF @(ps, ni, no, mapCs)@ that projects a map
-- onto one of its sides.  @outDims@ picks nIn or nOut for the result space
-- and must match the tuple arity of the ISL op.
rewriteMapProjTF
  :: MapSource a
  -> (IslPluginEnv -> TyCon)
  -> (Int -> Int -> Int)               -- pick nIn or nOut from (nIn, nOut)
  -> (Isl.Map %1 -> IslT IO Isl.Set)
  -> IslPluginEnv -> TcPluginRewriter
rewriteMapProjTF src tfOf pickOut op env _re _givens args = case args of
  [psTy, niTy, noTy, csTy]
    | Just nIn  <- extractNat niTy
    , Just nOut <- extractNat noTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
          nRes       = pickOut nIn nOut
          resTy      = if nRes == nIn then niTy else noTy
    , Just cs <- msReify src env paramNames nIn csTy ->
        rewriteSetResult env (tfOf env) args paramNames nRes psTy resTy $ \ctx ->
          runRawIsl1 ctx $ op (msBuild src ctx nParams nIn nOut paramNames cs)
  _ -> pure TcPluginNoRewrite

-- | Rewrite an Apply-like TF @(ps, ni, no, mapCs, setCs)@: image of @setCs@
-- under @mapCs@.
rewriteApplyTF
  :: MapSource ma -> SetSource sa
  -> (IslPluginEnv -> TyCon)
  -> IslPluginEnv -> TcPluginRewriter
rewriteApplyTF ms ss tfOf env _re _givens args = case args of
  [psTy, niTy, noTy, mapCsTy, setCsTy]
    | Just nIn  <- extractNat niTy
    , Just nOut <- extractNat noTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just mapCs <- msReify ms env paramNames nIn mapCsTy
    , Just setCs <- ssReify ss env paramNames setCsTy ->
        rewriteSetResult env (tfOf env) args paramNames nOut psTy noTy $ \ctx ->
          let m = msBuild ms ctx nParams nIn nOut paramNames mapCs
              s = ssBuild ss ctx nParams nIn paramNames setCs
          in runRawIsl1 ctx $ S.apply s m
  _ -> pure TcPluginNoRewrite

-- | Rewrite a ProjectOut-like TF @(ps, n, nResult, first, count, cs)@.
rewriteProjectOutTF
  :: SetSource a
  -> (IslPluginEnv -> TyCon)
  -> IslPluginEnv -> TcPluginRewriter
rewriteProjectOutTF src tfOf env _re _givens args = case args of
  [psTy, nTy, nResultTy, firstTy, countTy, csTy]
    | Just nDims   <- extractNat nTy
    , Just nResult <- extractNat nResultTy
    , Just first   <- extractNat firstTy
    , Just count   <- extractNat countTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just cs <- ssReify src env paramNames csTy ->
        rewriteSetResult env (tfOf env) args paramNames nResult psTy nResultTy $ \ctx ->
          runRawIsl1 ctx $
            S.projectOut (ssBuild src ctx nParams nDims paramNames cs)
                         Isl.islDimSet (fromIntegral first) (fromIntegral count)
  _ -> pure TcPluginNoRewrite

-- | Rewrite a ReverseMap-like TF @(ps, ni, no, mapCs)@: reverses the map.
rewriteReverseMapTF
  :: MapSource a
  -> (IslPluginEnv -> TyCon)
  -> IslPluginEnv -> TcPluginRewriter
rewriteReverseMapTF src tfOf env _re _givens args = case args of
  [psTy, niTy, noTy, csTy]
    | Just nIn  <- extractNat niTy
    , Just nOut <- extractNat noTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just cs <- msReify src env paramNames nIn csTy ->
        rewriteMapResult env (tfOf env) args paramNames nOut nIn psTy $ \ctx ->
          runRawIsl1 ctx $ M.reverse (msBuild src ctx nParams nIn nOut paramNames cs)
  _ -> pure TcPluginNoRewrite

-- | Rewrite a Compose-like TF @(ps, ni, nk, no, m1Cs, m2Cs)@: @m1 ∘ m2@.
rewriteComposeTF
  :: MapSource a
  -> (IslPluginEnv -> TyCon)
  -> IslPluginEnv -> TcPluginRewriter
rewriteComposeTF src tfOf env _re _givens args = case args of
  [psTy, niTy, nkTy, noTy, m1CsTy, m2CsTy]
    | Just nIn  <- extractNat niTy
    , Just nK   <- extractNat nkTy
    , Just nOut <- extractNat noTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just m1 <- msReify src env paramNames nK  m1CsTy
    , Just m2 <- msReify src env paramNames nIn m2CsTy ->
        rewriteMapResult env (tfOf env) args paramNames nIn nOut psTy $ \ctx ->
          let mm1 = msBuild src ctx nParams nK  nOut paramNames m1
              mm2 = msBuild src ctx nParams nIn nK   paramNames m2
          in runRawIsl1 ctx $ M.applyRange mm2 mm1
  _ -> pure TcPluginNoRewrite

-- | Rewrite a ToString TF @(ps, n, cs)@ that renders a set as a Symbol.
rewriteSetToStringTF
  :: SetSource a
  -> (IslPluginEnv -> TyCon)
  -> IslPluginEnv -> TcPluginRewriter
rewriteSetToStringTF src tfOf env _re _givens args = case args of
  [psTy, nTy, csTy]
    | Just nDims <- extractNat nTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just cs <- ssReify src env paramNames csTy -> do
        let ctx      = envCtxPtr env
            str      = islSetToStr (ssBuild src ctx nParams nDims paramNames cs)
            resultTy = mkStrLitTy (mkFastString str)
            origTy   = mkTyConApp (tfOf env) args
            co       = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
        pure $ TcPluginRewriteTo (Reduction co resultTy) []
  _ -> pure TcPluginNoRewrite

rewriteApply :: IslPluginEnv -> TcPluginRewriter
rewriteApply = rewriteApplyTF conjMap conjSet envIslApply

rewriteIntersectSet :: IslPluginEnv -> TcPluginRewriter
rewriteIntersectSet = rewriteBinarySetTF conjSet envIslIntersectSet S.intersect

rewriteComplementSet :: IslPluginEnv -> TcPluginRewriter
rewriteComplementSet = rewriteUnarySetTF conjSet envIslComplementSet S.complement

rewriteDifferenceSet :: IslPluginEnv -> TcPluginRewriter
rewriteDifferenceSet = rewriteBinarySetTF conjSet envIslDifferenceSet S.subtract

rewriteDomain :: IslPluginEnv -> TcPluginRewriter
rewriteDomain = rewriteMapProjTF conjMap envIslDomainTF const M.domain

rewriteRange :: IslPluginEnv -> TcPluginRewriter
rewriteRange = rewriteMapProjTF conjMap envIslRangeTF (\_ no -> no) M.range

rewriteCompose :: IslPluginEnv -> TcPluginRewriter
rewriteCompose = rewriteComposeTF conjMap envIslCompose

rewriteReverseMap :: IslPluginEnv -> TcPluginRewriter
rewriteReverseMap = rewriteReverseMapTF conjMap envIslReverseMap

rewriteProjectOut :: IslPluginEnv -> TcPluginRewriter
rewriteProjectOut = rewriteProjectOutTF conjSet envIslProjectOut

-- | Rewrite IslFromString ps n str
-- Parses an ISL set from its string representation.
rewriteFromString :: IslPluginEnv -> TcPluginRewriter
rewriteFromString env _re _givens args = case args of
  [psTy, nTy, strTy] ->
    case (extractNat nTy, extractSymbol strTy) of
      (Just nDims, Just str) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
        rewriteSetResult env (envIslFromString env) args paramNames nDims psTy nTy $ \ctx ->
          runRawIsl1 ctx $ S.readFromStr str
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
                str = islSetToStr s
                resultTy = mkStrLitTy (mkFastString str)
                origTy = mkTyConApp (envIslToString env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslMapToString ps ni no cs → Symbol
--
-- Uses 'unfoldTypeListMaybe' for @csTy@ (not 'unfoldTypeList') so
-- that a stuck type-family application in @cs@ (e.g., an unreduced
-- @IslMultiAffToMap@) causes the rewriter to defer rather than
-- silently produce a map string for an unconstrained map.  See the
-- Haddock on 'unfoldTypeListMaybe' for the full story — this is the
-- fix for the v6 bug where the reflected-route 'islImageSubsetCheck'
-- mirror over-approximated rewritten body accesses because the map
-- text came back as @"[N] -> { [...] -> [...] }"@ with no output
-- equalities.
rewriteMapToString :: IslPluginEnv -> TcPluginRewriter
rewriteMapToString env _re _givens args = case args of
  [psTy, niTy, noTy, csTy] ->
    case (extractNat niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> case unfoldTypeListMaybe csTy of
        Nothing -> pure TcPluginNoRewrite   -- csTy is stuck; try again after inner reduction
        Just raw ->
          let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
              mCs = reifyMapConstraintList env paramNames nIn raw
          in case mCs of
               Just cs -> do
                 let ctx = envCtxPtr env
                     m = buildMap ctx (length paramNames) nIn nOut paramNames cs
                     str = islMapToStr m
                     resultTy = mkStrLitTy (mkFastString str)
                     origTy = mkTyConApp (envIslMapToString env) args
                     co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
                 pure $ TcPluginRewriteTo (Reduction co resultTy) []
               _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- * Union-aware rewriters

rewriteIntersectSetU :: IslPluginEnv -> TcPluginRewriter
rewriteIntersectSetU = rewriteBinarySetTF disjSet envIslIntersectSetU S.intersect

rewriteComplementSetU :: IslPluginEnv -> TcPluginRewriter
rewriteComplementSetU = rewriteUnarySetTF disjSet envIslComplementSetU S.complement

rewriteDifferenceSetU :: IslPluginEnv -> TcPluginRewriter
rewriteDifferenceSetU = rewriteBinarySetTF disjSet envIslDifferenceSetU S.subtract

rewriteApplyU :: IslPluginEnv -> TcPluginRewriter
rewriteApplyU = rewriteApplyTF disjMap disjSet envIslApplyU

rewriteDomainU :: IslPluginEnv -> TcPluginRewriter
rewriteDomainU = rewriteMapProjTF disjMap envIslDomainTFU const M.domain

rewriteRangeU :: IslPluginEnv -> TcPluginRewriter
rewriteRangeU = rewriteMapProjTF disjMap envIslRangeTFU (\_ no -> no) M.range

rewriteComposeU :: IslPluginEnv -> TcPluginRewriter
rewriteComposeU = rewriteComposeTF disjMap envIslComposeU

rewriteReverseMapU :: IslPluginEnv -> TcPluginRewriter
rewriteReverseMapU = rewriteReverseMapTF disjMap envIslReverseMapU

rewriteProjectOutU :: IslPluginEnv -> TcPluginRewriter
rewriteProjectOutU = rewriteProjectOutTF disjSet envIslProjectOutU

rewriteToStringU :: IslPluginEnv -> TcPluginRewriter
rewriteToStringU = rewriteSetToStringTF disjSet envIslToStringU

-- | Rewrite IslMapToStringU ps ni no css → Symbol.  Uses 'unfoldTypeList'
-- (total) here because U-disjunction reification already handles stuck
-- outer applications; only 'rewriteMapToString' needs the extra guard.
rewriteMapToStringU :: IslPluginEnv -> TcPluginRewriter
rewriteMapToStringU env _re _givens args = case args of
  [psTy, niTy, noTy, cssTy]
    | Just nIn  <- extractNat niTy
    , Just nOut <- extractNat noTy
    , let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
          nParams    = length paramNames
    , Just css <- reifyMapDisjunction env paramNames nIn cssTy -> do
        let ctx      = envCtxPtr env
            m        = buildUnionMapFromDisj ctx nParams nIn nOut paramNames css
            str      = islMapToStr m
            resultTy = mkStrLitTy (mkFastString str)
            origTy   = mkTyConApp (envIslMapToStringU env) args
            co       = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
        pure $ TcPluginRewriteTo (Reduction co resultTy) []
  _ -> pure TcPluginNoRewrite


-- * Multi-aff rewriters

-- | Rewrite IslMultiAffToMap ps ni no es → [TConstraint ps (ni+no)]
rewriteMultiAffToMap :: IslPluginEnv -> TcPluginRewriter
rewriteMultiAffToMap env _re _givens args = case args of
  [psTy, niTy, noTy, esTy] ->
    case (extractNat niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mEs = reifyTExprList env paramNames (unfoldTypeList esTy)
        case mEs of
          Just es -> do
            let ctx = envCtxPtr env
                nParams = length paramNames
                ma = buildMultiAff ctx nParams nIn nOut paramNames es
                bm = runRawIsl1 ctx $ BM.fromMultiAff ma
                m  = runRawIsl1 ctx $ M.fromBasicMap bm
                mapCs = case decomposeIslMap env paramNames nIn nOut (mapRef m) of
                  [single] -> single
                  _        -> error "isl-plugin: IslMultiAffToMap: expected single conjunction from multi_aff"
                combinedNTy = mkNumLitTy (fromIntegral (nIn + nOut))
                resultTy = mkPromotedListTy
                  (mkTyConApp (envTConstraintTC env) [psTy, combinedNTy])
                  (map (liftMapConstraint env paramNames nIn psTy combinedNTy) mapCs)
                origTy = mkTyConApp (envIslMultiAffToMap env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslApplyMultiAff ps ni no es setCs → [[TConstraint ps no]]
rewriteApplyMultiAff :: IslPluginEnv -> TcPluginRewriter
rewriteApplyMultiAff env _re _givens args = case args of
  [psTy, niTy, noTy, esTy, setCsTy] ->
    case (extractNat niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            nParams = length paramNames
            mEs = reifyTExprList env paramNames (unfoldTypeList esTy)
            mSetCs = reifyConstraintList env paramNames (unfoldTypeList setCsTy)
        case (mEs, mSetCs) of
          (Just es, Just setCs) ->
            rewriteSetResult env (envIslApplyMultiAff env) args paramNames nOut psTy noTy $ \ctx ->
              let ma  = buildMultiAff ctx nParams nIn nOut paramNames es
                  m   = runRawIsl1 ctx $ M.fromMultiAff ma
                  s   = buildSet ctx nParams nIn paramNames setCs
              in runRawIsl1 ctx $ S.apply s m
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslPreimageMultiAff ps ni no es setCs → [TConstraint ps ni]
-- Given multi-aff f : Z^ni → Z^no and set S ⊆ Z^no, computes {x ∈ Z^ni : f(x) ∈ S}.
-- Returns a single conjunction (the preimage of a basic set under an affine map
-- is always a basic set).
-- Implementation: convert to map, reverse, apply — avoids ISL set/map space mismatch.
rewritePreimageMultiAff :: IslPluginEnv -> TcPluginRewriter
rewritePreimageMultiAff env _re _givens args = case args of
  [psTy, niTy, noTy, esTy, setCsTy] ->
    case (extractNat niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            nParams = length paramNames
            mEs = reifyTExprList env paramNames (unfoldTypeList esTy)
            mSetCs = reifyConstraintList env paramNames (unfoldTypeList setCsTy)
        case (mEs, mSetCs) of
          (Just es, Just setCs) -> do
            -- Compute the preimage: reverse(fromMultiAff(f)) applied to S
            result <- tcPluginIO $ pure $
              let ctx = envCtxPtr env
                  ma  = buildMultiAff ctx nParams nIn nOut paramNames es
                  m   = runRawIsl1 ctx $ M.fromMultiAff ma
                  rev = runRawIsl1 ctx $ M.reverse m
                  s   = buildSet ctx nParams nOut paramNames setCs
                  res = runRawIsl1 ctx $ S.apply s rev
              in decomposeIslSet env paramNames nIn (setRef res)
            -- Return single conjunction (coalesce disjunction)
            let cs = case result of
                  [single] -> single
                  multi    -> concat multi  -- merge conjunctions (safe for basic-set results)
                elemKind = mkTyConApp (envTConstraintTC env) [psTy, niTy]
                resultTy = mkPromotedListTy elemKind
                  (map (liftConstraint env paramNames psTy niTy) cs)
                origTy = mkTyConApp (envIslPreimageMultiAff env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslComposeMultiAff ps ni nk no es1 es2 → [TExpr ps ni]
rewriteComposeMultiAff :: IslPluginEnv -> TcPluginRewriter
rewriteComposeMultiAff env _re _givens args = case args of
  [psTy, niTy, nkTy, noTy, es1Ty, es2Ty] ->
    case (extractNat niTy, extractNat nkTy, extractNat noTy) of
      (Just nIn, Just nK, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            nParams = length paramNames
            mEs1 = reifyTExprList env paramNames (unfoldTypeList es1Ty)
            mEs2 = reifyTExprList env paramNames (unfoldTypeList es2Ty)
        case (mEs1, mEs2) of
          (Just es1, Just es2) -> do
            let ctx = envCtxPtr env
                ma1 = buildMultiAff ctx nParams nK nOut paramNames es1
                ma2 = buildMultiAff ctx nParams nIn nK paramNames es2
                composed = runRawIsl1 ctx $ MA.pullbackMultiAff ma1 ma2
                resultExprs = decomposeIslMultiAff ctx nIn nParams composed
                resultTy = liftExprList env paramNames psTy niTy resultExprs
                origTy = mkTyConApp (envIslComposeMultiAff env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslMultiAffToString ps ni no es → Symbol
rewriteMultiAffToString :: IslPluginEnv -> TcPluginRewriter
rewriteMultiAffToString env _re _givens args = case args of
  [psTy, niTy, noTy, esTy] ->
    case (extractNat niTy, extractNat noTy) of
      (Just nIn, Just nOut) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            mEs = reifyTExprList env paramNames (unfoldTypeList esTy)
        case mEs of
          Just es -> do
            let ctx = envCtxPtr env
                ma = buildMultiAff ctx (length paramNames) nIn nOut paramNames es
                str = MA.toStr (multiAffRef ma)
                resultTy = mkStrLitTy (mkFastString str)
                origTy = mkTyConApp (envIslMultiAffToString env) args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslMultiAffFromString ps ni no str → [TExpr ps ni]
rewriteMultiAffFromString :: IslPluginEnv -> TcPluginRewriter
rewriteMultiAffFromString env _re _givens args = case args of
  [psTy, niTy, noTy, strTy] ->
    case (extractNat niTy, extractNat noTy, extractSymbol strTy) of
      (Just nIn, Just nOut, Just str) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            ctx = envCtxPtr env
            ma = runRawIsl1 ctx $ MA.readFromStr str
            resultExprs = decomposeIslMultiAff ctx nIn (length paramNames) ma
            resultTy = liftExprList env paramNames psTy niTy resultExprs
            origTy = mkTyConApp (envIslMultiAffFromString env) args
            co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
        pure $ TcPluginRewriteTo (Reduction co resultTy) []
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- | Rewrite IslSetDimMax ps n d cs → [(  [TConstraint ps n], TExpr ps n )]
rewriteSetDimMax :: IslPluginEnv -> TcPluginRewriter
rewriteSetDimMax env = rewriteSetDimOpt env True

-- | Rewrite IslSetDimMin ps n d cs → [( [TConstraint ps n], TExpr ps n )]
rewriteSetDimMin :: IslPluginEnv -> TcPluginRewriter
rewriteSetDimMin env = rewriteSetDimOpt env False

-- | Shared implementation for IslSetDimMax and IslSetDimMin.
rewriteSetDimOpt :: IslPluginEnv -> Bool -> TcPluginRewriter
rewriteSetDimOpt env isMax _re _givens args = case args of
  [psTy, nTy, dTy, csTy] ->
    case (extractNat nTy, extractNat dTy) of
      (Just nDims, Just d) -> do
        let paramNames = mapMaybe extractSymbol (unfoldTypeList psTy)
            nParams = length paramNames
            mDisj = reifyDisjunction env paramNames csTy
        case mDisj of
          Just disj -> do
            let ctx = envCtxPtr env
                -- Build union set from disjunction
                sets = [buildSet ctx nParams nDims paramNames conj | conj <- disj]
                s = case sets of
                  []     -> error "isl-plugin: IslSetDimMax/Min: empty disjunction"
                  [x]    -> x
                  (x:xs) -> foldl (\acc bs -> runRawIsl1 ctx $ S.union acc bs) x xs
                pa = runRawIsl1 ctx $
                  if isMax then S.dimMax s (fromIntegral d)
                           else S.dimMin s (fromIntegral d)
                -- Decompose PwAff into pieces: each piece is (Set domain, Aff expression)
                pieces = unsafePerformIO $ PA.foreachPiece (pwAffRef pa) $ \domSetRef affRef -> do
                  let domCs = decomposeIslSet env paramNames nDims domSetRef
                      expr  = decomposeIslAff ctx nDims nParams affRef
                  pure (domCs, expr)
                -- Build result: list of tuples (domain constraints, expression)
                -- Domain is a disjunction of conjunctions; flatten to one entry per conjunction
                flatPieces = [(conj, expr) | (domDisj, expr) <- pieces, conj <- domDisj]
                elemKind = mkTyConApp (envTConstraintTC env) [psTy, nTy]
                exprKind = mkTyConApp (envTExprTC env) [psTy, nTy]
                conjKind = mkPromotedListTy elemKind []
                pairKind = mkTyConApp (promotedTupleDataCon Boxed 2) [conjKind, exprKind]
                resultElems = [mkTyConApp (promotedTupleDataCon Boxed 2)
                                 [ conjKind, exprKind
                                 , mkPromotedListTy elemKind
                                     (map (liftConstraint env paramNames psTy nTy) conj)
                                 , liftExpr env paramNames psTy nTy expr
                                 ]
                              | (conj, expr) <- flatPieces]
                resultTy = mkPromotedListTy pairKind resultElems
                tfTyCon = if isMax then envIslSetDimMax env else envIslSetDimMin env
                origTy = mkTyConApp tfTyCon args
                co = mkUnivCo (PluginProv "isl-plugin") Nominal origTy resultTy
            pure $ TcPluginRewriteTo (Reduction co resultTy) []
          _ -> pure TcPluginNoRewrite
      _ -> pure TcPluginNoRewrite
  _ -> pure TcPluginNoRewrite

-- * Decomposing ISL results back to value-level constraints

decomposeIslSet :: IslPluginEnv -> [String] -> Int -> Isl.SetRef -> [[Constraint SetIx]]
decomposeIslSet _env paramNames nDims sRef =
  let nParams = length paramNames
  in unsafePerformIO $ S.foreachBasicSet sRef $ \bsRef -> do
       divExprs <- extractSetDivs bsRef nDims nParams
       BS.foreachConstraint bsRef $ \cRef ->
         extractSetConstraint nParams nDims divExprs cRef

decomposeIslMap :: IslPluginEnv -> [String] -> Int -> Int -> Isl.MapRef -> [[Constraint MapIx]]
decomposeIslMap _env paramNames nIn nOut mRef =
  let nParams = length paramNames
  in unsafePerformIO $ M.foreachBasicMap mRef $ \bmRef -> do
       divExprs <- extractMapDivs bmRef nIn nOut nParams
       BM.foreachConstraint bmRef $ \cRef ->
         extractMapConstraint nParams nIn nOut divExprs cRef

-- * Lifting value-level constraints to GHC types

-- | Lift a disjunction (list of conjunctions) to a promoted list of lists type.
-- Result kind: [[TConstraint ps n]]
liftDisjunction :: IslPluginEnv -> [String] -> Type -> Type -> [[Constraint SetIx]] -> Type
liftDisjunction env paramNames psTy nTy css =
  let elemKind = mkTyConApp (envTConstraintTC env) [psTy, nTy]
      innerListKind = mkTyConApp listTyCon [elemKind]
  in mkPromotedListTy innerListKind
       [mkPromotedListTy elemKind
          (map (liftConstraint env paramNames psTy nTy) cs)
       | cs <- css]

-- | Lift a map disjunction. Map constraints use flat dim indexing (0..ni+no-1).
-- The nTy parameter should be the combined (ni+no) type.
liftMapDisjunction :: IslPluginEnv -> [String] -> Int -> Type -> Type -> [[Constraint MapIx]] -> Type
liftMapDisjunction env paramNames nIn psTy combinedNTy css =
  let elemKind = mkTyConApp (envTConstraintTC env) [psTy, combinedNTy]
      innerListKind = mkTyConApp listTyCon [elemKind]
  in mkPromotedListTy innerListKind
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

-- | Lift a list of value-level Expr SetIx to a promoted list type @'[e0, e1, ...]@
-- of kind @[TExpr ps ni]@.
liftExprList :: IslPluginEnv -> [String] -> Type -> Type -> [Expr SetIx] -> Type
liftExprList env paramNames psTy nTy exprs =
  let elemKind = mkTyConApp (envTExprTC env) [psTy, nTy]
  in mkPromotedListTy elemKind
       (map (liftExpr env paramNames psTy nTy) exprs)

-- | Build a promoted list type @'[x1, x2, ...]@ from element kind and elements.
mkPromotedListTy :: Type -> [Type] -> Type
mkPromotedListTy k = foldr (\x xs -> mkTyConApp promotedConsDataCon [k, x, xs])
                           (mkTyConApp promotedNilDataCon [k])

-- * Building ISL objects

-- ** Sets

buildBasicSet
  :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> Isl.BasicSet
buildBasicSet ctx nParams nDims paramNames constraints =
  runRawIsl ctx $ IslL.do
    space0 <- Space.setAlloc (fromIntegral nParams) (fromIntegral nDims)
    space  <- IslL.foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                     space0 (zip [0..] paramNames)
    univ   <- BS.universe space
    result <- IslL.foldM addSetConstraint univ constraints
    urWrap result

buildSet :: Isl.Ctx -> Int -> Int -> [String] -> [Constraint SetIx] -> Isl.Set
buildSet ctx nParams nDims paramNames constraints =
  runRawIsl1 ctx $ S.fromBasicSet (buildBasicSet ctx nParams nDims paramNames constraints)

-- ** Maps

buildBasicMap
  :: Isl.Ctx -> Int -> Int -> Int -> [String] -> [Constraint MapIx] -> Isl.BasicMap
buildBasicMap ctx nParams nIn nOut paramNames constraints =
  runRawIsl ctx $ IslL.do
    space0 <- Space.alloc (fromIntegral nParams) (fromIntegral nIn) (fromIntegral nOut)
    space  <- IslL.foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                     space0 (zip [0..] paramNames)
    univ   <- BM.universe space
    result <- IslL.foldM addMapConstraint univ constraints
    urWrap result

buildMap :: Isl.Ctx -> Int -> Int -> Int -> [String] -> [Constraint MapIx] -> Isl.Map
buildMap ctx nParams nIn nOut paramNames constraints =
  runRawIsl1 ctx $ M.fromBasicMap (buildBasicMap ctx nParams nIn nOut paramNames constraints)

-- ** Multi-affs

-- | Build an ISL MultiAff from a list of output expressions.
-- Each expression is an affine function of the input dims and params.
buildMultiAff
  :: Isl.Ctx -> Int -> Int -> Int -> [String] -> [Expr SetIx] -> Isl.MultiAff
buildMultiAff ctx nParams nIn nOut paramNames exprs =
  runRawIsl ctx $ IslL.do
    space0 <- Space.alloc (fromIntegral nParams) (fromIntegral nIn) (fromIntegral nOut)
    space  <- IslL.foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                     space0 (zip [0..] paramNames)
    ma0 <- MA.zero space
    result <- IslL.foldM (\ma (j, expr) -> IslL.do
      Both (Ur maRef) ma' <- IslL.query ma (\r -> r)
      domSpace <- MA.getDomainSpace maRef
      ls       <- LS.fromSpace domSpace
      aff      <- exprToSetAff ls expr
      MA.setAff ma' (fromIntegral j) aff
      ) ma0 (zip [0..] exprs)
    urWrap result

-- | Extract output expressions from an ISL MultiAff.
decomposeIslMultiAff :: Isl.Ctx -> Int -> Int -> Isl.MultiAff -> [Expr SetIx]
decomposeIslMultiAff ctx nIn nParams ma =
  let nOut = MA.dim (multiAffRef ma) Isl.islDimOut
  in [decomposeOneAff ctx nIn nParams ma j | j <- [0..nOut-1]]

-- | Extract a single output aff from a MultiAff and convert to Expr SetIx.
decomposeOneAff :: Isl.Ctx -> Int -> Int -> Isl.MultiAff -> Int -> Expr SetIx
decomposeOneAff ctx nIn nParams ma j = unsafePerformIO $ do
  aff <- MA.multiAffGetAffCopy (multiAffRef ma) (fromIntegral j)
  let ar = affRef aff
  dimCoeffs <- mapM (\i -> do
    v <- Aff.affGetCoefficientSi ar Isl.islDimIn i
    pure (v, Ix (SetDim i))) [0..nIn-1]
  paramCoeffs <- mapM (\i -> do
    v <- Aff.affGetCoefficientSi ar Isl.islDimParam i
    pure (v, Ix (SetParam i))) [0..nParams-1]
  constant <- Aff.affGetConstantSi ar
  IslU.consume aff
  let allTerms = [(v, e) | (v, e) <- dimCoeffs ++ paramCoeffs, v /= 0]
  pure $ rebuildExprWithDivs allTerms constant

-- | Extract an Expr SetIx from a standalone Aff (used for PwAff pieces).
-- The Aff uses isl_dim_in for set dimensions.
decomposeIslAff :: Isl.Ctx -> Int -> Int -> Isl.AffRef -> Expr SetIx
decomposeIslAff _ctx nDims nParams ar = unsafePerformIO $ do
  dimCoeffs <- mapM (\i -> do
    v <- Aff.affGetCoefficientSi ar Isl.islDimIn i
    pure (v, Ix (SetDim i))) [0..nDims-1]
  paramCoeffs <- mapM (\i -> do
    v <- Aff.affGetCoefficientSi ar Isl.islDimParam i
    pure (v, Ix (SetParam i))) [0..nParams-1]
  constant <- Aff.affGetConstantSi ar
  let allTerms = [(v, e) | (v, e) <- dimCoeffs ++ paramCoeffs, v /= 0]
  pure $ rebuildExprWithDivs allTerms constant

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

-- | Strict variant of 'unfoldTypeList' that distinguishes "concrete
-- empty list" from "stuck / not a list literal".
--
-- Returns 'Nothing' when @ty@ is neither @'[]@, @'[]@, nor a cons
-- @':'@.  This is the case when @ty@ is, for example, a stuck
-- type-family application like @IslMultiAffToMap ps ni no es@ that
-- has not yet been reduced by its own rewriter.
--
-- Using 'unfoldTypeList' in a rewriter that consumes a list-kinded
-- argument silently confuses "no constraints" with "arg not yet
-- reduced", producing wrong results rather than deferring (this was
-- the v6 bug in @rewriteMapToString@: its stuck @csTy@ silently
-- decoded as an empty list, @buildMap@ produced an unconstrained
-- map, and the reflected-route 'islImageSubsetCheck' rejected valid
-- accesses).  Rewriters that must not make that mistake should use
-- this variant and return 'TcPluginNoRewrite' on 'Nothing'.
unfoldTypeListMaybe :: Type -> Maybe [Type]
unfoldTypeListMaybe (CastTy ty _) = unfoldTypeListMaybe ty
unfoldTypeListMaybe ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | getOccString tc == ":" , length args >= 3 ->
        let (x, xs) = lastTwoArgs args
        in (x :) <$> unfoldTypeListMaybe xs
  Just (tc, _)
    | getOccString tc == "'[]" -> Just []
    | getOccString tc == "[]"  -> Just []
  _ -> Nothing
