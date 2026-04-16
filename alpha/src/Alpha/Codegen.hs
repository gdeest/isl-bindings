{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | C code generation from Alpha systems via ISL AST builder.
module Alpha.Codegen
  ( codegen
  , CodegenError(..)
  , BoundError(..)
    -- * Reduction metadata (exposed for validation / external callers)
  , ReduceInfo(..)
  , buildReduceMap
  ) where

import Control.DeepSeq (NFData(..))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Typed.Constraints (NamedSet(..), NamedMap(..), buildUnionMapFromNamed)
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols, symbolVals)
import qualified Isl.UnionMap as UM
import Isl.Monad (IslT, Ur(..), runIslT, unsafeIslFromIO)
import qualified Isl.Linear as Isl
import Isl.Types (AstNode(..), Ctx)
import Isl.AstBuild (astBuildAlloc, astBuildNodeFromScheduleMap, CNode(..), walkAstNode, astNodeFree)

import Isl.TypeLevel.Reflection (reflectDomConstraints)
import Alpha.Core
import Alpha.Lower (lowerSystem)
import Alpha.Schedule (Schedule(..), EqSchedule(..), DimAnnotation(..))
import Alpha.Allocation (Allocation(..), EqStorage(..))
import qualified Alpha.Polyhedral.Schedule as S
import Alpha.Codegen.CRender (renderCNodeToC)
import Alpha.Codegen.ExprRender (RenderCtx(..), renderEquationMacro, BoundErr(..))
import Alpha.Codegen.FunctionMapping
  ( CFunctionMapping(..), ArgPassing(..), declListBoundsM )
import Alpha.Scalar (ScalarDesc(..), cTypeName)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Error type
-- ═══════════════════════════════════════════════════════════════════════

data CodegenError
  = CodegenScheduleError !String
  | CodegenInternalError !String
  | ConflictingAnnotation !Int !DimAnnotation !DimAnnotation
    -- ^ Two equations annotate the same schedule dim with different values.
  | BoundExtractionFailed !String !Int !BoundError
    -- ^ ISL @dim_max@ failed (piecewise / parse / unbounded) for a
    -- named variable's given dimension.
  | MissingScalarDesc !String
    -- ^ @codegen@ was called without a 'ScalarDesc' entry in @descs@
    -- for the named equation.  Surfaced from 'generateFromEqList'
    -- instead of an @error@ call (#16).
  | MissingReduceIdentity !ReduceOp !String
    -- ^ The 'ScalarDesc' for the named reduction accumulator does
    -- not define an identity for the given 'ReduceOp'
    -- (@sdReduceIdentity@ is 'Nothing' or returns 'Nothing') — #2.
    -- The second argument is the C type name (for debugging).
  deriving (Show, Eq)

-- | Reason 'extractBoundsISLM' could not recover a single exclusive
-- upper bound for one dimension.
data BoundError
  = PieceCount !Int       -- ^ @isl_pw_aff_n_piece@ returned @n /= 1@.
  | BoundParseFailed !String  -- ^ ISL @pw_aff@ string didn't match expected shape.
  deriving (Show, Eq)

instance NFData BoundError where
  rnf (PieceCount k)      = rnf k
  rnf (BoundParseFailed s) = rnf s

instance NFData CodegenError where
  rnf (CodegenScheduleError s)          = rnf s
  rnf (CodegenInternalError s)          = rnf s
  rnf (ConflictingAnnotation k a b)     = rnf k `seq` rnf a `seq` rnf b
  rnf (BoundExtractionFailed v d err)   = rnf v `seq` rnf d `seq` rnf err
  rnf (MissingScalarDesc n)             = rnf n
  rnf (MissingReduceIdentity op ty)     = op `seq` rnf ty


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Public API
-- ═══════════════════════════════════════════════════════════════════════

codegen
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule
  -> Allocation
  -> CFunctionMapping
  -> Map.Map String ScalarDesc
  -> IO (Either CodegenError String)
codegen sys@(System decls eqs) sched alloc fmap' descs = runIslT $ Isl.do
  let (domains, _writes, _reads, _projections) = lowerSystem sys
      reduceMap = buildReduceMap eqs
      schedMaps = lowerScheduleMaps sched domains reduceMap
      params = symbolVals @ps

  -- Per-variable exclusive upper bounds from declared domains.
  -- Structural pattern match first; ISL @dim_max@ fallback runs inside
  -- this IslT action (no unsafePerformIO). Piecewise results surface as
  -- 'BoundExtractionFailed' via @isl_pw_aff_n_piece@, not a string heuristic.
  Ur iRes <- declListBoundsM params (dInputs decls)
  Ur oRes <- declListBoundsM params (dOutputs decls)
  Ur lRes <- declListBoundsM params (dLocals decls)
  let mergedBounds = do
        mi <- iRes; mo <- oRes; ml <- lRes
        Right (Map.unions [mi, mo, ml])
  case mergedBounds of
    Left (name, dim, berr) ->
      Isl.pure (Ur (Left (BoundExtractionFailed name dim (mapBoundErr berr))))
    Right domBounds -> Isl.do
      -- Build union schedule map
      -- The schedule NamedMaps include domain constraints in their
      -- conjunctions, so the map's domain IS the iteration domain.
      Ur schedUMs <- Isl.mapM buildUnionMapFromNamed schedMaps
      case schedUMs of
        [] -> Isl.pure (Ur (Left (CodegenInternalError "no schedule maps")))
        (first':rest') -> Isl.do
          schedUM <- Isl.foldM (\acc x -> UM.union acc x) first' rest'

          -- ISL AST builder (consumes both build and schedUM)
          build <- astBuildAlloc
          node <- astBuildNodeFromScheduleMap build schedUM
          -- walkAstNode borrows; astNodeFree consumes. Wrap in one
          -- unsafeIslFromIO to avoid linear multiplicity conflict.
          Ur cTree <- walkAndFree node

          -- Extract ISL-determined iterator names from CUser calls
          let stmtArgs = extractStmtArgs cTree
          case generateMacros sys sched alloc params domBounds stmtArgs descs of
            Left err -> Isl.pure (Ur (Left err))
            Right macros -> case buildPragmaMap sched reduceMap domBounds of
              Left err -> Isl.pure (Ur (Left err))
              Right pragmas ->
                let skeleton = renderCNodeToC pragmas cTree
                in case assembleCSource params fmap' alloc macros skeleton descs domBounds reduceMap of
                     Left err   -> Isl.pure (Ur (Left err))
                     Right cSrc -> Isl.pure (Ur (Right cSrc))

-- | Lift the local 'BoundErr' (ExprRender) into 'BoundError' (this
-- module's public enum).  Kept as a thin adapter so ExprRender needn't
-- depend on the top-level 'CodegenError'.
mapBoundErr :: BoundErr -> BoundError
mapBoundErr (BEPieceCount n)   = PieceCount n
mapBoundErr (BEParseFailed s)  = BoundParseFailed s


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Schedule lowering (factored from Compile.hs)
-- ═══════════════════════════════════════════════════════════════════════

-- | Lower schedule maps for codegen.
--
-- For reduction equations, the schedule is extended to cover the body
-- domain (which includes reduction dims).  The user-provided schedule
-- covers the equation's output dims; we append identity dims for the
-- reduction variables so ISL generates the reduction loop.
--
-- The body domain constraints are constructed by extending the equation
-- domain with the reduction dimensions' constraints from the lower pass.
lowerScheduleMaps :: Schedule -> [NamedSet] -> Map.Map String ReduceInfo -> [NamedMap]
lowerScheduleMaps (Schedule entries) domains reduceMap =
  concatMap lowerOne domains
  where
    lowerOne dom@(NamedSet { nsName = Just name }) =
      case Map.lookup name entries of
        Just eq ->
          case Map.lookup name reduceMap of
            Just ri ->
              -- Reduction equation: schedule the body domain with reduction dims.
              let nEq = nsNDims dom
                  nRedDims = riRedDims ri
                  -- Build body domain: equation dims + reduction dims,
                  -- with the full body domain constraints.
                  bodyDom = NamedSet
                    { nsName   = Just name
                    , nsParams = nsParams dom
                    , nsNDims  = nEq + nRedDims
                    , nsConjs  = riBodyConjs ri
                    }
                  origExprs = S.schedExprs (esDef eq)
                  redExprs  = [ C.Ix (C.InDim (nEq + r))
                              | r <- [0 .. nRedDims - 1] ]
                  extDef = S.ScheduleDef (origExprs ++ redExprs)
              in [S.schedToNamedMap' name bodyDom extDef]
            Nothing ->
              [S.schedToNamedMap' name dom (esDef eq)]
        Nothing -> []
    lowerOne _ = []

-- ═══════════════════════════════════════════════════════════════════════
-- §4. Macro generation
-- ═══════════════════════════════════════════════════════════════════════

-- | Extract statement name → iterator arg names from CUser nodes.
-- Parses ISL-generated calls like @"C(c1, c2, c3);"@.
extractStmtArgs :: CNode -> Map.Map String [String]
extractStmtArgs = go
  where
    go (CFor _ _ _ _ body)  = go body
    go (CIf _ thn mels)     = go thn <> maybe Map.empty go mels
    go (CBlock cs)           = foldMap go cs
    go (CUser stmt)          = case parseStmtCall stmt of
      Just (name, args) -> Map.singleton name args
      Nothing           -> Map.empty

    parseStmtCall :: String -> Maybe (String, [String])
    parseStmtCall s =
      let s' = filter (/= ';') (filter (/= ' ') s)
      in case break (== '(') s' of
        (name, '(':rest) -> case break (== ')') rest of
          (argStr, _) -> Just (name, splitOn ',' argStr)
        _ -> Nothing

    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn sep str = case break (== sep) str of
      (tok, [])     -> [tok]
      (tok, _:rest) -> tok : splitOn sep rest

generateMacros
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule
  -> Allocation
  -> [String]
  -> Map.Map String [String]
  -> Map.Map String [String]
  -> Map.Map String ScalarDesc
  -> Either CodegenError String
generateMacros (System _decls eqs) sched alloc params domBounds stmtArgs descs =
  let storMap = allocEntries alloc
  in unlines <$> generateFromEqList eqs sched storMap params domBounds stmtArgs descs

generateFromEqList
  :: forall ps decls defined.
     KnownSymbols ps
  => EqList ps decls defined
  -> Schedule
  -> Map.Map String EqStorage
  -> [String]
  -> Map.Map String [String]
  -> Map.Map String [String]
  -> Map.Map String ScalarDesc
  -> Either CodegenError [String]
generateFromEqList EqNil _ _ _ _ _ _ = Right []
generateFromEqList (Defines (Proxy :: Proxy name) body :& rest) sched storMap params domBounds stmtArgs descs =
  let eqName = symbolVal (Proxy @name)
      iterVars = case Map.lookup eqName stmtArgs of
        Just args -> args
        Nothing ->
          let nSchedDims = case Map.lookup eqName (schedEntries sched) of
                Just es -> esNTime es; Nothing -> 0
              nRedDims = case extractReduceInfo body of
                Just ri -> riRedDims ri; Nothing -> 0
          in ["c" ++ show i | i <- [0 .. nSchedDims + nRedDims - 1]]
      nOutDims = case Map.lookup eqName (schedEntries sched) of
        Just es -> esNIter es
        Nothing -> 0
  in case Map.lookup eqName descs of
    Nothing -> Left (MissingScalarDesc eqName)
    Just desc ->
      let ctx = RenderCtx
            { rcParams    = params
            , rcIterVars  = iterVars
            , rcStorage   = storMap
            , rcDomBounds = domBounds
            , rcDesc      = desc
            }
          macro = renderEquationMacro eqName nOutDims body ctx
      in (macro :) <$>
           generateFromEqList rest sched storMap params domBounds stmtArgs descs

-- | Extract reduction info from an equation body: number of reduction
-- dims and the body domain constraints (for ISL loop generation).
data ReduceInfo = ReduceInfo
  { riOp       :: !ReduceOp
  , riRedDims  :: !Int
  , riBodyConjs :: ![C.Conjunction C.SetIx]
  }

extractReduceInfo :: forall ps decls n d a. Alpha.Core.Expr ps decls n d a -> Maybe ReduceInfo
extractReduceInfo (Reduce op _ (inner :: Alpha.Core.Expr ps decls nBody dBody a)) =
  let nRed = fromIntegral (natVal (Proxy @nBody)) - fromIntegral (natVal (Proxy @n))
      bodyCs = reflectDomConstraints @ps @nBody @dBody
  in Just (ReduceInfo op nRed [C.Conjunction bodyCs])
extractReduceInfo _ = Nothing

-- | Build a map from equation name → reduction info.
buildReduceMap
  :: forall ps decls defined.
     EqList ps decls defined -> Map.Map String ReduceInfo
buildReduceMap EqNil = Map.empty
buildReduceMap (Defines (Proxy :: Proxy name) body :& rest) =
  let eqName = symbolVal (Proxy @name)
      m = buildReduceMap rest
  in case extractReduceInfo body of
    Just ri -> Map.insert eqName ri m
    Nothing -> m


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Pragma assembly
-- ═══════════════════════════════════════════════════════════════════════

-- | Build the per-CFor-depth pragma string map consumed by
-- 'renderCNodeToC'.  Walks per-equation annotations and, for each
-- 'ReductionParallel', synthesizes an @omp parallel for
-- reduction(op:buf[:size]) schedule(static)@ clause using the
-- equation's reduction op and its output buffer / size.
--
-- Returns 'Left' on genuinely conflicting pragmas at the same dim
-- (e.g. @Parallel@ on eq A vs @Vectorize@ on eq B at the shared
-- outer dim).
buildPragmaMap
  :: Schedule
  -> Map.Map String ReduceInfo
  -> Map.Map String [String]
  -> Either CodegenError (Map.Map Int String)
buildPragmaMap (Schedule entries) redMap domBounds =
  -- Intermediate map tracks the source annotation so a conflict at dim k
  -- can be reported with both the prior and new 'DimAnnotation'.
  fmap (fmap fst) (foldr stepEq (Right Map.empty) (Map.toList entries))
  where
    stepEq _          (Left e)    = Left e
    stepEq (name, es) (Right acc) =
      Map.foldrWithKey (insertOne name) (Right acc) (esAnnotations es)

    insertOne _    _ _   (Left e)    = Left e
    insertOne name k ann (Right acc) = case mkPragma name ann of
      Left e       -> Left e
      Right pragma -> case Map.lookup k acc of
        Nothing        -> Right (Map.insert k (pragma, ann) acc)
        Just (p', ann')
          | p' == pragma -> Right acc
          | otherwise    -> Left (ConflictingAnnotation k ann' ann)

    mkPragma _    Parallel  =
      Right "#pragma omp parallel for schedule(static)"
    mkPragma _    Vectorize =
      Right "#pragma omp simd"
    mkPragma name ReductionParallel =
      case Map.lookup name redMap of
        Nothing -> Left (CodegenInternalError $
          "ReductionParallel annotation on non-reduction equation " ++ name)
        Just ri ->
          let op = reduceOpOmp (riOp ri)
              sz = domainSizeExpr domBounds name
          in Right ("#pragma omp parallel for reduction("
                   ++ op ++ ":" ++ name ++ "_buf[:" ++ sz
                   ++ "]) schedule(static)")

-- | OpenMP reduction-clause identifier for an Alpha 'ReduceOp'.
reduceOpOmp :: ReduceOp -> String
reduceOpOmp ReduceSum  = "+"
reduceOpOmp ReduceProd = "*"
reduceOpOmp ReduceMin  = "min"
reduceOpOmp ReduceMax  = "max"

-- | Flat size expression @(b0) * (b1) * ...@ for a variable's declared
-- domain, used in @reduction(op:buf[:size])@ and calloc sizing.
domainSizeExpr :: Map.Map String [String] -> String -> String
domainSizeExpr domBounds n = case Map.lookup n domBounds of
  Just bs -> intercalate " * " ["(" ++ b ++ ")" | b <- bs]
  Nothing -> "1"


-- ═══════════════════════════════════════════════════════════════════════
-- §6. C source assembly
-- ═══════════════════════════════════════════════════════════════════════

assembleCSource
  :: [String]
  -> CFunctionMapping
  -> Allocation
  -> String
  -> String
  -> Map.Map String ScalarDesc
  -> Map.Map String [String]    -- domain bounds per variable
  -> Map.Map String ReduceInfo  -- reduction info per equation
  -> Either CodegenError String
assembleCSource params fmap' alloc macros skeleton descs domBounds reduceMap = do
  let funcName = cfName fmap'
      passing = cfArgPassing fmap'

      lookupCType n = case Map.lookup n descs of
        Just (MkScalarDesc { sdCNumType = ct }) -> cTypeName ct
        Nothing -> "double"

      sizeExpr = domainSizeExpr domBounds

      paramDecls = intercalate ", " ["int64_t " ++ p | p <- params]

      callerBufNames = [ n | (n, CallerAllocated) <- Map.toAscList passing ]
      callerArgs = [ ", " ++ lookupCType n ++ " *restrict " ++ n ++ "_buf"
                   | n <- callerBufNames ]

      localAllocs = [ "  " ++ lookupCType n ++ " *" ++ n ++ "_buf = ("
                      ++ lookupCType n ++ "*)calloc("
                      ++ sizeExpr n
                      ++ ", sizeof(" ++ lookupCType n ++ "));"
                    | (n, LocallyManaged) <- Map.toAscList passing ]

      localFrees = [ "  free(" ++ n ++ "_buf);"
                   | (n, LocallyManaged) <- Map.toAscList passing ]

      initLoop n val =
        "  for (int64_t _ri = 0; _ri < " ++ sizeExpr n ++ "; _ri++) "
        ++ n ++ "_buf[_ri] = " ++ val ++ ";"

  -- Reduction init: always emit a per-op identity init loop (#2).  The
  -- buffer is always calloc'd for local storage, but relying on bitwise
  -- zero being the additive identity is a type-silent coupling; the
  -- explicit init loop makes the type→identity mapping the single source
  -- of truth.  On a missing 'sdReduceIdentity' or an unsupported op,
  -- surface 'MissingReduceIdentity' instead of silently falling through.
  reductionInits <- traverse (reduceInitLine initLoop) (Map.toAscList reduceMap)

  pure $ unlines $
    [ "#include <stdlib.h>"
    , "#include <stdint.h>"
    , "#include <string.h>"
    , "#include <math.h>"
    , "#ifdef _OPENMP"
    , "#include <omp.h>"
    , "#endif"
    , ""
    , "#define floord(n,d) (((n)<0) ? -((-(n)+(d)-1)/(d)) : (n)/(d))"
    , "#define min(x,y)    (((x)<(y)) ? (x) : (y))"
    , "#define max(x,y)    (((x)>(y)) ? (x) : (y))"
    , ""
    , "// Statement macros"
    , macros
    , ""
    , "void " ++ funcName ++ "(" ++ paramDecls ++ concat callerArgs ++ ") {"
    ]
    ++ localAllocs
    ++ reductionInits
    ++ [ skeleton ]
    ++ localFrees
    ++ [ "}"
       , ""
       , "// Uniform wrapper for generic FFI calling convention"
       , "void alpha_call(int64_t* params, void** bufs) {"
       , "  " ++ funcName ++ "("
         ++ intercalate ", "
              (  [ "params[" ++ show i ++ "]" | i <- [0 .. length params - 1] ]
              ++ [ "(" ++ lookupCType n ++ "*)bufs[" ++ show i ++ "]"
                 | (i, n) <- zip [0..] callerBufNames ])
         ++ ");"
       , "}"
       ]
  where
    reduceInitLine initLoop (n, ri) = do
      let op = riOp ri
      case Map.lookup n descs of
        Nothing -> Left (MissingScalarDesc n)
        Just (MkScalarDesc { sdCNumType = ct, sdReduceIdentity = mIdFn }) ->
          case mIdFn of
            Nothing   -> Left (MissingReduceIdentity op (cTypeName ct))
            Just idFn -> case idFn op of
              Nothing  -> Left (MissingReduceIdentity op (cTypeName ct))
              Just val -> Right (initLoop n val)


-- ═══════════════════════════════════════════════════════════════════════
-- §7. ISL helpers
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk an ISL AST node to a pure 'CNode' tree and free the node.
-- Consumes the node exactly once. Uses 'unsafeCoerce' for two reasons:
--   1. Present a linear-safe wrapper (inner @go@ uses node twice:
--      walk borrows, free consumes).
--   2. Run IslT actions inside unsafeIslFromIO's callback by cracking
--      open the opaque IslT newtype (IslT m a ~ Ctx -> m a internally).
walkAndFree :: AstNode %1 -> IslT IO (Ur CNode)
walkAndFree = unsafeCoerce go
  where
    go :: AstNode -> IslT IO (Ur CNode)
    go node = unsafeIslFromIO $ \ctx -> do
      ct <- runIslAction ctx (walkAstNode node)
      runIslAction ctx (astNodeFree node)
      pure (Ur ct)

    runIslAction :: Ctx -> IslT IO a -> IO a
    runIslAction ctx action = unsafeCoerce action ctx
