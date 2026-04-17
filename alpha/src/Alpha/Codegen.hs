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
    -- * AST walking (exposed for regression tests)
  , extractStmtArgs
  ) where

import Control.DeepSeq (NFData(..))
import Data.List (intercalate, nub)
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

import Isl.TypeLevel.Reflection (DomTag, reflectDomConstraints)
import Alpha.Core
import Alpha.Lower (lowerSystem, logicalName)
import Alpha.Schedule (Schedule(..), EqSchedule(..), DimAnnotation(..))
import Alpha.Allocation (Allocation(..), EqStorage(..))
import qualified Alpha.Polyhedral.Schedule as S
import Alpha.Codegen.CRender (renderCNodeToC)
import Alpha.Codegen.ExprRender (RenderCtx(..), renderEquationMacro, BoundErr(..), RenderErr(..))
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
  | NonStandardMapConstraint !String !Int
    -- ^ A @Dep@ map equality has a non-±1 coefficient on @OutDim k@
    -- (or couples multiple OutDims); no direct subscript synthesizable.
    -- Arguments: dependency target name, offending output-dim index.
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
  rnf (NonStandardMapConstraint v k)    = rnf v `seq` rnf k


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Public API
-- ═══════════════════════════════════════════════════════════════════════

codegen
  :: forall ps pctx inputs outputs locals.
     KnownSymbols ps
  => System ps pctx inputs outputs locals
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

mapRenderErr :: RenderErr -> CodegenError
mapRenderErr (RENonStandardMapConstraint v k) = NonStandardMapConstraint v k


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Schedule lowering (factored from Compile.hs)
-- ═══════════════════════════════════════════════════════════════════════

-- | Lower schedule maps for codegen.
--
-- After the Case fan-out, iteration 'domains' contain one 'NamedSet'
-- per /statement/ — possibly multiple per equation.  We look up the
-- schedule entry by 'logicalName' and reuse the same schedule payload
-- for every branch statement of an equation.  ISL then sees N
-- statements at the same schedule point with disjoint domains and
-- generates split/peeled loops naturally.
--
-- For reduction equations, the schedule is extended to cover the body
-- domain (which includes reduction dims).  The user-provided schedule
-- covers the equation's output dims; we append identity dims for the
-- reduction variables so ISL generates the reduction loop.  Per-branch
-- reduction shape is supported: each branch statement gets its own
-- 'ReduceInfo' entry (or none), so branches may mix reducing /
-- non-reducing / differently-reducing shapes.
lowerScheduleMaps :: Schedule -> [NamedSet] -> Map.Map String ReduceInfo -> [NamedMap]
lowerScheduleMaps (Schedule entries) domains reduceMap =
  concatMap lowerOne domains
  where
    lowerOne dom@(NamedSet { nsName = Just stmtName }) =
      case Map.lookup (logicalName stmtName) entries of
        Just eq ->
          case Map.lookup stmtName reduceMap of
            Just ri ->
              -- Reduction statement: schedule the body domain with reduction dims.
              let nEq = nsNDims dom
                  nRedDims = riRedDims ri
                  bodyDom = NamedSet
                    { nsName   = Just stmtName
                    , nsParams = nsParams dom
                    , nsNDims  = nEq + nRedDims
                    , nsConjs  = riBodyConjs ri
                    }
                  origExprs = S.schedExprs (esDef eq)
                  redExprs  = [ C.Ix (C.InDim (nEq + r))
                              | r <- [0 .. nRedDims - 1] ]
                  extDef = S.ScheduleDef (origExprs ++ redExprs)
              in [S.schedToNamedMap' stmtName bodyDom extDef]
            Nothing ->
              [S.schedToNamedMap' stmtName dom (esDef eq)]
        Nothing -> []
    lowerOne _ = []

-- ═══════════════════════════════════════════════════════════════════════
-- §4. Macro generation
-- ═══════════════════════════════════════════════════════════════════════

-- | Extract statement name → iterator arg names from CUser nodes.
-- 'CUser' carries structured args post-#7 — no string parsing required.
extractStmtArgs :: CNode -> Map.Map String [String]
extractStmtArgs (CFor _ _ _ _ body) = extractStmtArgs body
extractStmtArgs (CIf _ thn mels)    = extractStmtArgs thn
                                        <> maybe Map.empty extractStmtArgs mels
extractStmtArgs (CBlock cs)         = foldMap extractStmtArgs cs
extractStmtArgs (CUser name args)   = Map.singleton name args

generateMacros
  :: forall ps pctx inputs outputs locals.
     KnownSymbols ps
  => System ps pctx inputs outputs locals
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
  :: forall ps pctx decls defined.
     KnownSymbols ps
  => EqList ps pctx decls defined
  -> Schedule
  -> Map.Map String EqStorage
  -> [String]
  -> Map.Map String [String]
  -> Map.Map String [String]
  -> Map.Map String ScalarDesc
  -> Either CodegenError [String]
generateFromEqList EqNil _ _ _ _ _ _ = Right []
generateFromEqList (Defines (Proxy :: Proxy name) body :& rest) sched storMap params domBounds stmtArgs descs =
  let eqName   = symbolVal (Proxy @name)
      nOutDims = case Map.lookup eqName (schedEntries sched) of
        Just es -> esNIter es
        Nothing -> 0
  in do
    macros <- renderOneEq eqName nOutDims body sched storMap params domBounds stmtArgs descs
    (macros ++) <$> generateFromEqList rest sched storMap params domBounds stmtArgs descs

-- | Render one equation to a list of @#define@ lines.  A plain RHS
-- emits a single macro @eqName(...)@; a top-level 'Case' fans out to
-- @eqName__br0(...), eqName__br1(...), …@, each writing to the same
-- logical array @eqName@.
renderOneEq
  :: forall ps pctx decls n (d :: DomTag ps n) a.
     KnownSymbols ps
  => String
  -> Int
  -> Alpha.Core.Expr ps pctx decls n d a
  -> Schedule
  -> Map.Map String EqStorage
  -> [String]
  -> Map.Map String [String]
  -> Map.Map String [String]
  -> Map.Map String ScalarDesc
  -> Either CodegenError [String]
renderOneEq eqName nOutDims (Case branches) _sched storMap params domBounds stmtArgs descs =
  renderBranchList eqName nOutDims 0 branches storMap params domBounds stmtArgs descs
renderOneEq eqName nOutDims body _sched storMap params domBounds stmtArgs descs =
  case renderWithStmtName eqName eqName nOutDims body storMap params domBounds stmtArgs descs of
    Left err    -> Left err
    Right macro -> Right [macro]

-- | Walk a 'Branches' list, rendering one macro per branch.  The
-- branch index @i@ threads the statement name @eqName__brI@; the LHS
-- of each macro is the logical array name @eqName@ (shared), so all N
-- branches write to the same buffer under disjoint domains.
renderBranchList
  :: forall ps pctx decls n (amb :: DomTag ps n) branchDoms a.
     KnownSymbols ps
  => String -> Int -> Int
  -> Branches ps pctx decls n amb branchDoms a
  -> Map.Map String EqStorage
  -> [String]
  -> Map.Map String [String]
  -> Map.Map String [String]
  -> Map.Map String ScalarDesc
  -> Either CodegenError [String]
renderBranchList _ _ _ BNil _ _ _ _ _ = Right []
renderBranchList eqName nOutDims i
                 (BCons _ body rest) storMap params domBounds stmtArgs descs = do
  let stmtName = eqName ++ "__br" ++ show i
  macro <- renderWithStmtName stmtName eqName nOutDims body
             storMap params domBounds stmtArgs descs
  (macro :) <$>
    renderBranchList eqName nOutDims (i + 1) rest
                     storMap params domBounds stmtArgs descs

-- | Shared macro-emission tail: look up stmtArgs, scalar desc, render.
renderWithStmtName
  :: forall ps pctx decls n (d :: DomTag ps n) a.
     KnownSymbols ps
  => String  -- macro / stmt name (key into stmtArgs)
  -> String  -- logical array name (LHS of the write)
  -> Int     -- output dims
  -> Alpha.Core.Expr ps pctx decls n d a
  -> Map.Map String EqStorage
  -> [String]
  -> Map.Map String [String]
  -> Map.Map String [String]
  -> Map.Map String ScalarDesc
  -> Either CodegenError String
renderWithStmtName stmtName lhsName nOutDims body
                   storMap params domBounds stmtArgs descs =
  case Map.lookup stmtName stmtArgs of
    Nothing -> Left (CodegenInternalError
      ("generateFromEqList: no CUser for statement " ++ show stmtName
       ++ " — walkAstNode didn't emit a call for a lowered statement"))
    Just iterVars -> case Map.lookup lhsName descs of
      Nothing -> Left (MissingScalarDesc lhsName)
      Just desc ->
        let ctx = RenderCtx
              { rcParams    = params
              , rcIterVars  = iterVars
              , rcStorage   = storMap
              , rcDomBounds = domBounds
              , rcDesc      = desc
              }
        in case renderEquationMacro stmtName lhsName nOutDims body ctx of
          Left rerr -> Left (mapRenderErr rerr)
          Right m   -> Right m

-- | Extract reduction info from an equation (or branch) body: number
-- of reduction dims and the body domain constraints (for ISL loop
-- generation).
--
-- 'riLogicalArray' threads the equation name (not the statement name)
-- through to the pragma/init paths so they aggregate reductions by
-- buffer, not by statement: two Case branches both reducing into
-- @C_buf@ collapse to one @reduction(+:C_buf[:size])@ clause and one
-- identity-init loop.
data ReduceInfo = ReduceInfo
  { riOp           :: !ReduceOp
  , riRedDims      :: !Int
  , riBodyConjs    :: ![C.Conjunction C.SetIx]
  , riLogicalArray :: !String
  }

extractReduceInfo
  :: forall ps pctx decls n d a.
     String  -- logical array name (equation name)
  -> Alpha.Core.Expr ps pctx decls n d a
  -> Maybe ReduceInfo
extractReduceInfo eqName (Reduce op _ (_inner :: Alpha.Core.Expr ps pctx decls nBody dBody a)) =
  let nRed = fromIntegral (natVal (Proxy @nBody)) - fromIntegral (natVal (Proxy @n))
      bodyCs = reflectDomConstraints @ps @nBody @dBody
  in Just (ReduceInfo op nRed [C.Conjunction bodyCs] eqName)
extractReduceInfo _ _ = Nothing

-- | Build a map from /statement/ name → reduction info.
--
-- A plain reducing equation keys under @eqName@; a 'Case' equation
-- where branch @i@ reduces keys under @eqName__brI@.  Branches that
-- do not reduce have no entry.  This per-statement shape is what
-- lifts the v1 "uniform Reduce across branches" restriction — each
-- fanned-out statement carries its own independent reduction metadata.
buildReduceMap
  :: forall ps pctx decls defined.
     EqList ps pctx decls defined -> Map.Map String ReduceInfo
buildReduceMap EqNil = Map.empty
buildReduceMap (Defines (Proxy :: Proxy name) body :& rest) =
  let eqName = symbolVal (Proxy @name)
      restMap = buildReduceMap rest
  in case body of
    Case branches -> addBranchReduces eqName 0 branches restMap
    _             -> case extractReduceInfo eqName body of
      Just ri -> Map.insert eqName ri restMap
      Nothing -> restMap

-- | Walk a 'Branches' list, inserting a 'ReduceInfo' entry per
-- branch whose body is a 'Reduce'.  Branch index @i@ parallels
-- 'renderBranchList' / 'lowerCaseBranches' so keys align with
-- statement names everywhere.
addBranchReduces
  :: forall ps pctx decls n (amb :: DomTag ps n) branchDoms a.
     String -> Int
  -> Branches ps pctx decls n amb branchDoms a
  -> Map.Map String ReduceInfo
  -> Map.Map String ReduceInfo
addBranchReduces _ _ BNil acc = acc
addBranchReduces eqName i (BCons _ body rest) acc =
  let stmtName = eqName ++ "__br" ++ show i
      acc'     = addBranchReduces eqName (i + 1) rest acc
  in case extractReduceInfo eqName body of
    Just ri -> Map.insert stmtName ri acc'
    Nothing -> acc'


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
      -- After the Case fan-out, 'redMap' is keyed by /statement/ name;
      -- a reducing equation has entries under either @name@ (non-Case)
      -- or @name__brI@ (Case-split branches).  The pragma text depends
      -- only on the op and logical buffer, both of which are shared
      -- across an equation's branches by construction, so any one
      -- entry works.
      case lookupByLogical name redMap of
        Nothing -> Left (CodegenInternalError $
          "ReductionParallel annotation on non-reduction equation " ++ name)
        Just ri ->
          let op = reduceOpOmp (riOp ri)
              sz = domainSizeExpr domBounds name
          in Right ("#pragma omp parallel for reduction("
                   ++ op ++ ":" ++ name ++ "_buf[:" ++ sz
                   ++ "]) schedule(static)")

-- | Find any 'ReduceInfo' entry whose logical array matches.  Used by
-- the pragma path where the equation name — not the statement name —
-- is in hand.
lookupByLogical :: String -> Map.Map String ReduceInfo -> Maybe ReduceInfo
lookupByLogical eqName =
  fmap snd . Map.lookupMin . Map.filter (\ri -> riLogicalArray ri == eqName)

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
  --
  -- After the Case fan-out, a single logical array may have N
  -- per-branch 'ReduceInfo' entries; we dedup by logical array name to
  -- emit exactly one init loop per buffer.  Case branches within an
  -- equation share an op by construction (they write to the same
  -- buffer with the same reduction semantics), so picking any entry's
  -- op is well-defined.
  let uniqueReducers =
        [ (arr, ri)
        | arr <- nub (map riLogicalArray (Map.elems reduceMap))
        , Just ri <- [lookupByLogical arr reduceMap]
        ]
  reductionInits <- traverse (reduceInitLine initLoop) uniqueReducers

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
