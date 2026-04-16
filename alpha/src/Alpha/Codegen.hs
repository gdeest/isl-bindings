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
  ) where

import Control.DeepSeq (NFData(..))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, natVal, symbolVal)
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
import Alpha.Codegen.ExprRender (RenderCtx(..), renderEquationMacro, extractOneBound, extractBoundsISL, descCType)
import Alpha.Codegen.FunctionMapping (CFunctionMapping(..), ArgPassing(..))
import Alpha.Scalar (ScalarDesc(..), cTypeName)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Error type
-- ═══════════════════════════════════════════════════════════════════════

data CodegenError
  = CodegenScheduleError !String
  | CodegenInternalError !String
  deriving (Show, Eq)

instance NFData CodegenError where
  rnf (CodegenScheduleError s) = rnf s
  rnf (CodegenInternalError s) = rnf s


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
codegen sys@(System _decls eqs) sched alloc fmap' descs = runIslT $ Isl.do
  let (domains, _writes, _reads, _projections) = lowerSystem sys
      reduceMap = buildReduceMap eqs
      schedMaps = lowerScheduleMaps sched domains reduceMap
      params = symbolVals @ps

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
          domBounds = extractAllBounds domains params
          macros = generateMacros sys sched alloc params domBounds stmtArgs descs
          annotations = mergeAnnotations sched
          skeleton = renderCNodeToC annotations cTree
          cSrc = assembleCSource params fmap' alloc macros skeleton descs
      Isl.pure (Ur (Right cSrc))


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
  -> String
generateMacros (System _decls eqs) sched alloc params domBounds stmtArgs descs =
  let storMap = allocEntries alloc
      macroLines = generateFromEqList eqs sched storMap params domBounds stmtArgs descs
  in unlines macroLines

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
  -> [String]
generateFromEqList EqNil _ _ _ _ _ _ = []
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
      desc = case Map.lookup eqName descs of
        Just d  -> d
        Nothing -> error $ "Alpha.Codegen: no ScalarDesc for " ++ eqName
      ctx = RenderCtx
        { rcParams    = params
        , rcIterVars  = iterVars
        , rcStorage   = storMap
        , rcDomBounds = domBounds
        , rcDesc      = desc
        }
      nOutDims = case Map.lookup eqName (schedEntries sched) of
        Just es -> esNIter es
        Nothing -> 0
      macro = renderEquationMacro eqName nOutDims body ctx
  in macro : generateFromEqList rest sched storMap params domBounds stmtArgs descs

-- | Extract reduction info from an equation body: number of reduction
-- dims and the body domain constraints (for ISL loop generation).
data ReduceInfo = ReduceInfo
  { riRedDims  :: !Int
  , riBodyConjs :: ![C.Conjunction C.SetIx]
  }

extractReduceInfo :: forall ps decls n d a. Alpha.Core.Expr ps decls n d a -> Maybe ReduceInfo
extractReduceInfo (Reduce _ _ (inner :: Alpha.Core.Expr ps decls nBody dBody a)) =
  let nRed = fromIntegral (natVal (Proxy @nBody)) - fromIntegral (natVal (Proxy @n))
      bodyCs = reflectDomConstraints @ps @nBody @dBody
  in Just (ReduceInfo nRed [C.Conjunction bodyCs])
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
-- §5. Domain bounds extraction
-- ═══════════════════════════════════════════════════════════════════════

extractAllBounds :: [NamedSet] -> [String] -> Map.Map String [String]
extractAllBounds domains params = Map.fromList
  [ (name, extractDomBounds params conjs nDims)
  | NamedSet { nsName = Just name, nsNDims = nDims, nsConjs = conjs } <- domains
  ]

-- | Extract per-dimension exclusive upper bounds. Tries the fast
-- structural pattern matcher first; falls back to ISL @dim_max@
-- for domains with non-standard constraint forms (e.g., from
-- @IslPreimageMultiAff@).
extractDomBounds :: [String] -> [C.Conjunction C.SetIx] -> Int -> [String]
extractDomBounds params conjs nDims =
  let patternBounds = [ extractOneBound params conjs d | d <- [0 .. nDims - 1] ]
  in if any ("/* unknown */" ==) patternBounds
     then
       -- Pattern matcher failed on at least one dim; use ISL for all.
       let domStr = conjsToDomStr params nDims conjs
       in extractBoundsISL domStr nDims
     else patternBounds

-- | Build an ISL set string from conjunctions.
conjsToDomStr :: [String] -> Int -> [C.Conjunction C.SetIx] -> String
conjsToDomStr params nDims conjs =
  let paramStr = if null params then "" else "[" ++ intercalate ", " params ++ "] -> "
      dimVars = ["i" ++ show d | d <- [0 .. nDims - 1]]
      dimStr = "[" ++ intercalate ", " dimVars ++ "]"
      renderConj (C.Conjunction cs) = intercalate " and " (map (renderOneConstraint dimVars params) cs)
      conjStrs = map renderConj conjs
  in paramStr ++ "{ " ++ dimStr ++ " : " ++ intercalate " or " conjStrs ++ " }"

renderOneConstraint :: [String] -> [String] -> C.Constraint C.SetIx -> String
renderOneConstraint dims params (C.EqualityConstraint e) =
  renderBoundExpr dims params e ++ " = 0"
renderOneConstraint dims params (C.InequalityConstraint e) =
  renderBoundExpr dims params e ++ " >= 0"

renderBoundExpr :: [String] -> [String] -> C.Expr C.SetIx -> String
renderBoundExpr dims params = go
  where
    go (C.Ix (C.SetDim d))   | d < length dims = dims !! d
                              | otherwise = "d" ++ show d
    go (C.Ix (C.SetParam p)) | p < length params = params !! p
                              | otherwise = "p" ++ show p
    go (C.Constant n)  = show n
    go (C.Mul 1 e)     = go e
    go (C.Mul (-1) e)  = "(-" ++ go e ++ ")"
    go (C.Mul k e)     = show k ++ "*" ++ go e
    go (C.Add a b)     = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (C.FloorDiv e d) = "floord(" ++ go e ++ ", " ++ show d ++ ")"


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Annotation merging
-- ═══════════════════════════════════════════════════════════════════════

mergeAnnotations :: Schedule -> Map.Map Int DimAnnotation
mergeAnnotations (Schedule entries) =
  Map.unionsWith (\a b -> if a == b then a else error $
    "conflicting annotations on same schedule dim: " ++ show a ++ " vs " ++ show b)
    [esAnnotations es | es <- Map.elems entries]


-- ═══════════════════════════════════════════════════════════════════════
-- §7. C source assembly
-- ═══════════════════════════════════════════════════════════════════════

assembleCSource
  :: [String]
  -> CFunctionMapping
  -> Allocation
  -> String
  -> String
  -> Map.Map String ScalarDesc
  -> String
assembleCSource params fmap' alloc macros skeleton descs =
  let funcName = cfName fmap'
      passing = cfArgPassing fmap'

      lookupCType n = case Map.lookup n descs of
        Just (MkScalarDesc { sdCNumType = ct }) -> cTypeName ct
        Nothing -> "double"  -- fallback for inputs without explicit desc

      paramDecls = intercalate ", " ["int64_t " ++ p | p <- params]

      callerBufNames = [ n | (n, CallerAllocated) <- Map.toAscList passing ]
      callerArgs = [ ", " ++ lookupCType n ++ " *restrict " ++ n ++ "_buf"
                   | n <- callerBufNames ]

      localAllocs = [ "  " ++ lookupCType n ++ " *" ++ n ++ "_buf = ("
                      ++ lookupCType n ++ "*)calloc("
                      ++ "1" -- TODO: compute size from domain bounds
                      ++ ", sizeof(" ++ lookupCType n ++ "));"
                    | (n, LocallyManaged) <- Map.toAscList passing ]

      localFrees = [ "  free(" ++ n ++ "_buf);"
                   | (n, LocallyManaged) <- Map.toAscList passing ]

  in unlines $
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


-- ═══════════════════════════════════════════════════════════════════════
-- §8. ISL helpers
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
