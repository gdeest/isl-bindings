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
import Alpha.Codegen.ExprRender (RenderCtx(..), renderEquationMacro, extractOneBound)
import Alpha.Codegen.FunctionMapping (CFunctionMapping(..), ArgPassing(..))


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
  -> IO (Either CodegenError String)
codegen sys@(System _decls eqs) sched alloc fmap' = runIslT $ Isl.do
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

      -- Compute domain bounds for stride linearization
      let domBounds = extractAllBounds domains params
          macros = generateMacros sys sched alloc params domBounds
          annotations = mergeAnnotations sched
          skeleton = renderCNodeToC annotations cTree
          cSrc = assembleCSource params fmap' alloc macros skeleton
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

generateMacros
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule
  -> Allocation
  -> [String]       -- parameter names
  -> Map.Map String [String]  -- domain bounds per variable
  -> String         -- all #define lines
generateMacros (System _decls eqs) sched alloc params domBounds =
  let storMap = allocEntries alloc
      macroLines = generateFromEqList eqs sched storMap params domBounds
  in unlines macroLines

generateFromEqList
  :: forall ps decls defined.
     KnownSymbols ps
  => EqList ps decls defined
  -> Schedule
  -> Map.Map String EqStorage
  -> [String]
  -> Map.Map String [String]
  -> [String]
generateFromEqList EqNil _ _ _ _ = []
generateFromEqList (Defines (Proxy :: Proxy name) body :& rest) sched storMap params domBounds =
  let eqName = symbolVal (Proxy @name)
      nSchedDims = case Map.lookup eqName (schedEntries sched) of
        Just es -> esNTime es
        Nothing -> 0
      nRedDims = case extractReduceInfo body of
        Just ri -> riRedDims ri
        Nothing -> 0
      nTime = nSchedDims + nRedDims
      iterVars = ["c" ++ show i | i <- [0 .. nTime - 1]]
      ctx = RenderCtx
        { rcParams    = params
        , rcIterVars  = iterVars
        , rcStorage   = storMap
        , rcDomBounds = domBounds
        , rcCType     = "double"
        }
      nOutDims = case Map.lookup eqName (schedEntries sched) of
        Just es -> esNIter es
        Nothing -> 0
      macro = renderEquationMacro eqName nOutDims body ctx
  in macro : generateFromEqList rest sched storMap params domBounds

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
-- §5. Domain bounds extraction (pure, constraint-based)
-- ═══════════════════════════════════════════════════════════════════════

extractAllBounds :: [NamedSet] -> [String] -> Map.Map String [String]
extractAllBounds domains params = Map.fromList
  [ (name, [ extractOneBound params conjs d | d <- [0 .. nDims - 1] ])
  | NamedSet { nsName = Just name, nsNDims = nDims, nsConjs = conjs } <- domains
  ]


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
  :: [String]           -- parameter names
  -> CFunctionMapping
  -> Allocation
  -> String             -- macro definitions
  -> String             -- loop skeleton
  -> String
assembleCSource params fmap' alloc macros skeleton =
  let funcName = cfName fmap'
      passing = cfArgPassing fmap'

      -- Parameters as function args
      paramDecls = intercalate ", " ["int64_t " ++ p | p <- params]

      -- Buffer arguments (CallerAllocated)
      callerArgs = [ ", double *restrict " ++ n ++ "_buf"
                   | (n, CallerAllocated) <- Map.toAscList passing ]

      -- Local buffer allocations
      localAllocs = [ "  double *" ++ n ++ "_buf = (double*)calloc("
                      ++ "1" -- TODO: compute size from domain bounds
                      ++ ", sizeof(double));"
                    | (n, LocallyManaged) <- Map.toAscList passing ]

      localFrees = [ "  free(" ++ n ++ "_buf);"
                   | (n, LocallyManaged) <- Map.toAscList passing ]

      -- Equation names (outputs + locals, not inputs) for undef
      eqNames = [ n | (n, p) <- Map.toAscList passing
                 , p == CallerAllocated || p == LocallyManaged
                 -- Inputs don't get macros; filter by whether they have macros
                 -- For now, undef everything that's not clearly an input
                 -- The macro generator only generates macros for equations,
                 -- so we extract those names from the macros string
                 ]

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
    ++ [ "}" ]


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
