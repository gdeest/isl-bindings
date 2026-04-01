{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Multi-statement C code generation from polyhedral scanners.
--
-- Walks a 'MergedAST' (from "Isl.Scan.PrettyMulti") and emits C loops
-- with interleaved statement bodies, scratch buffer declarations,
-- reduction init\/finalize, and OpenMP pragmas.
--
-- This is the \"Path D\" that bridges multi-statement polyhedral scanning
-- ('MultiScanner') with C code emission, complementing the single-statement
-- path in "Isl.Infer.Codegen.Loop".
module Isl.Infer.Codegen.Multi
  ( -- * Kernel specification
    MultiCKernel(..)
  , CStmtBody(..)
  , StmtAnnotation(..)
  , ScratchDecl(..)
    -- * C code generation
  , generateMultiC
  , generateMultiFunction
    -- * Low-level emission (for composing into larger programs)
  , EmitCtx(..)
  , emitMergedAST
  , groupScratch
  , collectReductions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits (Nat)

import Isl.Scan.Types (AffineBound(..))
import Isl.Scan.Multi (MultiScanner(..))
import Isl.Scan.PrettyMulti (MergedAST(..), buildMergedAST)
import Isl.Infer.Codegen.Bound (lowerBoundToC, upperBoundToC, equalityBoundToC)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Annotation on a statement body for reduction or lifecycle semantics.
data StmtAnnotation
  = PlainStmt
    -- ^ Simple write, no special lifecycle.
  | ReductionStmt
    { rsInitCode     :: !String
      -- ^ Code emitted before the reduction loop opens (e.g. @memset(acc,0,...)@).
    , rsFinalizeCode :: !String
      -- ^ Code emitted after the reduction loop closes (e.g. @memcpy(out,acc,...)@).
    , rsReductionDim :: !String
      -- ^ Name of the time dimension that is the reduction axis.
      -- Init is emitted before this loop; finalize after it.
    }
  deriving (Show, Eq)

-- | A statement body: C code template + metadata.
data CStmtBody = CStmtBody
  { csbName       :: !String
    -- ^ Statement name (must match 'ssName' in the 'MultiScanner').
  , csbBody       :: !String
    -- ^ C code for the innermost body (inserted at the leaf of the loop nest).
  , csbAnnotation :: !StmtAnnotation
    -- ^ Reduction or lifecycle annotation.
  , csbAliases    :: [(String, String)]
    -- ^ @(timeDimName, bodyVarName)@ — emit @const int64_t bodyVarName = timeDimName;@
    -- before the body. Lets statement bodies use natural variable names
    -- (e.g. @\"dj\"@, @\"kb\"@) while the merged loop uses generic names (@\"d3\"@, @\"d4\"@).
  }
  deriving (Show, Eq)

-- | Thread-local scratch buffer declaration, scoped to a specific loop level.
data ScratchDecl = ScratchDecl
  { sdVarName  :: !String
    -- ^ C variable name (e.g. @\"panel\"@).
  , sdType     :: !String
    -- ^ C type (e.g. @\"float\"@).
  , sdSizeExpr :: !String
    -- ^ C expression for element count (e.g. @\"TK * TJ\"@).
  , sdAlign    :: !Int
    -- ^ Alignment in bytes (0 = no alignment attribute).
  , sdScope    :: !String
    -- ^ Time dimension name where the declaration is emitted.
    -- The buffer is declared just inside this loop's opening brace.
  }
  deriving (Show, Eq)

-- | Multi-statement kernel specification.
--
-- Parallel to 'CKernel' for single-statement kernels, but uses
-- 'MultiScanner' and supports multiple statement bodies with
-- scratch buffers and reduction annotations.
data MultiCKernel (nParams :: Nat) = MultiCKernel
  { mckName         :: !String
    -- ^ C function name.
  , mckIncludes     :: [String]
    -- ^ @#include@ directives (without the @#include@ prefix).
  , mckTypedefs     :: !String
    -- ^ Type definitions (structs, helpers).
  , mckMacros       :: [(String, String)]
    -- ^ @#define name value@ pairs.
  , mckReturnType   :: !String
    -- ^ Return type (usually @\"void\"@).
  , mckFuncParams   :: [(String, String)]
    -- ^ @(type, name)@ for function parameters.
  , mckTimeDimNames :: [String]
    -- ^ Names for the time-space dimensions (outermost to innermost).
  , mckParamNames   :: [String]
    -- ^ Names for polyhedron parameters.
  , mckParallelDims :: Set Int
    -- ^ Time dimension indices with @#pragma omp parallel for@.
    -- Indices refer to positions in 'mckTimeDimNames'.
  , mckSimdDims     :: Set Int
    -- ^ Time dimension indices with @#pragma omp simd@.
  , mckPreamble     :: !String
    -- ^ Code before the loop nest (inside the function body).
  , mckPostamble    :: !String
    -- ^ Code after the loop nest.
  , mckStmtBodies   :: [CStmtBody]
    -- ^ One per statement, matched by name to 'MStmtBody' nodes.
  , mckScratch      :: [ScratchDecl]
    -- ^ Scratch buffers, each scoped to a specific loop level.
  , mckScanner      :: MultiScanner nParams
    -- ^ The multi-statement scanner. When @nParams = 0@, all bounds are
    -- concrete constants. When @nParams > 0@, loop bounds reference the
    -- parameter names from 'mckParamNames' as C runtime variables.
  }

-- ---------------------------------------------------------------------------
-- Code generation
-- ---------------------------------------------------------------------------

-- | Generate a complete C source file from a 'MultiCKernel'.
generateMultiC :: MultiCKernel nParams -> String
generateMultiC mck = unlines $
  -- Includes
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  ]
  ++ map (\h -> "#include " ++ h) (mckIncludes mck)
  ++ [""]
  -- ISL macros
  ++ [ "#define ISL_CEIL_DIV(a, b) (((a) >= 0) ? (((a) + (b) - 1) / (b)) : -(-(a) / (b)))"
     , "#define ISL_FLOOR_DIV(a, b) (((a) >= 0) ? ((a) / (b)) : -((-(a) + (b) - 1) / (b)))"
     , "#define ISL_MAX(a, b) ((a) > (b) ? (a) : (b))"
     , "#define ISL_MIN(a, b) ((a) < (b) ? (a) : (b))"
     ]
  ++ map (\(n, v) -> "#define " ++ n ++ " " ++ v) (mckMacros mck)
  ++ [""]
  -- Typedefs
  ++ (if null (mckTypedefs mck) then [] else [mckTypedefs mck, ""])
  -- Function
  ++ [generateMultiFunction mck]

-- | Generate just the function (no includes/macros/typedefs).
generateMultiFunction :: MultiCKernel nParams -> String
generateMultiFunction mck =
  let ast = buildMergedAST (mckTimeDimNames mck) (mckScanner mck)
      stmtMap = Map.fromList [(csbName b, b) | b <- mckStmtBodies mck]
      scratchByScope = groupScratch (mckScratch mck)
      reductionByDim = collectReductions (mckStmtBodies mck)
      dimNameToIdx = Map.fromList (zip (mckTimeDimNames mck) [0..])
      ctx = EmitCtx
        { ecDimNames     = mckTimeDimNames mck
        , ecParamNames   = mckParamNames mck
        , ecParallelDims = mckParallelDims mck
        , ecSimdDims     = mckSimdDims mck
        , ecStmtBodies   = stmtMap
        , ecScratch      = scratchByScope
        , ecReductions   = reductionByDim
        , ecDimNameToIdx = dimNameToIdx
        , ecNestedParallel = False  -- standalone function, not inside a parallel region
        , ecNowait = False
        }
      bodyLines = emitMergedAST ctx 1 False ast
  in unlines $
    [ mckReturnType mck ++ " " ++ mckName mck ++ "("
      ++ renderParams (mckFuncParams mck) ++ ") {"
    ]
    ++ (if null (mckPreamble mck) then [] else [mckPreamble mck])
    ++ bodyLines
    ++ (if null (mckPostamble mck) then [] else [mckPostamble mck])
    ++ ["}"]

-- ---------------------------------------------------------------------------
-- Internal: emission context
-- ---------------------------------------------------------------------------

data EmitCtx = EmitCtx
  { ecDimNames     :: [String]
  , ecParamNames   :: [String]
  , ecParallelDims :: Set Int
  , ecSimdDims     :: Set Int
  , ecStmtBodies   :: Map String CStmtBody
  , ecScratch      :: Map String [ScratchDecl]  -- scope dim name -> decls
  , ecReductions   :: Map String (String, String)  -- reduction dim name -> (init, finalize)
  , ecDimNameToIdx :: Map String Int
  , ecNestedParallel :: Bool
    -- ^ If True, we're already inside a @#pragma omp parallel@ region.
    -- Emit @#pragma omp for@ instead of @#pragma omp parallel for@.
    -- This context flows down through composition — no string surgery needed.
  , ecNowait :: Bool
    -- ^ If True, add @nowait@ to the @omp for@ pragma.
    -- Allows independent projections to run concurrently within a parallel region.
  }

-- | Group scratch declarations by their scope dimension name.
groupScratch :: [ScratchDecl] -> Map String [ScratchDecl]
groupScratch = foldr (\sd acc -> Map.insertWith (++) (sdScope sd) [sd] acc) Map.empty

-- | Collect reduction init/finalize keyed by the reduction dimension name.
-- If multiple statements reduce over the same dimension, their init/finalize
-- code is concatenated.
collectReductions :: [CStmtBody] -> Map String (String, String)
collectReductions = foldr collect Map.empty
  where
    collect csb acc = case csbAnnotation csb of
      PlainStmt -> acc
      ReductionStmt initC finalC dim ->
        Map.insertWith (\(a1,b1) (a2,b2) -> (a1 ++ "\n" ++ a2, b1 ++ "\n" ++ b2))
          dim (initC, finalC) acc

-- ---------------------------------------------------------------------------
-- Internal: MergedAST → C lines
-- ---------------------------------------------------------------------------

-- | Walk a 'MergedAST' and emit C code lines.
emitMergedAST :: EmitCtx -> Int -> Bool -> MergedAST -> [String]

emitMergedAST ctx depth _inCollapse (MSequence blocks) =
  concatMap (emitMergedAST ctx depth False) blocks

emitMergedAST ctx depth _inCollapse (MStmtBody name _args) =
  case Map.lookup name (ecStmtBodies ctx) of
    Just csb ->
      let aliasLines = [ ind depth ++ "const int64_t " ++ bodyVar ++ " = " ++ timeDim ++ ";"
                        | (timeDim, bodyVar) <- csbAliases csb ]
          bodyLines = [ind depth ++ l | l <- lines (csbBody csb), not (null l)]
      in aliasLines ++ bodyLines
    Nothing  -> [ind depth ++ "/* unknown statement: " ++ name ++ " */"]

emitMergedAST ctx depth _inCollapse (MLet name bound body) =
  let expr = equalityBoundToC (ecDimNames ctx) (ecParamNames ctx) bound
  in [ind depth ++ "{ /* " ++ name ++ " = " ++ expr ++ " */"]
     ++ [ind (depth+1) ++ "const int64_t " ++ name ++ " = " ++ expr ++ ";"]
     ++ concatMap (emitMergedAST ctx (depth+1) False) body
     ++ [ind depth ++ "}"]

emitMergedAST ctx depth inCollapse (MLoop name loBounds hiBounds body) =
  let dimIdx = Map.findWithDefault (-1) name (ecDimNameToIdx ctx)
      isParallel = dimIdx `Set.member` ecParallelDims ctx
      isSimd     = dimIdx `Set.member` ecSimdDims ctx

      -- Compute collapse count for the first parallel loop
      collapseCount = if isParallel && not inCollapse
                      then countMergedCollapse (ecParallelDims ctx) (ecDimNameToIdx ctx)
                             (MLoop name loBounds hiBounds body)
                      else 0

      -- Render bounds
      loStrs = map (lowerBoundToC (ecDimNames ctx) (ecParamNames ctx)) loBounds
      hiStrs = map (upperBoundToC (ecDimNames ctx) (ecParamNames ctx)) hiBounds

      (loPre, loVar) = multiBound depth name "_lo" ">" loStrs
      (hiPre, hiVar) = multiBound depth name "_hi" "<" hiStrs

      -- Pragmas — context-aware: omit "parallel" if already in a parallel region
      parPrefix = if ecNestedParallel ctx then "omp for" else "omp parallel for"
      nwSuffix = if ecNowait ctx && ecNestedParallel ctx then " nowait" else ""
      parPragma
        | collapseCount > 1
          = [ind depth ++ "#pragma " ++ parPrefix ++ nwSuffix ++ " collapse("
             ++ show collapseCount ++ ") schedule(static)"]
        | collapseCount == 1
          = [ind depth ++ "#pragma " ++ parPrefix ++ nwSuffix ++ " schedule(static)"]
        | otherwise = []

      simdPragma
        | isSimd && not isParallel
          = [ind depth ++ "#pragma omp simd"]
        | otherwise = []

      forLine = ind depth ++ "for (int64_t " ++ name ++ " = " ++ loVar
                ++ "; " ++ name ++ " <= " ++ hiVar
                ++ "; " ++ name ++ "++) {"

      -- Scratch declarations scoped to this loop
      scratchLines = case Map.lookup name (ecScratch ctx) of
        Nothing -> []
        Just sds -> concatMap (emitScratch (depth + 1)) sds

      -- Reduction init code (emitted before the reduction loop, at loop depth).
      -- NOT wrapped in { } — variables declared here must be visible to
      -- the loop body and finalize (e.g. softmax state, accumulators).
      reductionInit = case Map.lookup name (ecReductions ctx) of
        Nothing       -> []
        Just (initC, _) ->
          [ind depth ++ l | l <- lines initC, not (null l)]

      -- Reduction finalize code (emitted after the reduction loop).
      reductionFinalize = case Map.lookup name (ecReductions ctx) of
        Nothing       -> []
        Just (_, finC) ->
          [ind depth ++ l | l <- lines finC, not (null l)]

      innerCollapse = inCollapse || collapseCount > 0

      innerLines = concatMap (emitMergedAST ctx (depth + 1) innerCollapse) body

      -- Reduction init goes BEFORE the loop (accumulate across iterations).
      -- Reduction finalize goes AFTER the loop (store results).
  in reductionInit
     ++ loPre ++ hiPre ++ parPragma ++ simdPragma ++ [forLine]
     ++ scratchLines
     ++ innerLines
     ++ [ind depth ++ "}"]
     ++ reductionFinalize

-- | Handle multiple bounds: if one bound, use directly. If multiple,
-- compute into a temp variable using inline max/min.
multiBound :: Int -> String -> String -> String -> [String] -> ([String], String)
multiBound _depth _name _suffix _cmpOp [] = ([], "0")
multiBound _depth _name _suffix _cmpOp [b] = ([], b)
multiBound depth name suffix cmpOp (b:bs) =
  let tmpName = name ++ suffix
      initLine = ind depth ++ "int64_t " ++ tmpName ++ " = " ++ b ++ ";"
      updateLines = [ ind depth ++ "if (" ++ x ++ " " ++ cmpOp ++ " " ++ tmpName ++ ") "
                      ++ tmpName ++ " = " ++ x ++ ";"
                    | x <- bs ]
  in (initLine : updateLines, tmpName)

-- | Emit a scratch buffer declaration.
emitScratch :: Int -> ScratchDecl -> [String]
emitScratch depth sd =
  let alignAttr = if sdAlign sd > 0
                  then " __attribute__((aligned(" ++ show (sdAlign sd) ++ ")))"
                  else ""
  in [ind depth ++ sdType sd ++ " " ++ sdVarName sd
      ++ "[" ++ sdSizeExpr sd ++ "]"
      ++ alignAttr ++ ";"]

-- | Count consecutive MLoop nodes that are parallel (for collapse).
countMergedCollapse :: Set Int -> Map String Int -> MergedAST -> Int
countMergedCollapse parDims nameToIdx (MLoop name _ _ body) =
  case Map.lookup name nameToIdx of
    Just idx | idx `Set.member` parDims ->
      1 + case body of
            [inner@(MLoop {})] -> countMergedCollapse parDims nameToIdx inner
            _                  -> 0
    _ -> 0
countMergedCollapse _ _ _ = 0

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Indentation (4 spaces per level).
ind :: Int -> String
ind n = replicate (n * 4) ' '

renderParams :: [(String, String)] -> String
renderParams [] = "void"
renderParams ps = go ps
  where
    go [] = ""
    go [x] = pair x
    go (x:xs) = pair x ++ ", " ++ go xs
    pair (t, n) = t ++ " " ++ n
