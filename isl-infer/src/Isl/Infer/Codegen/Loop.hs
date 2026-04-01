-- | Translate 'LoopLevel' to C for-loops.
--
-- Handles equality constraints, multiple bounds (inline max/min),
-- stride, OpenMP parallel with collapse, and @#pragma omp simd@.
module Isl.Infer.Codegen.Loop
  ( loopNestToC
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Isl.Scan.Types (LoopLevel(..), LoopNest(..))
import Isl.Infer.Codegen.Bound (lowerBoundToC, upperBoundToC, equalityBoundToC)

-- | Intermediate representation for a single loop level's C code.
data LoopCode
  = LEquality String String    -- ^ @const int64_t name = expr;@
  | LFor
    { lfDim      :: Int
    , lfName     :: String
    , lfLoBounds :: [String]
    , lfHiBounds :: [String]
    , lfStride   :: Integer
    }

-- | Generate C code for an entire loop nest with a body.
loopNestToC
  :: [String]       -- ^ dimension names
  -> [String]       -- ^ parameter names
  -> Set Int        -- ^ parallel dimension indices
  -> Set Int        -- ^ SIMD dimension indices (empty = auto-simd innermost)
  -> LoopNest ps n  -- ^ the loop nest
  -> String         -- ^ C code for the innermost body
  -> String
loopNestToC dimNames paramNames parallelDims simdDims0 nest body =
  let levels = lnLevels nest
      codes  = map (levelToCode dimNames paramNames) levels
      -- Auto-SIMD: if no explicit SIMD dims, mark innermost non-equality non-parallel loop
      simdDims = if Set.null simdDims0
                 then case findInnermostLoop parallelDims levels of
                        Just d  -> Set.singleton d
                        Nothing -> Set.empty
                 else simdDims0
  in unlines (emitNest 1 0 parallelDims simdDims codes body)

levelToCode :: [String] -> [String] -> LoopLevel -> LoopCode
levelToCode dimNames paramNames level =
  let dim = llDim level
      name = dimNames !! dim
  in case llEquality level of
    Just eq -> LEquality name (equalityBoundToC dimNames paramNames eq)
    Nothing -> LFor
      { lfDim      = dim
      , lfName     = name
      , lfLoBounds = map (lowerBoundToC dimNames paramNames) (llLowerBounds level)
      , lfHiBounds = map (upperBoundToC dimNames paramNames) (llUpperBounds level)
      , lfStride   = llStride level
      }

-- | Emit the loop nest with correct pragma placement.
--
-- @collapseRemaining@: when > 0, we're inside a collapse region and
-- must not emit additional parallel pragmas.
emitNest :: Int -> Int -> Set Int -> Set Int -> [LoopCode] -> String -> [String]
emitNest depth _collapse _par _simd [] body =
  [ind depth ++ l | l <- lines body, not (null l)]
emitNest depth collapse par simd (code : rest) body =
  case code of
    LEquality name expr ->
      (ind depth ++ "const int64_t " ++ name ++ " = " ++ expr ++ ";")
      : emitNest depth collapse par simd rest body

    LFor dim name loBs hiBs stride ->
      let isParallel = dim `Set.member` par
          isSimd     = dim `Set.member` simd
          stepStr = if stride == 1 then name ++ "++" else name ++ " += " ++ show stride

          -- Compute how many loops to collapse (only for the first parallel loop)
          collapseCount = if isParallel && collapse == 0
                          then countConsecutiveParallel par (code : rest)
                          else 0

          -- Preamble: compute lo/hi bounds into temp vars if multiple bounds
          (loPre, loVar) = case loBs of
            (b:[]) -> ([], b)
            (b:bs) -> ( (ind depth ++ "int64_t " ++ name ++ "_lo = " ++ b ++ ";")
                        : [ ind depth ++ "if (" ++ x ++ " > " ++ name ++ "_lo) "
                            ++ name ++ "_lo = " ++ x ++ ";"
                          | x <- bs ]
                      , name ++ "_lo" )
            []     -> ([], "0")
          (hiPre, hiVar) = case hiBs of
            (b:[]) -> ([], b)
            (b:bs) -> ( (ind depth ++ "int64_t " ++ name ++ "_hi = " ++ b ++ ";")
                        : [ ind depth ++ "if (" ++ x ++ " < " ++ name ++ "_hi) "
                            ++ name ++ "_hi = " ++ x ++ ";"
                          | x <- bs ]
                      , name ++ "_hi" )
            []     -> ([], "0")

          -- Pragmas go AFTER preamble, BEFORE the for line
          parPragma
            | collapseCount > 1
              = [ind depth ++ "#pragma omp parallel for collapse("
                 ++ show collapseCount ++ ") schedule(static)"]
            | collapseCount == 1
              = [ind depth ++ "#pragma omp parallel for schedule(static)"]
            | otherwise = []  -- inside collapse or not parallel

          simdPragma
            | isSimd && not isParallel
              = [ind depth ++ "#pragma omp simd"]
            | otherwise = []

          forLine = ind depth ++ "for (int64_t " ++ name ++ " = " ++ loVar
                    ++ "; " ++ name ++ " <= " ++ hiVar
                    ++ "; " ++ stepStr ++ ") {"

          -- Track collapse: if we just started a collapse, inner loops decrement
          newCollapse
            | collapseCount > 0 = collapseCount - 1
            | collapse > 0      = collapse - 1
            | otherwise          = 0

      in loPre ++ hiPre ++ parPragma ++ simdPragma ++ [forLine]
         ++ emitNest (depth + 1) newCollapse par simd rest body
         ++ [ind depth ++ "}"]

-- | Count consecutive for-loops (skipping equalities) that are parallel.
countConsecutiveParallel :: Set Int -> [LoopCode] -> Int
countConsecutiveParallel _   []                   = 0
countConsecutiveParallel par (LEquality _ _ : cs)  = countConsecutiveParallel par cs
countConsecutiveParallel par (LFor dim _ _ _ _ : cs)
  | dim `Set.member` par = 1 + countConsecutiveParallel par cs
  | otherwise            = 0

-- | Find the innermost non-equality, non-parallel loop dimension.
findInnermostLoop :: Set Int -> [LoopLevel] -> Maybe Int
findInnermostLoop par levels =
  case [llDim l | l <- reverse levels, isCandidate l] of
    (d:_) -> Just d
    []    -> Nothing
  where
    isCandidate l = case llEquality l of
      Just _  -> False
      Nothing -> not (llDim l `Set.member` par)

ind :: Int -> String
ind n = replicate (n * 4) ' '
