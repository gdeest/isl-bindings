-- | Render ISL-generated 'CNode' trees to C text with pragma insertion.
--
-- ISL's AST builder produces a 'CNode' tree (for-loops, if-guards,
-- blocks, user statements).  This module renders that tree to C text,
-- inserting @#pragma omp parallel for@ and @#pragma omp simd@
-- annotations at the appropriate loop levels based on schedule
-- dimension annotations.
--
-- The mapping from 'CFor' nesting depth to schedule dimension index
-- is by counting: each 'CFor' node encountered increments a dimension
-- counter.  'CIf' guards do NOT increment the counter (they are not
-- schedule dimensions).
module Alpha.Codegen.CRender
  ( renderCNodeToC
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Isl.AstBuild (CNode(..))
import Alpha.Schedule (DimAnnotation(..))


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Render a 'CNode' tree to C text with OpenMP pragma insertion.
--
-- The annotation map keys are schedule dimension indices (0-based,
-- matching 'CFor' nesting depth).
renderCNodeToC :: Map Int DimAnnotation -> CNode -> String
renderCNodeToC anns tree = rn 2 0 tree
  where
    rn :: Int -> Int -> CNode -> String
    rn d dimIdx (CFor iter ini cond inc body) =
      let pragma = case Map.lookup dimIdx anns of
            Just Parallel  -> ind d ++ "#pragma omp parallel for schedule(static)\n"
            Just Vectorize -> ind d ++ "#pragma omp simd\n"
            Nothing        -> ""
      in pragma
         ++ ind d ++ "for (int " ++ iter ++ " = " ++ ini ++ "; "
         ++ cond ++ "; " ++ iter ++ " += " ++ inc ++ ") {\n"
         ++ rn (d + 2) (dimIdx + 1) body
         ++ ind d ++ "}\n"

    rn d dimIdx (CIf cond thn mels) =
      ind d ++ "if (" ++ cond ++ ") {\n"
      ++ rn (d + 2) dimIdx thn
      ++ ind d ++ "}"
      ++ maybe "\n" (\e -> " else {\n" ++ rn (d + 2) dimIdx e ++ ind d ++ "}\n") mels

    rn d dimIdx (CBlock children) =
      concatMap (rn d dimIdx) children

    rn d _dimIdx (CUser stmt) =
      ind d ++ stmt ++ "\n"

    ind :: Int -> String
    ind n = replicate n ' '
