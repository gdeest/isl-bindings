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
renderCNodeToC anns tree = fst (rn 2 0 tree)
  where
    -- Returns (rendered text, next dimIdx after this subtree)
    rn :: Int -> Int -> CNode -> (String, Int)
    rn d dimIdx (CFor iter ini cond inc body) =
      let pragma = case Map.lookup dimIdx anns of
            Just Parallel  -> ind d ++ "#pragma omp parallel for schedule(static)\n"
            Just Vectorize -> ind d ++ "#pragma omp simd\n"
            Nothing        -> ""
          (bodyStr, dimIdx') = rn (d + 2) (dimIdx + 1) body
      in ( pragma
           ++ ind d ++ "for (int " ++ iter ++ " = " ++ ini ++ "; "
           ++ cond ++ "; " ++ iter ++ " += " ++ inc ++ ") {\n"
           ++ bodyStr
           ++ ind d ++ "}\n"
         , dimIdx' )

    rn d dimIdx (CIf cond thn mels) =
      let (thnStr, dimIdx') = rn (d + 2) dimIdx thn
          (elsStr, dimIdx'') = case mels of
            Just e  -> let (s, di) = rn (d + 2) dimIdx' e in (ind d ++ "} else {\n" ++ s, di)
            Nothing -> ("", dimIdx')
      in ( ind d ++ "if (" ++ cond ++ ") {\n"
           ++ thnStr
           ++ elsStr
           ++ ind d ++ "}\n"
         , dimIdx'' )

    rn d dimIdx (CBlock children) =
      let go di [] = ("", di)
          go di (c:cs) = let (s, di') = rn d di c
                             (rest, di'') = go di' cs
                         in (s ++ rest, di'')
      in go dimIdx children

    rn d dimIdx (CUser stmt) =
      (ind d ++ stmt ++ "\n", dimIdx)

    ind :: Int -> String
    ind n = replicate n ' '
