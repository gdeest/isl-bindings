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

import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Isl.AstBuild (CNode(..))


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Render a 'CNode' tree to C text.  For each @CFor@ whose nesting
-- depth matches a map key, the caller-supplied pragma line is emitted
-- immediately above.  Keys are 0-based schedule dim indices.
renderCNodeToC :: Map Int String -> CNode -> String
renderCNodeToC pragmas tree = fst (rn 2 0 tree)
  where
    -- Returns (rendered text, next dimIdx after this subtree)
    rn :: Int -> Int -> CNode -> (String, Int)
    rn d dimIdx (CFor iter ini cond inc body) =
      let pragma = case Map.lookup dimIdx pragmas of
            Just p  -> ind d ++ p ++ "\n"
            Nothing -> ""
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

    rn d dimIdx (CUser name args) =
      (ind d ++ name ++ "(" ++ intercalate ", " args ++ ");\n", dimIdx)

    ind :: Int -> String
    ind n = replicate n ' '
