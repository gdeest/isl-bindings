-- | Translate 'AffineBound' to C expressions.
--
-- An 'AffineBound' represents:
--   @(sum(coeff_i * loop_i) + sum(coeff_j * param_j) + constant) / divisor@
--
-- For lower bounds we need @ceil(expr / divisor)@; for upper bounds
-- we need @floor(expr / divisor)@. These must match the semantics of
-- 'evalLower' and 'evalUpper' in @Isl.Scan.Enumerate@.
module Isl.Infer.Codegen.Bound
  ( boundToC
  , lowerBoundToC
  , upperBoundToC
  , equalityBoundToC
  ) where

import Isl.Scan.Types (AffineBound(..))

-- | Render the numerator of an 'AffineBound' as a C expression.
-- @dimNames@ and @paramNames@ map indices to C variable names.
boundToC :: [String] -> [String] -> AffineBound -> String
boundToC dimNames paramNames ab =
  let loopTerms  = [renderTerm c (dimNames !! i)   | (c, i) <- abLoopCoeffs ab]
      paramTerms = [renderTerm c (paramNames !! i)  | (c, i) <- abParamCoeffs ab]
      constTerm  = if abConstant ab == 0 then [] else [show (abConstant ab)]
      allTerms   = loopTerms ++ paramTerms ++ constTerm
  in case allTerms of
       []  -> "0"
       _   -> joinTerms allTerms

-- | Lower bound: @ceil(numerator / divisor)@.
-- When divisor == 1, just the numerator.
lowerBoundToC :: [String] -> [String] -> AffineBound -> String
lowerBoundToC dimNames paramNames ab
  | abDivisor ab == 1 = boundToC dimNames paramNames ab
  | otherwise =
      "ISL_CEIL_DIV((" ++ boundToC dimNames paramNames ab ++ "), " ++ show (abDivisor ab) ++ ")"

-- | Upper bound: @floor(numerator / divisor)@.
-- When divisor == 1, just the numerator.
upperBoundToC :: [String] -> [String] -> AffineBound -> String
upperBoundToC dimNames paramNames ab
  | abDivisor ab == 1 = boundToC dimNames paramNames ab
  | otherwise =
      "ISL_FLOOR_DIV((" ++ boundToC dimNames paramNames ab ++ "), " ++ show (abDivisor ab) ++ ")"

-- | Equality bound: @numerator / divisor@ (exact division).
equalityBoundToC :: [String] -> [String] -> AffineBound -> String
equalityBoundToC dimNames paramNames ab
  | abDivisor ab == 1 = boundToC dimNames paramNames ab
  | otherwise =
      "((" ++ boundToC dimNames paramNames ab ++ ") / " ++ show (abDivisor ab) ++ ")"

-- | Render a single term: @coeff * name@.
renderTerm :: Integer -> String -> String
renderTerm 1 name    = name
renderTerm (-1) name = "(-" ++ name ++ ")"
renderTerm c name    = show c ++ " * " ++ name

-- | Join terms with @+@ handling negative terms.
joinTerms :: [String] -> String
joinTerms [] = "0"
joinTerms [t] = t
joinTerms (t:ts) = t ++ concatMap addTerm ts
  where
    addTerm s@('(':'-':_) = " " ++ s        -- (-x) renders as " (-x)" i.e. subtraction
    addTerm s@('-':_)     = " " ++ s
    addTerm s             = " + " ++ s
