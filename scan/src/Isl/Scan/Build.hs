{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Build a 'Scanner' from pure constraint representations.
--
-- Implements the Boulet-Feautrier algorithm: for each dimension,
-- partition constraints into lower bounds, upper bounds, and equalities,
-- then extract affine bound expressions in terms of outer loop
-- variables and parameters.
module Isl.Scan.Build
  ( mkScanner
  , mkLoopNest
  , buildLevels
  ) where

import Data.List (partition)
import GHC.TypeLits (Nat, Symbol)

import Isl.HighLevel.Constraints
  ( Conjunction(..), Constraint(..), SetIx(..), Expr(..), expandExpr
  , hasFloorDiv )
import Isl.HighLevel.Pure (PConjunction(..), PDisjunction(..))

import Isl.Scan.Types

-- | Build a 'Scanner' from a 'PDisjunction' (union of convex polyhedra).
--
-- Each basic set in the disjunction becomes one 'LoopNest'.
-- For correct enumeration without duplicates, the input should have been
-- made disjoint (via @isl_set_make_disjoint@) before decomposition.
mkScanner :: forall (ps :: [Symbol]) (n :: Nat). PDisjunction ps n -> Scanner ps n
mkScanner (PDisjunction pcs) = Scanner (map mkLoopNest pcs)

-- | Build a 'LoopNest' from a single convex polyhedron.
--
-- For each dimension k (0 = outermost, n-1 = innermost):
--
-- 1. Partition constraints into those involving x_k and those not.
-- 2. Among those involving x_k, classify by coefficient sign:
--    positive = lower bound, negative = upper bound, zero coeff but
--    in an equality = equality constraint.
-- 3. Normalize each into an 'AffineBound' by dividing out the
--    coefficient of x_k.
mkLoopNest :: forall (ps :: [Symbol]) (n :: Nat). PConjunction ps n -> LoopNest ps n
mkLoopNest (PConjunction (Conjunction constraints)) =
  let (levels, nParams, nDims) = buildLevels constraints
  in LoopNest levels nParams nDims

buildLevels :: [Constraint SetIx] -> ([LoopLevel], Int, Int)
buildLevels constraints =
  let -- Determine dimensions present
      allIxs = concatMap constraintIxs constraints
      nDims = if null [i | SetDim i <- allIxs] then 0
              else maximum [i | SetDim i <- allIxs] + 1
      nParams = if null [i | SetParam i <- allIxs] then 0
                else maximum [i | SetParam i <- allIxs] + 1
      levels = map (buildLevel nDims) [0 .. nDims - 1]
  in (levels, nParams, nDims)
  where
    buildLevel :: Int -> Int -> LoopLevel
    buildLevel nDims dim =
      let -- Drop constraints with FloorDiv (existential relationships from
          -- tiling maps — the useful bounds come from the simplified constraints).
          linear = filter (not . constraintHasFloorDiv) constraints
          -- Fourier-Motzkin: eliminate all inner dimensions (> dim) to derive
          -- implied bounds that only reference dim, outer dims, and params.
          projected = fmEliminateInner dim nDims linear
          -- Now filter for constraints involving this dimension with outer-only refs.
          usable = filter (\c -> involvesDim dim c && onlyOuterDims dim c) projected
          -- Classify: equalities with this dim, inequalities with this dim
          (eqs, ineqs) = partition isEquality usable
          -- For equalities: extract the bound
          eqBound = case eqs of
            (c:_) -> Just (extractBound dim c)
            []    -> Nothing
          -- For inequalities: partition into lower and upper by coefficient sign
          (lowers, uppers) = partitionBounds dim ineqs
      in LoopLevel
          { llDim = dim
          , llLowerBounds = lowers
          , llUpperBounds = uppers
          , llEquality = eqBound
          , llStride = 1
          }

-- | Fourier-Motzkin elimination of all inner dimensions (> dim).
--
-- For each inner dimension j (from innermost to dim+1), eliminate j by
-- combining pairs of constraints with opposite-sign coefficients on j.
-- This derives implied bounds that only reference dim, outer dims, and params.
fmEliminateInner :: Int -> Int -> [Constraint SetIx] -> [Constraint SetIx]
fmEliminateInner dim nDims cs = foldr fmEliminate cs [nDims - 1, nDims - 2 .. dim + 1]

-- | Eliminate a single variable (SetDim j) from a constraint set.
--
-- Partition constraints into: those with positive coeff on j (lower bounds),
-- negative coeff (upper bounds), and those not involving j (passthrough).
-- Combine each (lower, upper) pair to produce a new constraint without j.
-- Equalities are split into two inequalities first.
fmEliminate :: Int -> [Constraint SetIx] -> [Constraint SetIx]
fmEliminate j cs =
  let -- Only split equalities that involve dimension j into inequality pairs.
      -- Equalities not involving j pass through unchanged, preserving them
      -- for later detection by buildLevel.
      expanded = concatMap splitIfInvolves cs
      -- Partition by coefficient sign on dimension j
      (pos, neg, pass) = partitionByCoeff j expanded
      -- Combine each (pos, neg) pair: scale to eliminate j
      combined = [combineFM j p n | p <- pos, n <- neg]
  in pass ++ combined
  where
    splitIfInvolves :: Constraint SetIx -> [Constraint SetIx]
    splitIfInvolves c@(EqualityConstraint e)
      | involvesVar j e = [InequalityConstraint e, InequalityConstraint (negateExpr e)]
      | otherwise       = [c]  -- preserve equality if it doesn't involve j
    splitIfInvolves c = [c]

    involvesVar :: Int -> Expr SetIx -> Bool
    involvesVar d e =
      let (coeffs, _) = expandExpr e
      in any (\(_, ix) -> ix == SetDim d) coeffs

-- | Partition constraints by coefficient sign on dimension j.
-- Returns (positive-coeff, negative-coeff, not-involving-j).
partitionByCoeff :: Int -> [Constraint SetIx] -> ([Constraint SetIx], [Constraint SetIx], [Constraint SetIx])
partitionByCoeff j = go [] [] []
  where
    go pos neg pass [] = (pos, neg, pass)
    go pos neg pass (c:cs) =
      let (coeffs, _) = expandConstraintExpr c
      in case lookupCoeff (SetDim j) coeffs of
           Nothing -> go pos neg (c:pass) cs
           Just k
             | k > 0     -> go (c:pos) neg pass cs
             | k < 0     -> go pos (c:neg) pass cs
             | otherwise -> go pos neg (c:pass) cs  -- zero coeff, treat as pass

-- | Combine two inequality constraints to eliminate dimension j.
--
-- Given: a_j * x_j + rest_a >= 0  (a_j > 0)
--        b_j * x_j + rest_b >= 0  (b_j < 0)
-- Multiply first by |b_j|, second by a_j, and add:
--        |b_j| * rest_a + a_j * rest_b >= 0
combineFM :: Int -> Constraint SetIx -> Constraint SetIx -> Constraint SetIx
combineFM j (InequalityConstraint ePos) (InequalityConstraint eNeg) =
  let (posCoeffs, posConst) = expandExpr ePos
      (negCoeffs, negConst) = expandExpr eNeg
      aJ = case lookupCoeff (SetDim j) posCoeffs of
             Just k -> k
             Nothing -> error "combineFM: missing pos coeff"
      bJ = case lookupCoeff (SetDim j) negCoeffs of
             Just k -> k
             Nothing -> error "combineFM: missing neg coeff"
      scalePosBy = abs bJ
      scaleNegBy = aJ    -- aJ > 0
      -- Scale and sum all coefficients except j
      posOthers = [(c * scalePosBy, ix) | (c, ix) <- posCoeffs, ix /= SetDim j]
      negOthers = [(c * scaleNegBy, ix) | (c, ix) <- negCoeffs, ix /= SetDim j]
      combinedConst = posConst * scalePosBy + negConst * scaleNegBy
      combinedCoeffs = mergeCoeffs (posOthers ++ negOthers)
  in InequalityConstraint (rebuildExpr combinedCoeffs combinedConst)
combineFM _ _ _ = error "combineFM: expected two inequality constraints"

-- | Merge coefficient lists by summing coefficients for the same index.
mergeCoeffs :: [(Integer, SetIx)] -> [(Integer, SetIx)]
mergeCoeffs = foldl addCoeff []
  where
    addCoeff [] (c, ix) = [(c, ix)]
    addCoeff ((c1, ix1):rest) (c2, ix2)
      | ix1 == ix2 = (c1 + c2, ix1) : rest
      | otherwise  = (c1, ix1) : addCoeff rest (c2, ix2)

-- | Negate all terms in an expression.
negateExpr :: Expr SetIx -> Expr SetIx
negateExpr e =
  let (coeffs, constant) = expandExpr e
  in rebuildExpr [(-c, ix) | (c, ix) <- coeffs] (-constant)

-- | Rebuild an Expr from coefficients and constant.
rebuildExpr :: [(Integer, SetIx)] -> Integer -> Expr SetIx
rebuildExpr coeffs constant =
  let terms = [mkTerm c ix | (c, ix) <- coeffs, c /= 0]
      base = if constant /= 0 || null terms
             then Constant constant
             else head terms
      rest = if constant /= 0 || null terms then terms else tail terms
  in foldl Add base rest
  where
    mkTerm 1 ix = Ix ix
    mkTerm c ix = Mul c (Ix ix)

-- | Check if a constraint contains FloorDiv expressions.
constraintHasFloorDiv :: Constraint SetIx -> Bool
constraintHasFloorDiv (EqualityConstraint e) = hasFloorDiv e
constraintHasFloorDiv (InequalityConstraint e) = hasFloorDiv e

-- | Check if a constraint involves a given set dimension.
involvesDim :: Int -> Constraint SetIx -> Bool
involvesDim dim c =
  let (coeffs, _) = expandConstraintExpr c
  in any (\(_, ix) -> ix == SetDim dim) coeffs

-- | Check that a constraint's set dimension references (other than @dim@)
-- are all outer (index < dim). Parameters are always OK.
onlyOuterDims :: Int -> Constraint SetIx -> Bool
onlyOuterDims dim c =
  let (coeffs, _) = expandConstraintExpr c
  in all (\(_, ix) -> case ix of
            SetDim i  -> i <= dim  -- allow the dim itself and outer dims
            SetParam _ -> True
         ) coeffs

-- | Extract the affine expression from a constraint.
expandConstraintExpr :: Constraint SetIx -> ([(Integer, SetIx)], Integer)
expandConstraintExpr (EqualityConstraint e) = expandExpr e
expandConstraintExpr (InequalityConstraint e) = expandExpr e

isEquality :: Constraint SetIx -> Bool
isEquality (EqualityConstraint _) = True
isEquality _ = False

-- | Get all indices referenced by a constraint (handles FloorDiv).
constraintIxs :: Constraint SetIx -> [SetIx]
constraintIxs (EqualityConstraint e) = exprIxs e
constraintIxs (InequalityConstraint e) = exprIxs e

-- | Extract all variable indices from an expression, including inside FloorDiv.
exprIxs :: Expr SetIx -> [SetIx]
exprIxs (Ix ix)         = [ix]
exprIxs (Constant _)    = []
exprIxs (Mul _ e)       = exprIxs e
exprIxs (Add a b)       = exprIxs a ++ exprIxs b
exprIxs (FloorDiv e _)  = exprIxs e

-- | Extract an 'AffineBound' from a constraint for a given dimension.
--
-- For a constraint @c_k * x_k + (other terms) + constant >= 0@ (inequality)
-- or @c_k * x_k + (other terms) + constant = 0@ (equality):
--
-- Solve for x_k: @x_k = -(other terms + constant) / c_k@
--
-- The "other terms" are split into loop variable refs (outer dims)
-- and parameter refs.
extractBound :: Int -> Constraint SetIx -> AffineBound
extractBound dim c =
  let (coeffs, constant) = expandConstraintExpr c
      -- coeffs is [(Integer, SetIx)] — (coefficient, index)
      -- Find coefficient of the target dimension
      dimCoeff = case lookupCoeff (SetDim dim) coeffs of
                   Just k  -> k
                   Nothing -> error "extractBound: dimension not in constraint"
      -- Other terms (excluding the target dimension)
      others = filter (\(_, ix) -> ix /= SetDim dim) coeffs
      -- Solve: x_k = -(others + constant) / dimCoeff
      -- So negate all other coefficients and the constant
      absDivisor = abs dimCoeff
      sign = signum dimCoeff  -- +1 or -1
      -- When dimCoeff > 0: x_k >= -(others + constant) / dimCoeff
      -- When dimCoeff < 0: x_k <= -(others + constant) / dimCoeff
      -- In both cases, we negate and divide by |dimCoeff|,
      -- but the sign determines whether it's a lower or upper bound.
      -- The caller (partitionBounds) handles the lower/upper distinction.
      -- Here we just produce the raw bound: -(others + constant) / |dimCoeff|
      -- with sign already folded in.
      loopCoeffs = [(-sign * coeff, i) | (coeff, SetDim i) <- others]
      paramCoeffs = [(-sign * coeff, i) | (coeff, SetParam i) <- others]
  in AffineBound
      { abLoopCoeffs = loopCoeffs
      , abParamCoeffs = paramCoeffs
      , abConstant = -sign * constant
      , abDivisor = absDivisor
      }

-- | Partition inequality constraints involving dimension @dim@ into
-- lower bounds (positive coefficient on x_k) and upper bounds
-- (negative coefficient on x_k).
--
-- For @c_k * x_k + rest >= 0@:
--   - c_k > 0 → lower bound: @x_k >= -(rest) / c_k@
--   - c_k < 0 → upper bound: @x_k <= -(rest) / |c_k|@ (flip sign)
partitionBounds :: Int -> [Constraint SetIx] -> ([AffineBound], [AffineBound])
partitionBounds dim ineqs =
  let classified = map classifyAndExtract ineqs
      lowers = [b | (True, b) <- classified]
      uppers = [b | (False, b) <- classified]
  in (lowers, uppers)
  where
    classifyAndExtract :: Constraint SetIx -> (Bool, AffineBound)
    classifyAndExtract c =
      let (coeffs, _) = expandConstraintExpr c
          dimCoeff = case lookupCoeff (SetDim dim) coeffs of
                       Just k  -> k
                       Nothing -> error "partitionBounds: dim not in constraint"
          isLower = dimCoeff > 0
      in (isLower, extractBound dim c)

-- | Look up the coefficient for a given index in a list of (coefficient, index) pairs.
lookupCoeff :: Eq ix => ix -> [(Integer, ix)] -> Maybe Integer
lookupCoeff target = go
  where
    go [] = Nothing
    go ((coeff, ix) : rest)
      | ix == target = Just coeff
      | otherwise    = go rest
