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
  ( Conjunction(..), Constraint(..), SetIx(..), Expr, expandExpr )
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
    buildLevel _nDims dim =
      let -- Get constraints that involve this dimension AND only reference
          -- outer dimensions (< dim) or parameters — not inner dimensions (> dim).
          usable = filter (\c -> involvesDim dim c && onlyOuterDims dim c) constraints
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

-- | Get all indices referenced by a constraint.
constraintIxs :: Constraint SetIx -> [SetIx]
constraintIxs c = map snd (fst (expandConstraintExpr c))

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
