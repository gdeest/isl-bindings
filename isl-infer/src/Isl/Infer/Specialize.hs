-- | Runtime specialization: substitute concrete parameter values
-- into parametric polyhedra, eliminating all parameters.
--
-- This makes the Boulet-Feautrier scanner instant even for high-dimensional
-- polyhedra, because all bounds become concrete integer arithmetic
-- instead of symbolic expressions.
module Isl.Infer.Specialize
  ( specialize
  , specializeDis
  , specializeExpr
  , specializePartial
  ) where

import Isl.HighLevel.Constraints
  ( Conjunction(..), Constraint(..), Expr(..), SetIx(..) )
import Isl.HighLevel.Pure
  ( PConjunction(..), PDisjunction(..) )

-- | Substitute concrete parameter values into a conjunction.
-- Every @SetParam i@ is replaced with @Constant (params !! i)@.
-- The resulting conjunction has no parameters.
specialize :: [Integer] -> PConjunction ps n -> PConjunction '[] n
specialize params (PConjunction (Conjunction cs)) =
  PConjunction (Conjunction (map (specializeConstraint params) cs))

-- | Specialize a disjunction (union of conjunctions).
specializeDis :: [Integer] -> PDisjunction ps n -> PDisjunction '[] n
specializeDis params (PDisjunction pcs) =
  PDisjunction (map (specialize params) pcs)

-- | Partial specialization: substitute some parameters, keep others symbolic.
-- @Just v@ replaces the parameter with a constant, @Nothing@ keeps it.
-- Remaining parameter indices are renumbered to close gaps.
specializePartial :: [Maybe Integer] -> PConjunction ps n -> PConjunction '[] n
specializePartial params (PConjunction (Conjunction cs)) =
  PConjunction (Conjunction (map (specializeConstraintPartial params) cs))

specializeConstraintPartial :: [Maybe Integer] -> Constraint SetIx -> Constraint SetIx
specializeConstraintPartial params (EqualityConstraint e) =
  EqualityConstraint (specializeExprPartial params e)
specializeConstraintPartial params (InequalityConstraint e) =
  InequalityConstraint (specializeExprPartial params e)

-- | Like 'specializeExpr' but only substitutes @Just@ entries.
-- Remaining @SetParam@ indices are renumbered based on how many
-- earlier params were specialized away.
specializeExprPartial :: [Maybe Integer] -> Expr SetIx -> Expr SetIx
specializeExprPartial params = go
  where
    -- Build renumbering: for param i that is Nothing, its new index
    -- is i minus the count of Just entries before it
    renumber i = i - length [() | Just _ <- take i params]

    go (Ix (SetParam i))
      | i < length params, Just v <- params !! i = Constant v
      | otherwise = Ix (SetParam (renumber i))
    go (Ix (SetDim d))   = Ix (SetDim d)
    go (Constant k)      = Constant k
    go (Add a b)         = simplifyAdd (go a) (go b)
    go (Mul k e)         = simplifyMul k (go e)
    go (FloorDiv e d)    = simplifyFloorDiv (go e) d

specializeConstraint :: [Integer] -> Constraint SetIx -> Constraint SetIx
specializeConstraint params (EqualityConstraint e) =
  EqualityConstraint (specializeExpr params e)
specializeConstraint params (InequalityConstraint e) =
  InequalityConstraint (specializeExpr params e)

-- | Substitute parameter values in an expression.
-- @SetParam i@ → @Constant (params !! i)@
-- @SetDim i@ → unchanged
-- All arithmetic is simplified where possible.
specializeExpr :: [Integer] -> Expr SetIx -> Expr SetIx
specializeExpr params = go
  where
    go (Ix (SetParam i)) = Constant (params !! i)
    go (Ix (SetDim d))   = Ix (SetDim d)
    go (Constant k)      = Constant k
    go (Add a b)         = simplifyAdd (go a) (go b)
    go (Mul k e)         = simplifyMul k (go e)
    go (FloorDiv e d)    = simplifyFloorDiv (go e) d

-- | Simplify addition, folding constants.
simplifyAdd :: Expr ix -> Expr ix -> Expr ix
simplifyAdd (Constant 0) b = b
simplifyAdd a (Constant 0) = a
simplifyAdd (Constant a) (Constant b) = Constant (a + b)
simplifyAdd a b = Add a b

-- | Simplify scalar multiplication, folding constants.
simplifyMul :: Integer -> Expr ix -> Expr ix
simplifyMul 0 _ = Constant 0
simplifyMul 1 e = e
simplifyMul k (Constant c) = Constant (k * c)
simplifyMul k e = Mul k e

-- | Simplify floor division, evaluating when numerator is constant.
simplifyFloorDiv :: Expr ix -> Integer -> Expr ix
simplifyFloorDiv (Constant n) d
  | d > 0     = Constant (if n >= 0 then n `div` d else -(((-n) + d - 1) `div` d))
  | otherwise = Constant (negate (if n >= 0 then n `div` (negate d) else -(((-n) + (negate d) - 1) `div` (negate d))))
simplifyFloorDiv e d = FloorDiv e d
