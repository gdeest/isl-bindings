{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Isl.HighLevel.Constraints where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import GHC.Generics (Generic)

-- | Dimension index for set constraints, distinguishing dimensions from parameters.
data SetIx = SetDim !Int | SetParam !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData SetIx

-- | Dimension index for map constraints, distinguishing input, output, and parameters.
data MapIx = InDim !Int | OutDim !Int | MapParam !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData MapIx

-- | Affine expressions, with variables of type 'ix'.
data Expr ix
  = Ix ix
  | Constant Integer
  | Mul Integer (Expr ix)
  | Add (Expr ix) (Expr ix)
  deriving (Generic)

instance NFData ix => NFData (Expr ix)

infixl 4 -:
infixl 4 +:
infixr 5 *:

infix 3 ==:
infix 3 <=:
infix 3 >=:

infixl 2 &&:


(&&:) :: ToConjunction c => c ix -> Constraint ix -> Conjunction ix
(&&:) conj constr = Conjunction (constr:cs)
  where Conjunction cs = toConjunction conj

(-:) :: Expr ix -> Expr ix -> Expr ix
e1 -: e2 = e1 +: (-1) *: e2

(+:) :: Expr ix -> Expr ix -> Expr ix
e1 +: e2 = Add e1 e2

(*:) :: Integer -> Expr ix -> Expr ix
k *: e1 = Mul k e1


cst :: Integer -> Expr ix
cst = Constant

idx :: ix -> Expr ix
idx = Ix

(<=:) :: Expr ix -> Expr ix -> Constraint ix
e1 <=: e2 = InequalityConstraint $ Add e2 (Mul (-1) e1)

(>=:) :: Expr ix -> Expr ix -> Constraint ix
e1 >=: e2 = e2 <=: e1

(==:) :: Expr ix -> Expr ix -> Constraint ix
e1 ==: e2 = EqualityConstraint $ Add e2 (Mul (-1) e1)


-- | Affine equality / inequality constraints, with variable type 'ix'.
--
-- While equality constraints can be represented as a pair of inequality
-- constraints, they get special treatment in Isl for performance reasons.
data Constraint ix
  = EqualityConstraint (Expr ix)
    -- ^ 'EqualityConstraint e' represents the constraint 'e = 0'.
  | InequalityConstraint (Expr ix)
    -- ^ 'InequalityConstraint e' represents the constraint 'e >= 0'.
  deriving (Generic)

instance NFData ix => NFData (Constraint ix)

-- | Represents a conjunction of constraints, defining a single convex
-- polyhedron.
newtype Conjunction ix = Conjunction [Constraint ix]
  deriving (Generic)

instance NFData ix => NFData (Conjunction ix)

deriving instance Show ix => Show (Expr ix)
deriving instance Show ix => Show (Constraint ix)
deriving instance Show ix => Show (Conjunction ix)

deriving instance Eq ix => Eq (Expr ix)
deriving instance Eq ix => Eq (Constraint ix)
deriving instance Eq ix => Eq (Conjunction ix)

class ToConjunction (c :: Type -> Type) where
  toConjunction :: c ix -> Conjunction ix

instance ToConjunction Conjunction where
  toConjunction = id

instance ToConjunction Constraint where
  toConjunction = Conjunction . pure

-- | Legacy alias — use 'MapIx' for new code.
type MapDim = MapIx

-- | Reconstruct an 'Expr' from a list of (coefficient, variable) pairs and
-- a constant offset. Inverse of 'expandExpr'.
--
-- @expandExpr (rebuildExpr coeffs c) ≈ (coeffs, c)@ (modulo zero-coeff filtering)
rebuildExpr :: [(Integer, ix)] -> Integer -> Expr ix
rebuildExpr coeffs constant =
  let terms = [if c == 1 then Ix i else Mul c (Ix i) | (c, i) <- coeffs, c /= 0]
      constTerm = [Constant constant | constant /= 0]
      allTerms = terms ++ constTerm
  in case allTerms of
    []     -> Constant 0
    [t]    -> t
    (t:ts) -> foldl Add t ts

-- | Expands an affine expression to a set of coefficients / variable pairs,
-- plus a constant offset.
--
-- Variables are guaranteed to appear only once in the result, and increasing
-- order.
expandExpr :: Ord ix => Expr ix -> ([(Integer, ix)], Integer)
expandExpr (Ix ix) = ([(1, ix)], 0)
expandExpr (Constant k) = ([], k)
expandExpr (Mul k e) =
  ((\(k', ix) -> (k*k', ix)) <$> linearTerms, k*co)
  where (linearTerms, co) = expandExpr e
expandExpr (Add e1 e2) = merge (expandExpr e1) (expandExpr e2)
  where merge (lt1, c1) (lt2, c2) = (mergeTerms lt1 lt2, c1+c2)
        mergeTerms [] terms = terms
        mergeTerms terms [] = terms
        mergeTerms ts1@(t1@(coeff1, ix1):rst1) ts2@(t2@(coeff2, ix2):rst2)
          | ix1 < ix2 = t1:(mergeTerms rst1 ts2)
          | ix2 < ix1 = t2:(mergeTerms ts1 rst2)
          | otherwise = (coeff1+coeff2, ix1):(mergeTerms rst1 rst2)
