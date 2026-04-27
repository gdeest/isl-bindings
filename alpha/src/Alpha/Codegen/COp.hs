-- | Typed operation descriptors for the Expr GADT.
--
-- 'Pw' and 'PMap' carry these tags so the expression tree is
-- renderable to C.  Polymorphic evaluation lives in "Alpha.Scalar".
module Alpha.Codegen.COp
  ( -- * Binary operations
    BinOp(..)
    -- * Unary operations
  , UnaryOp(..)
    -- * Reduction operations
  , ReduceOp(..)
  ) where



-- ═══════════════════════════════════════════════════════════════════════
-- §1. Binary operations
-- ═══════════════════════════════════════════════════════════════════════

data BinOp = OpAdd | OpSub | OpMul | OpDiv | OpMin | OpMax
  deriving (Show, Eq, Ord)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Unary operations
-- ═══════════════════════════════════════════════════════════════════════

data UnaryOp = OpNeg | OpAbs | OpFloor | OpCeil | OpSqrt
  deriving (Show, Eq, Ord)


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Reduction operations
-- ═══════════════════════════════════════════════════════════════════════

data ReduceOp = ReduceSum | ReduceProd | ReduceMin | ReduceMax
  deriving (Show, Eq, Ord)
