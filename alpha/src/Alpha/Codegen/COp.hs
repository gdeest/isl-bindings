-- | Typed operation descriptors for the Expr GADT.
--
-- These replace the opaque Haskell functions previously stored in
-- 'Pw' and 'PMap', making the expression tree renderable to C.
-- The interpreter evaluates them at 'Double'.
module Alpha.Codegen.COp
  ( -- * Binary operations
    BinOp(..)
  , evalBinOp
    -- * Unary operations
  , UnaryOp(..)
  , evalUnaryOp
    -- * Reduction operations
  , ReduceOp(..)
  , evalReduceOp
  ) where


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Binary operations
-- ═══════════════════════════════════════════════════════════════════════

data BinOp = OpAdd | OpSub | OpMul | OpDiv | OpMin | OpMax
  deriving (Show, Eq, Ord)

evalBinOp :: BinOp -> Double -> Double -> Double
evalBinOp OpAdd = (+)
evalBinOp OpSub = (-)
evalBinOp OpMul = (*)
evalBinOp OpDiv = (/)
evalBinOp OpMin = min
evalBinOp OpMax = max


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Unary operations
-- ═══════════════════════════════════════════════════════════════════════

data UnaryOp = OpNeg | OpAbs | OpFloor | OpCeil | OpSqrt
  deriving (Show, Eq, Ord)

evalUnaryOp :: UnaryOp -> Double -> Double
evalUnaryOp OpNeg   = negate
evalUnaryOp OpAbs   = abs
evalUnaryOp OpFloor = fromIntegral . (floor :: Double -> Integer)
evalUnaryOp OpCeil  = fromIntegral . (ceiling :: Double -> Integer)
evalUnaryOp OpSqrt  = sqrt


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Reduction operations
-- ═══════════════════════════════════════════════════════════════════════

data ReduceOp = ReduceSum | ReduceProd | ReduceMin | ReduceMax
  deriving (Show, Eq, Ord)

-- | Fold step: @evalReduceOp op acc val = acc `op` val@.
evalReduceOp :: ReduceOp -> Double -> Double -> Double
evalReduceOp ReduceSum  = (+)
evalReduceOp ReduceProd = (*)
evalReduceOp ReduceMin  = min
evalReduceOp ReduceMax  = max
