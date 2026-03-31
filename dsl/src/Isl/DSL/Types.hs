{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Core AST types for the polyhedral DSL.
--
-- Two expression types: 'AExpr' (affine, for subscripts and bounds)
-- and 'ValExpr' (arbitrary, for computation bodies). Both have 'Num'
-- instances. 'AExpr' enforces affine-by-construction (no var*var).
module Isl.DSL.Types
  ( -- * Phantom-tagged index
    Ix(..)
  , ix
    -- * Affine expressions (subscripts, bounds, schedule coords)
  , AExpr(..)
  , div_
  , mod_
    -- * Value expressions (computation bodies, reified AST)
  , ValExpr(..)
  , BinOp(..)
    -- * Ranges
  , Range(..)
  , range
    -- * Array declarations
  , ArrayDecl(..)
  , WritePolicy(..)
    -- * Loop AST
  , LoopNode(..)
  , StmtId(..)
    -- * References
  , ArrayRef(..)
  , StmtRef(..)
    -- * Schedule tree
  , SchedTree(..)
  , SchedDim(..)
  , SchedCtx(..)
    -- * Transforms
  , Transform(..)
    -- * Memory mappings
  , MemMapDef
    -- * Program
  , Program(..)
    -- * Extracted statements (shared between Compile and Verify)
  , ExtractedStmt(..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Isl.HighLevel.Constraints (Conjunction, SetIx)


-- ---------------------------------------------------------------------------
-- Phantom-tagged index
-- ---------------------------------------------------------------------------

-- | An iteration variable bound by 'for_', tagged with its type-level name.
-- Use 'ix' to unwrap to 'AExpr' for arithmetic.
newtype Ix (name :: Symbol) = Ix AExpr

-- | Unwrap a typed index to a plain affine expression.
ix :: Ix name -> AExpr
ix (Ix e) = e


-- ---------------------------------------------------------------------------
-- Affine expressions
-- ---------------------------------------------------------------------------

-- | Affine expressions over iteration variables and parameters.
-- Affine by construction: 'AMul' only allows scalar (Integer) * expression,
-- and there is no variable * variable operation.
data AExpr
  = AVar !String          -- ^ Iteration variable reference
  | AParam !String        -- ^ Parameter reference
  | ALit !Integer         -- ^ Integer literal
  | AAdd AExpr AExpr      -- ^ Addition
  | ANeg AExpr            -- ^ Negation
  | AMul !Integer AExpr   -- ^ Scalar multiplication (affine!)
  | AFloorDiv AExpr !Integer  -- ^ @floor(expr / k)@ — quasi-affine
  | AMod AExpr !Integer       -- ^ @expr mod k@ — quasi-affine
  deriving (Show, Eq, Generic)

-- | @floor(expr / k)@
div_ :: AExpr -> Integer -> AExpr
div_ = AFloorDiv

-- | @expr mod k@
mod_ :: AExpr -> Integer -> AExpr
mod_ = AMod

instance Num AExpr where
  (+) = AAdd
  a - b = AAdd a (ANeg b)
  -- (*) is only valid for literal * expr. We check at runtime.
  -- This allows fromInteger n * expr to work via Num.
  ALit n * b = AMul n b
  a * ALit n = AMul n a
  _ * _ = error "AExpr: non-affine multiplication (variable * variable). \
                \Use AMul for scalar multiplication only."
  negate = ANeg
  abs = error "AExpr: abs not supported"
  signum = error "AExpr: signum not supported"
  fromInteger = ALit


-- ---------------------------------------------------------------------------
-- Value expressions (reified AST for computation bodies)
-- ---------------------------------------------------------------------------

-- | Binary operations on value expressions.
data BinOp = VAdd | VSub | VMul | VDiv
  deriving (Show, Eq, Generic)

-- | Value expressions — the reified AST for the RHS of assignments.
-- Opaque to polyhedral analysis; array access patterns are extracted
-- from 'VRead' nodes.
data ValExpr
  = VRead !String [AExpr]   -- ^ Array read: name + affine subscripts
  | VLit !Rational           -- ^ Numeric literal
  | VBin BinOp ValExpr ValExpr  -- ^ Binary operation
  | VNeg ValExpr             -- ^ Negation
  | VLift AExpr              -- ^ Promote affine expression to value
  | VApp !String [ValExpr]   -- ^ Named function application (escape hatch)
  deriving (Show, Eq, Generic)

instance Num ValExpr where
  (+) = VBin VAdd
  (-) = VBin VSub
  (*) = VBin VMul
  negate = VNeg
  abs = error "ValExpr: abs not supported"
  signum = error "ValExpr: signum not supported"
  fromInteger = VLit . fromInteger

instance Fractional ValExpr where
  (/) = VBin VDiv
  fromRational = VLit


-- ---------------------------------------------------------------------------
-- Ranges
-- ---------------------------------------------------------------------------

-- | An inclusive range @[lo, hi]@ of affine expressions.
data Range = Range AExpr AExpr
  deriving (Show, Eq, Generic)

-- | @range lo hi@ = inclusive range @[lo, hi]@.
range :: AExpr -> AExpr -> Range
range = Range


-- ---------------------------------------------------------------------------
-- Array declarations
-- ---------------------------------------------------------------------------

-- | Write policy for an array.
data WritePolicy
  = AnyWrite     -- ^ No constraint on write multiplicity
  | ExactWrite   -- ^ Each cell must be written exactly once
  deriving (Show, Eq, Generic)

-- | Array declaration with polyhedral bounds.
data ArrayDecl = ArrayDecl
  { adName   :: !String
  , adBounds :: [Range]        -- ^ Per-dimension bounds (affine in params)
  , adPolicy :: !WritePolicy
  } deriving (Show, Eq, Generic)


-- ---------------------------------------------------------------------------
-- Loop AST
-- ---------------------------------------------------------------------------

-- | Unique statement identifier (assigned by the builder).
newtype StmtId = StmtId Int
  deriving (Show, Eq, Ord, Generic)

-- | Loop nest AST node. The Haskell loop structure IS the initial schedule tree.
data LoopNode
  = ForLoop !String AExpr AExpr [LoopNode]
    -- ^ @for name in [lo, hi]: body@
  | Assign !StmtId !String [AExpr] ValExpr
    -- ^ @array[subs] = expr@ — a single statement
  | Guard AExpr [LoopNode]
    -- ^ Conditional: @when (cond): body@
  | Seq [LoopNode]
    -- ^ Sequential composition
  deriving (Show, Eq, Generic)


-- ---------------------------------------------------------------------------
-- References
-- ---------------------------------------------------------------------------

-- | Reference to an array (returned by 'array' in the builder).
newtype ArrayRef = ArrayRef String
  deriving (Show, Eq, Ord, Generic)

-- | Reference to a statement (returned by '<==').
newtype StmtRef = StmtRef StmtId
  deriving (Show, Eq, Ord, Generic)


-- ---------------------------------------------------------------------------
-- Schedule tree
-- ---------------------------------------------------------------------------

-- | A single named schedule dimension within a band.
data SchedDim = SchedDim
  { sdName :: !String
  , sdExpr :: AExpr        -- ^ Expression in terms of iteration variables
  } deriving (Show, Eq, Generic)

-- | Schedule tree — mirrors ISL's schedule tree structure.
data SchedTree
  = STBand !String [SchedDim] [SchedTree]
    -- ^ Band node: original loop name, schedule dimensions, children
  | STSequence [SchedTree]
    -- ^ Sequence of children (textual order)
  | STDomain !StmtId
    -- ^ Leaf: a statement
  deriving (Show, Eq, Generic)

-- | Schedule context: maps dimension names to affine expressions.
-- Used in 'mapBand' callbacks.
newtype SchedCtx = SchedCtx (Map String AExpr)
  deriving (Show, Eq)


-- ---------------------------------------------------------------------------
-- Transforms
-- ---------------------------------------------------------------------------

-- | Schedule transformation. Named transforms are sugar over 'TMapBand'.
data Transform
  = TSkew !String !String !Integer      -- ^ @skew d0 d1 factor@
  | TTile !String !Integer              -- ^ @tile dim size@
  | TInterchange !String !String        -- ^ @interchange d0 d1@
  | TParallel !String                   -- ^ @parallel dim@ (verified)
  | TUnroll !String !Integer            -- ^ @unroll dim factor@
  | TShift !String !Integer             -- ^ @shift dim offset@
  | TFuse !String !String               -- ^ @fuse d0 d1@ (sibling bands)
  | TFiss !String !Int                  -- ^ @fiss dim at@ (split band)
  | TMapBand !String (SchedCtx -> [(String, AExpr)])
    -- ^ Generic: arbitrary quasi-affine map on a named band

instance Show Transform where
  show (TSkew d0 d1 f)   = "TSkew " ++ show d0 ++ " " ++ show d1 ++ " " ++ show f
  show (TTile d s)        = "TTile " ++ show d ++ " " ++ show s
  show (TInterchange a b) = "TInterchange " ++ show a ++ " " ++ show b
  show (TParallel d)      = "TParallel " ++ show d
  show (TUnroll d f)      = "TUnroll " ++ show d ++ " " ++ show f
  show (TShift d o)       = "TShift " ++ show d ++ " " ++ show o
  show (TFuse a b)        = "TFuse " ++ show a ++ " " ++ show b
  show (TFiss d n)        = "TFiss " ++ show d ++ " " ++ show n
  show (TMapBand d _)     = "TMapBand " ++ show d ++ " <fn>"

-- | Structural equality (TMapBand functions are never equal).
instance Eq Transform where
  TSkew a b c == TSkew x y z = a == x && b == y && c == z
  TTile a b == TTile x y = a == x && b == y
  TInterchange a b == TInterchange x y = a == x && b == y
  TParallel a == TParallel x = a == x
  TUnroll a b == TUnroll x y = a == x && b == y
  TShift a b == TShift x y = a == x && b == y
  TFuse a b == TFuse x y = a == x && b == y
  TFiss a b == TFiss x y = a == x && b == y
  TMapBand _ _ == TMapBand _ _ = False  -- functions can't be compared
  _ == _ = False


-- ---------------------------------------------------------------------------
-- Memory mappings
-- ---------------------------------------------------------------------------

-- | Memory mapping function: @(iteration context, logical subscripts) -> physical subscripts@.
type MemMapDef = SchedCtx -> [AExpr] -> [AExpr]


-- ---------------------------------------------------------------------------
-- Program
-- ---------------------------------------------------------------------------

-- | A polyhedral program, indexed by its parameter names.
data Program (ps :: [Symbol]) = Program
  { pLoops      :: [LoopNode]           -- ^ The sequential specification
  , pArrays     :: Map String ArrayDecl -- ^ Array declarations
  , pTransforms :: [Transform]          -- ^ Schedule transforms (from rescheduled)
  , pMemMaps    :: Map String MemMapDef -- ^ Per-array memory mappings
  , pParDims    :: Set String           -- ^ Dimensions declared parallel
  }


-- ---------------------------------------------------------------------------
-- Extracted statements (shared between Compile and Verify)
-- ---------------------------------------------------------------------------

-- | A single statement extracted from the loop nest, with its domain
-- and initial schedule.
data ExtractedStmt = ExtractedStmt
  { esStmtId    :: !StmtId
  , esName      :: !String           -- ^ auto-generated name "S0", "S1", ...
  , esArrayName :: !String           -- ^ array being written
  , esSubs      :: [AExpr]           -- ^ write subscripts
  , esExpr      :: ValExpr           -- ^ RHS expression
  , esDimNames  :: [String]          -- ^ iteration variable names (outer to inner)
  , esParamNames :: [String]         -- ^ parameter names
  , esDomain    :: Conjunction SetIx -- ^ domain constraints from loop bounds
  , esSchedule  :: [(String, Int)]   -- ^ initial schedule: [(dimName, nestingDepth)]
  , esSeqIdx    :: [Int]             -- ^ textual ordering at each nesting level
  } deriving (Show)
