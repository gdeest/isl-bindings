{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Scalar type abstraction for Alpha kernels.
module Alpha.Scalar
  ( -- * C type tag
    CNumType(..)
  , cTypeName
  , cMathSuffix
    -- * Haskell interpretation
  , HsInterp(..)
  , numInterp
    -- * Const bridge
  , ConstBridge(..)
    -- * Marshalling
  , Marshal(..)
  , storableMarshal
    -- * Existential bundle
  , ScalarDesc(..)
    -- * Typeclass
  , AlphaScalar(..)
  , scalarDesc
    -- * Dispatch functions
  , evalBinOp
  , evalUnaryOp
  , evalReduceOp
    -- * Heterogeneous buffers
  , SomeBuffer(..)
  , toSomeBuffer
  , fromSomeBuffer
  ) where

import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf, pokeByteOff, peekByteOff)
import qualified Data.Vector.Unboxed as V

import Alpha.Codegen.COp (BinOp(..), UnaryOp(..), ReduceOp(..))


-- ═══════════════════════════════════════════════════════════════════════
-- §1. C type tag
-- ═══════════════════════════════════════════════════════════════════════

data CNumType
  = CFloat64
  | CFloat32
  | CFloat16
  | CBFloat16
  | CInt8  | CInt16  | CInt32  | CInt64
  | CUInt8 | CUInt16 | CUInt32 | CUInt64
  | CCustom !String !String  -- type name, math suffix
  deriving (Show, Eq)

cTypeName :: CNumType -> String
cTypeName CFloat64 = "double"
cTypeName CFloat32 = "float"
cTypeName CFloat16 = "_Float16"
cTypeName CBFloat16 = "__bf16"
cTypeName CInt8    = "int8_t"
cTypeName CInt16   = "int16_t"
cTypeName CInt32   = "int32_t"
cTypeName CInt64   = "int64_t"
cTypeName CUInt8   = "uint8_t"
cTypeName CUInt16  = "uint16_t"
cTypeName CUInt32  = "uint32_t"
cTypeName CUInt64  = "uint64_t"
cTypeName (CCustom name _) = name

cMathSuffix :: CNumType -> String
cMathSuffix CFloat64 = ""
cMathSuffix CFloat32 = "f"
cMathSuffix CFloat16 = "f16"
cMathSuffix CBFloat16 = "bf16"
cMathSuffix (CCustom _ sfx) = sfx
cMathSuffix _ = ""  -- integer types: no math suffix


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Haskell interpretation
-- ═══════════════════════════════════════════════════════════════════════

data HsInterp a = HsInterp
  { hiAdd, hiSub, hiMul, hiDiv :: a -> a -> a
  , hiNeg, hiAbs               :: a -> a
  , hiFloor, hiCeil, hiSqrt    :: a -> a
  , hiMin, hiMax               :: a -> a -> a
  , hiFromInteger              :: Integer -> a
  , hiCompare                  :: a -> a -> Ordering
  }

numInterp :: forall a. (Num a, Ord a, RealFrac a, Floating a) => HsInterp a
numInterp = HsInterp
  { hiAdd = (+), hiSub = (-), hiMul = (*), hiDiv = (/)
  , hiNeg = negate, hiAbs = abs
  , hiFloor = fromIntegral . (floor :: a -> Integer)
  , hiCeil  = fromIntegral . (ceiling :: a -> Integer)
  , hiSqrt  = sqrt
  , hiMin = min, hiMax = max
  , hiFromInteger = fromInteger
  , hiCompare = compare
  }


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Const bridge
-- ═══════════════════════════════════════════════════════════════════════

newtype ConstBridge a = ConstBridge { showLiteral :: a -> String }


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Marshalling
-- ═══════════════════════════════════════════════════════════════════════

data Marshal a = Marshal
  { maElemSize :: !Int
  , maPoke     :: Ptr Word8 -> Int -> a -> IO ()
  , maPeek     :: Ptr Word8 -> Int -> IO a
  }

storableMarshal :: forall a. Storable a => Marshal a
storableMarshal =
  let sz = sizeOf (undefined :: a)
  in Marshal
    { maElemSize = sz
    , maPoke = \ptr i v -> pokeByteOff (castPtr ptr) (i * sz) v
    , maPeek = \ptr i   -> peekByteOff (castPtr ptr) (i * sz)
    }


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Existential bundle
-- ═══════════════════════════════════════════════════════════════════════

data ScalarDesc where
  MkScalarDesc :: forall a.
    { sdCNumType        :: !CNumType
    , sdHsInterp        :: !(Maybe (HsInterp a))
    , sdConstBridge     :: !(Maybe (ConstBridge a))
    , sdMarshal         :: !(Maybe (Marshal a))
    , sdReduceIdentity  :: !(Maybe (ReduceOp -> Maybe String))
      -- ^ C literal for each reduction op's identity element.  Used by
      -- 'Alpha.Codegen.assembleCSource' to init reduction accumulator
      -- buffers regardless of type — e.g. @"0"@ for integer 'ReduceSum',
      -- @"INT32_MIN"@ for integer 'ReduceMax'.  'Nothing' (either at the
      -- field or inner lookup) surfaces as 'MissingReduceIdentity'.
    } -> ScalarDesc


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Typeclass
-- ═══════════════════════════════════════════════════════════════════════

class AlphaScalar a where
  scalarInterp          :: HsInterp a
  scalarCNumType        :: CNumType
  scalarConstBridge     :: ConstBridge a
  scalarMarshal         :: Marshal a
  scalarReduceIdentity  :: ReduceOp -> Maybe String

scalarDesc :: forall a. AlphaScalar a => ScalarDesc
scalarDesc = MkScalarDesc
  (scalarCNumType @a)
  (Just (scalarInterp @a))
  (Just (scalarConstBridge @a))
  (Just (scalarMarshal @a))
  (Just (scalarReduceIdentity @a))


-- ═══════════════════════════════════════════════════════════════════════
-- §7. Instances
-- ═══════════════════════════════════════════════════════════════════════

instance AlphaScalar Double where
  scalarInterp = numInterp
  scalarCNumType = CFloat64
  scalarConstBridge = ConstBridge showDoubleLiteral
  scalarMarshal = storableMarshal
  scalarReduceIdentity op = Just $ case op of
    ReduceSum  -> "0.0"
    ReduceProd -> "1.0"
    ReduceMin  -> "(1.0/0.0)"   -- +INFINITY
    ReduceMax  -> "(-1.0/0.0)"  -- -INFINITY

instance AlphaScalar Float where
  scalarInterp = numInterp
  scalarCNumType = CFloat32
  scalarConstBridge = ConstBridge showFloatLiteral
  scalarMarshal = storableMarshal
  scalarReduceIdentity op = Just $ case op of
    ReduceSum  -> "0.0f"
    ReduceProd -> "1.0f"
    ReduceMin  -> "(1.0f/0.0f)"
    ReduceMax  -> "(-1.0f/0.0f)"

-- | Integer 32-bit scalar.  Added to support integer reductions with
-- correct identity literals (e.g. @INT32_MIN@ for 'ReduceMax') — see #2.
-- Fractional ops (sqrt/div-as-truncation) error out; they are not
-- reachable from any integer-typed Alpha expression that survives
-- type-checking.
instance AlphaScalar Int32 where
  scalarInterp = HsInterp
    { hiAdd = (+), hiSub = (-), hiMul = (*)
    , hiDiv = quot
    , hiNeg = negate, hiAbs = abs
    , hiFloor = id, hiCeil = id
    , hiSqrt = \_ -> error "Alpha.Scalar: sqrt not defined for Int32"
    , hiMin = min, hiMax = max
    , hiFromInteger = fromInteger
    , hiCompare = compare
    }
  scalarCNumType = CInt32
  scalarConstBridge = ConstBridge show
  scalarMarshal = storableMarshal
  scalarReduceIdentity op = Just $ case op of
    ReduceSum  -> "0"
    ReduceProd -> "1"
    ReduceMin  -> "INT32_MAX"
    ReduceMax  -> "INT32_MIN"

showDoubleLiteral :: Double -> String
showDoubleLiteral d
  | d == fromIntegral (round d :: Integer) = show (round d :: Integer) ++ ".0"
  | otherwise = show d

showFloatLiteral :: Float -> String
showFloatLiteral f
  | f == fromIntegral (round f :: Integer) = show (round f :: Integer) ++ ".0f"
  | otherwise = show f ++ "f"


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Dispatch functions
-- ═══════════════════════════════════════════════════════════════════════

evalBinOp :: HsInterp a -> BinOp -> a -> a -> a
evalBinOp hi OpAdd = hiAdd hi
evalBinOp hi OpSub = hiSub hi
evalBinOp hi OpMul = hiMul hi
evalBinOp hi OpDiv = hiDiv hi
evalBinOp hi OpMin = hiMin hi
evalBinOp hi OpMax = hiMax hi

evalUnaryOp :: HsInterp a -> UnaryOp -> a -> a
evalUnaryOp hi OpNeg   = hiNeg hi
evalUnaryOp hi OpAbs   = hiAbs hi
evalUnaryOp hi OpFloor = hiFloor hi
evalUnaryOp hi OpCeil  = hiCeil hi
evalUnaryOp hi OpSqrt  = hiSqrt hi

evalReduceOp :: HsInterp a -> ReduceOp -> a -> a -> a
evalReduceOp hi ReduceSum  = hiAdd hi
evalReduceOp hi ReduceProd = hiMul hi
evalReduceOp hi ReduceMin  = hiMin hi
evalReduceOp hi ReduceMax  = hiMax hi


-- ═══════════════════════════════════════════════════════════════════════
-- §9. Heterogeneous buffers
-- ═══════════════════════════════════════════════════════════════════════

data SomeBuffer = SomeBuffer
  { sbDesc      :: !ScalarDesc
  , sbData      :: !(ForeignPtr Word8)
  , sbElemCount :: !Int
  }

toSomeBuffer :: forall a. (AlphaScalar a, V.Unbox a) => V.Vector a -> IO SomeBuffer
toSomeBuffer vec = do
  let m   = scalarMarshal @a
      n   = V.length vec
      sz  = maElemSize m
  fptr <- mallocForeignPtrBytes (n * sz)
  withForeignPtr fptr $ \ptr ->
    V.iforM_ vec $ \i v -> maPoke m ptr i v
  pure SomeBuffer
    { sbDesc      = scalarDesc @a
    , sbData      = fptr
    , sbElemCount = n
    }

fromSomeBuffer :: forall a. (AlphaScalar a, V.Unbox a) => SomeBuffer -> IO (V.Vector a)
fromSomeBuffer sb = do
  let m = scalarMarshal @a
      n = sbElemCount sb
  withForeignPtr (sbData sb) $ \ptr ->
    V.generateM n $ \i -> maPeek m ptr i
