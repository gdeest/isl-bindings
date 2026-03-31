-- | GGUF file format types (version 3).
--
-- GGUF is a self-contained format for LLM weights, tokenizer, and config.
-- Layout: header → metadata KV → tensor infos → alignment → tensor data.
module Isl.Infer.GGUF.Types
  ( GGUFFile(..)
  , TensorInfo(..)
  , GGUFValue(..)
  , GGMLType(..)
  , fromGGMLTypeId
  , ggmlTypeSize
  , ggmlBlockSize
  , ggufMagic
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr)

-- | Magic number: "GGUF" in little-endian.
-- | Magic: bytes 'G','G','U','F' = 0x46554747 little-endian
ggufMagic :: Word32
ggufMagic = 0x46554747

-- | A loaded GGUF file.
data GGUFFile = GGUFFile
  { ggufVersion     :: !Word32
  , ggufTensorCount :: !Word64
  , ggufMetadata    :: !(Map String GGUFValue)
  , ggufTensors     :: !(Map String TensorInfo)
  , ggufBasePtr     :: !(ForeignPtr Word8)
  , ggufDataOffset  :: !Int
  }

instance Show GGUFFile where
  show g = "GGUFFile { version=" ++ show (ggufVersion g)
        ++ ", tensors=" ++ show (ggufTensorCount g)
        ++ ", metadata=" ++ show (length (ggufMetadata g)) ++ " keys"
        ++ ", dataOffset=" ++ show (ggufDataOffset g) ++ " }"

-- | Tensor descriptor (from header, not data).
data TensorInfo = TensorInfo
  { tiName   :: !String
  , tiDims   :: ![Word64]
  , tiType   :: !GGMLType
  , tiOffset :: !Word64
  } deriving (Show)

-- | GGUF metadata values.
data GGUFValue
  = GVUInt8    !Word8
  | GVInt8     !Int8
  | GVUInt16   !Word16
  | GVInt16    !Int16
  | GVUInt32   !Word32
  | GVInt32    !Int32
  | GVUInt64   !Word64
  | GVInt64    !Int64
  | GVFloat32  !Float
  | GVFloat64  !Double
  | GVBool     !Bool
  | GVString   !String
  | GVArray    ![GGUFValue]
  deriving (Show)

-- | GGML quantization types.
data GGMLType
  = GGML_F32
  | GGML_F16
  | GGML_Q4_0
  | GGML_Q4_1
  | GGML_Q5_0
  | GGML_Q5_1
  | GGML_Q8_0
  | GGML_Q8_1
  | GGML_Q2_K
  | GGML_Q3_K
  | GGML_Q4_K
  | GGML_Q5_K
  | GGML_Q6_K
  | GGML_Q8_K
  | GGML_OTHER !Word32
  deriving (Show, Eq)

fromGGMLTypeId :: Word32 -> GGMLType
fromGGMLTypeId 0  = GGML_F32
fromGGMLTypeId 1  = GGML_F16
fromGGMLTypeId 2  = GGML_Q4_0
fromGGMLTypeId 3  = GGML_Q4_1
fromGGMLTypeId 6  = GGML_Q5_0
fromGGMLTypeId 7  = GGML_Q5_1
fromGGMLTypeId 8  = GGML_Q8_0
fromGGMLTypeId 9  = GGML_Q8_1
fromGGMLTypeId 10 = GGML_Q2_K
fromGGMLTypeId 11 = GGML_Q3_K
fromGGMLTypeId 12 = GGML_Q4_K
fromGGMLTypeId 13 = GGML_Q5_K
fromGGMLTypeId 14 = GGML_Q6_K
fromGGMLTypeId 15 = GGML_Q8_K
fromGGMLTypeId n  = GGML_OTHER n

-- | Bytes per block for quantized types, bytes per element for F32/F16.
ggmlTypeSize :: GGMLType -> Int
ggmlTypeSize GGML_F32  = 4
ggmlTypeSize GGML_F16  = 2
ggmlTypeSize GGML_Q4_0 = 18
ggmlTypeSize GGML_Q4_1 = 20
ggmlTypeSize GGML_Q5_0 = 22
ggmlTypeSize GGML_Q5_1 = 24
ggmlTypeSize GGML_Q8_0 = 34  -- 2 (f16 delta) + 32 (32 × int8)
ggmlTypeSize GGML_Q8_1 = 40
ggmlTypeSize _         = 0

-- | Elements per quantization block.
ggmlBlockSize :: GGMLType -> Int
ggmlBlockSize GGML_F32  = 1
ggmlBlockSize GGML_F16  = 1
ggmlBlockSize GGML_Q4_0 = 32
ggmlBlockSize GGML_Q4_1 = 32
ggmlBlockSize GGML_Q5_0 = 32
ggmlBlockSize GGML_Q5_1 = 32
ggmlBlockSize GGML_Q8_0 = 32
ggmlBlockSize GGML_Q8_1 = 32
ggmlBlockSize _         = 1
