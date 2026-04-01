{-# LANGUAGE ScopedTypeVariables #-}

-- | GGUF file parser.
--
-- Mmaps the file, parses the header (metadata + tensor descriptors),
-- and provides access to tensor data via raw pointers into the mmap'd region.
module Isl.Infer.GGUF
  ( loadGGUF
  , tensorByteSize
  , withTensorData
  , lookupMetaString
  , lookupMetaU32
  , lookupMetaI32
  , lookupMetaF32
  , lookupMetaU64
  , lookupMetaStringArray
  , lookupMetaF32Array
  , module Isl.Infer.GGUF.Types
  ) where

import Control.Monad (when, replicateM)
import Data.Bits ((.&.), shiftL, (.|.))
import Data.Char (chr)
import Data.IORef
import Data.Int (Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CSize(..), CInt(..))
import System.Posix.Types (COff(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr, nullPtr)
import Foreign.Storable (Storable, peekByteOff)
import System.IO (withBinaryFile, IOMode(..), hFileSize)
import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode(..))

import Isl.Infer.GGUF.Types

foreign import ccall unsafe "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr ())

foreign import ccall unsafe "sys/mman.h madvise"
  c_madvise :: Ptr () -> CSize -> CInt -> IO CInt

-- | Load and parse a GGUF file.
--
-- Uses MAP_POPULATE to prefault all pages at load time (no demand-paging
-- faults during first token) and MADV_HUGEPAGE to request 2MB pages
-- (reduces TLB entries from ~1.8M to ~3750 for a 7B model).
loadGGUF :: FilePath -> IO GGUFFile
loadGGUF path = do
  size <- withBinaryFile path ReadMode hFileSize
  fd <- openFd path ReadOnly defaultFileFlags
  let mapPopulate = 0x008000 :: CInt
  basePtr <- c_mmap nullPtr (fromIntegral size) 1 (2 .|. mapPopulate) (fromIntegral fd) 0
  closeFd fd
  when (basePtr == nullPtr || basePtr == plusPtr nullPtr (-1)) $
    error $ "loadGGUF: mmap failed for " ++ path
  -- Request transparent huge pages (2MB) to reduce TLB pressure
  let madvHugepage = 14 :: CInt
  _ <- c_madvise basePtr (fromIntegral size) madvHugepage
  fptr <- newForeignPtr_ (castPtr basePtr :: Ptr Word8)
  parseGGUF basePtr fptr

parseGGUF :: Ptr () -> ForeignPtr Word8 -> IO GGUFFile
parseGGUF base fptr = do
  posRef <- newIORef (0 :: Int)

  let rd :: forall a. Storable a => Int -> IO a
      rd n = do
        pos <- readIORef posRef
        val <- peekByteOff base pos
        writeIORef posRef (pos + n)
        return val

      readU8  = rd 1 :: IO Word8
      readU32 = rd 4 :: IO Word32
      readU64 = rd 8 :: IO Word64
      readF32 = rd 4 :: IO Float
      readF64 = rd 8 :: IO Double

      readString = do
        len <- readU64
        pos <- readIORef posRef
        bytes <- mapM (\i -> peekByteOff base (pos + i) :: IO Word8)
                      [0 .. fromIntegral len - 1]
        writeIORef posRef (pos + fromIntegral len)
        return (map (chr . fromIntegral) bytes)

      readValue :: Word32 -> IO GGUFValue
      readValue 0  = GVUInt8   <$> readU8
      readValue 1  = GVInt8    . fromIntegral <$> (readU8 :: IO Word8)
      readValue 2  = GVUInt16  <$> (rd 2 :: IO Word16)
      readValue 3  = GVInt16   <$> (rd 2 :: IO Int16)
      readValue 4  = GVUInt32  <$> readU32
      readValue 5  = GVInt32   . fromIntegral <$> readU32
      readValue 6  = GVFloat32 <$> readF32
      readValue 7  = GVBool    . (/= 0) <$> readU8
      readValue 8  = GVString  <$> readString
      readValue 9  = do
        elemType <- readU32
        len <- readU64
        GVArray <$> replicateM (fromIntegral len) (readValue elemType)
      readValue 10 = GVUInt64  <$> readU64
      readValue 11 = GVInt64   . fromIntegral <$> readU64
      readValue 12 = GVFloat64 <$> readF64
      readValue t  = error $ "loadGGUF: unknown value type " ++ show t

  -- Header
  magic <- readU32
  when (magic /= ggufMagic) $
    error $ "loadGGUF: bad magic " ++ show magic
  version <- readU32
  when (version < 2 || version > 3) $
    error $ "loadGGUF: unsupported version " ++ show version
  tensorCount <- readU64
  metadataCount <- readU64

  -- Metadata KV pairs
  metadata <- fmap Map.fromList $
    replicateM (fromIntegral metadataCount) $ do
      key <- readString
      valType <- readU32
      val <- readValue valType
      return (key, val)

  -- Tensor descriptors
  tensors <- fmap Map.fromList $
    replicateM (fromIntegral tensorCount) $ do
      name <- readString
      nDims <- readU32
      dims <- replicateM (fromIntegral nDims) readU64
      typeId <- readU32
      offset <- readU64
      return (name, TensorInfo name dims (fromGGMLTypeId typeId) offset)

  -- Data offset: aligned to general.alignment (default 32)
  let alignment = case Map.lookup "general.alignment" metadata of
        Just (GVUInt32 a) -> fromIntegral a
        _                 -> 32 :: Int
  pos <- readIORef posRef
  let dataOff = ((pos + alignment - 1) `div` alignment) * alignment

  return GGUFFile
    { ggufVersion     = version
    , ggufTensorCount = tensorCount
    , ggufMetadata    = metadata
    , ggufTensors     = tensors
    , ggufBasePtr     = fptr
    , ggufDataOffset  = dataOff
    }

-- | Access tensor data safely within a callback.
-- The Ptr is valid only within the callback.
withTensorData :: GGUFFile -> TensorInfo -> (Ptr Word8 -> IO a) -> IO a
withTensorData gf ti action =
  withForeignPtr (ggufBasePtr gf) $ \basePtr ->
    action (basePtr `plusPtr` (ggufDataOffset gf + fromIntegral (tiOffset ti)))

-- | Tensor size in bytes.
tensorByteSize :: TensorInfo -> Int
tensorByteSize ti =
  let elems = product (map fromIntegral (tiDims ti))
      bs = ggmlBlockSize (tiType ti)
      ts = ggmlTypeSize (tiType ti)
  in (elems `div` bs) * ts

-- | Decode a list of bytes as UTF-8 into a Haskell String.
decodeUTF8 :: [Word8] -> String
decodeUTF8 [] = []
decodeUTF8 (b:bs)
  | b < 0x80  = chr (fromIntegral b) : decodeUTF8 bs
  | b < 0xC0  = chr 0xFFFD : decodeUTF8 bs  -- invalid continuation byte
  | b < 0xE0  = case bs of
      (b1:rest) -> chr (fromIntegral (b .&. 0x1F) `shiftL` 6
                    .|. fromIntegral (b1 .&. 0x3F)) : decodeUTF8 rest
      _ -> [chr 0xFFFD]
  | b < 0xF0  = case bs of
      (b1:b2:rest) -> chr (fromIntegral (b .&. 0x0F) `shiftL` 12
                       .|. fromIntegral (b1 .&. 0x3F) `shiftL` 6
                       .|. fromIntegral (b2 .&. 0x3F)) : decodeUTF8 rest
      _ -> [chr 0xFFFD]
  | otherwise = case bs of
      (b1:b2:b3:rest) -> chr (fromIntegral (b .&. 0x07) `shiftL` 18
                          .|. fromIntegral (b1 .&. 0x3F) `shiftL` 12
                          .|. fromIntegral (b2 .&. 0x3F) `shiftL` 6
                          .|. fromIntegral (b3 .&. 0x3F)) : decodeUTF8 rest
      _ -> [chr 0xFFFD]

-- Metadata lookups

lookupMetaString :: GGUFFile -> String -> Maybe String
lookupMetaString gf key = case Map.lookup key (ggufMetadata gf) of
  Just (GVString s) -> Just s; _ -> Nothing

lookupMetaU32 :: GGUFFile -> String -> Maybe Word32
lookupMetaU32 gf key = case Map.lookup key (ggufMetadata gf) of
  Just (GVUInt32 n) -> Just n; _ -> Nothing

lookupMetaI32 :: GGUFFile -> String -> Maybe Int32
lookupMetaI32 gf key = case Map.lookup key (ggufMetadata gf) of
  Just (GVInt32 n) -> Just n; _ -> Nothing

lookupMetaF32 :: GGUFFile -> String -> Maybe Float
lookupMetaF32 gf key = case Map.lookup key (ggufMetadata gf) of
  Just (GVFloat32 f) -> Just f; _ -> Nothing

lookupMetaU64 :: GGUFFile -> String -> Maybe Word64
lookupMetaU64 gf key = case Map.lookup key (ggufMetadata gf) of
  Just (GVUInt64 n) -> Just n; _ -> Nothing

lookupMetaStringArray :: GGUFFile -> String -> Maybe [String]
lookupMetaStringArray gf key = case Map.lookup key (ggufMetadata gf) of
  Just (GVArray vs) -> Just [s | GVString s <- vs]; _ -> Nothing

lookupMetaF32Array :: GGUFFile -> String -> Maybe [Float]
lookupMetaF32Array gf key = case Map.lookup key (ggufMetadata gf) of
  Just (GVArray vs) -> Just [f | GVFloat32 f <- vs]; _ -> Nothing
