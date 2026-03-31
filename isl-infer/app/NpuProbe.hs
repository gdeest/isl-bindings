{-# LANGUAGE ForeignFunctionInterface #-}

-- | Probe the AMD XDNA2 NPU: query metadata, firmware version, clock info.
module Main where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes, free, allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, minusPtr)
import Foreign.Storable
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode(..))
import System.Posix.Types (Fd(..))
import Text.Printf (printf)

-- DRM ioctl numbers for amdxdna
-- From amdxdna_accel.h:
-- DRM_COMMAND_BASE = 0x40
-- DRM_AMDXDNA_GET_INFO = 7
-- DRM_IOCTL_AMDXDNA_GET_INFO = DRM_IOWR(0x40 + 7, struct amdxdna_drm_get_info)

-- ioctl(2)
foreign import ccall unsafe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr () -> IO CInt

-- The ioctl number for DRM_IOCTL_AMDXDNA_GET_INFO
-- DRM_IOWR(0x47, struct amdxdna_drm_get_info) where struct is 16 bytes
-- _IOWR('d', 0x47, 16) = direction(3) | size(16) << 16 | type('d') << 8 | nr(0x47)
-- = 0xC0106447 -- but let's compute it properly

-- _IOC(dir, type, nr, size)
-- dir: _IOC_READ|_IOC_WRITE = 3
-- type: 'd' = 0x64 (DRM)
-- nr: DRM_COMMAND_BASE + DRM_AMDXDNA_GET_INFO = 0x40 + 7 = 0x47
-- size: sizeof(struct amdxdna_drm_get_info) = 4+4+8 = 16
--
-- _IOC(3, 0x64, 0x47, 16) = (3 << 30) | (16 << 16) | (0x64 << 8) | 0x47
--                          = 0xC0000000 | 0x00100000 | 0x00006400 | 0x00000047
--                          = 0xC0106447

ioctlGetInfo :: CULong
ioctlGetInfo = 0xC0106447

-- enum amdxdna_drm_get_param values
queryAieMetadata :: Word32
queryAieMetadata = 1  -- DRM_AMDXDNA_QUERY_AIE_METADATA

queryAieVersion :: Word32
queryAieVersion = 2  -- DRM_AMDXDNA_QUERY_AIE_VERSION

queryClockMetadata :: Word32
queryClockMetadata = 3  -- DRM_AMDXDNA_QUERY_CLOCK_METADATA

queryFirmwareVersion :: Word32
queryFirmwareVersion = 8 -- DRM_AMDXDNA_QUERY_FIRMWARE_VERSION

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "=== AMD XDNA2 NPU Probe ==="
  putStrLn ""

  -- Open the NPU device
  putStrLn "Opening /dev/accel/accel0..."
  fd <- openFd "/dev/accel/accel0" ReadWrite defaultFileFlags
  let cfd = fromIntegral fd :: CInt
  putStrLn $ "  fd = " ++ show fd

  -- Query firmware version (16 bytes: 4x u32)
  putStrLn ""
  putStrLn "--- Firmware Version ---"
  allocaBytes 256 $ \buf -> do
    let getInfo = allocaBytes 16 $ \gi -> do
          pokeByteOff gi 0 (queryFirmwareVersion :: Word32) -- param
          pokeByteOff gi 4 (16 :: Word32)                    -- buffer_size
          pokeByteOff gi 8 (fromIntegral (ptrToWord buf) :: Word64) -- buffer ptr
          ret <- c_ioctl cfd ioctlGetInfo (castPtr gi)
          return ret
    ret <- getInfo
    if ret == 0
      then do
        major <- peekByteOff buf 0 :: IO Word32
        minor <- peekByteOff buf 4 :: IO Word32
        patch <- peekByteOff buf 8 :: IO Word32
        build <- peekByteOff buf 12 :: IO Word32
        printf "  Version: %d.%d.%d (build %d)\n" major minor patch build
      else putStrLn $ "  ioctl failed: " ++ show ret

  -- Query AIE metadata
  putStrLn ""
  putStrLn "--- AIE Metadata ---"
  allocaBytes 256 $ \buf -> do
    allocaBytes 16 $ \gi -> do
      pokeByteOff gi 0 (queryAieMetadata :: Word32)
      pokeByteOff gi 4 (256 :: Word32)
      pokeByteOff gi 8 (fromIntegral (ptrToWord buf) :: Word64)
      ret <- c_ioctl cfd ioctlGetInfo (castPtr gi)
      if ret == 0
        then do
          colSize <- peekByteOff buf 0 :: IO Word32
          cols    <- peekByteOff buf 4 :: IO Word16
          rows    <- peekByteOff buf 6 :: IO Word16
          verMaj  <- peekByteOff buf 8 :: IO Word32
          verMin  <- peekByteOff buf 12 :: IO Word32
          -- core tile metadata at offset 16
          coreRows  <- peekByteOff buf 16 :: IO Word16
          coreStart <- peekByteOff buf 18 :: IO Word16
          coreDma   <- peekByteOff buf 20 :: IO Word16
          coreLocks <- peekByteOff buf 22 :: IO Word16
          coreEvts  <- peekByteOff buf 24 :: IO Word16
          -- mem tile metadata at offset 28
          memRows   <- peekByteOff buf 28 :: IO Word16
          memStart  <- peekByteOff buf 30 :: IO Word16
          -- shim tile metadata at offset 40
          shimRows  <- peekByteOff buf 40 :: IO Word16
          shimStart <- peekByteOff buf 42 :: IO Word16

          printf "  Column size: %d bytes\n" colSize
          printf "  Columns: %d\n" cols
          printf "  Rows: %d\n" rows
          printf "  AIE version: %d.%d\n" verMaj verMin
          printf "  Core tiles: %d rows (start %d), %d DMA ch, %d locks, %d events\n"
            coreRows coreStart coreDma coreLocks coreEvts
          printf "  Mem tiles: %d rows (start %d)\n" memRows memStart
          printf "  Shim tiles: %d rows (start %d)\n" shimRows shimStart

          let totalCores = fromIntegral cols * fromIntegral coreRows :: Int
          printf "  Total AIE cores: %d × %d = %d\n" cols coreRows totalCores
        else putStrLn $ "  ioctl failed: " ++ show ret

  -- Query clock metadata
  putStrLn ""
  putStrLn "--- Clock ---"
  allocaBytes 256 $ \buf -> do
    allocaBytes 16 $ \gi -> do
      pokeByteOff gi 0 (queryClockMetadata :: Word32)
      pokeByteOff gi 4 (256 :: Word32)
      pokeByteOff gi 8 (fromIntegral (ptrToWord buf) :: Word64)
      ret <- c_ioctl cfd ioctlGetInfo (castPtr gi)
      if ret == 0
        then do
          -- First clock: name[16] + freq_mhz[4] + pad[4] = 24 bytes
          name1 <- peekArray 16 (castPtr buf :: Ptr Word8)
          freq1 <- peekByteOff buf 16 :: IO Word32
          name2 <- peekArray 16 (castPtr (buf `plusPtr` 24) :: Ptr Word8)
          freq2 <- peekByteOff buf 40 :: IO Word32
          let showName = map (toEnum . fromIntegral) . takeWhile (/= 0)
          printf "  %s: %d MHz\n" (showName name1 :: String) freq1
          printf "  %s: %d MHz\n" (showName name2 :: String) freq2
        else putStrLn $ "  ioctl failed: " ++ show ret

  closeFd fd
  putStrLn "\nDone."

-- Helper to convert Ptr to Word64 for the ioctl buffer pointer
ptrToWord :: Ptr a -> Word64
ptrToWord p = fromIntegral (p `minusPtr` nullPtr)
