-- | Runtime Q8_0 → Q4_0 weight downquantization.
--
-- Converts FFN weight tensors from Q8_0 to Q4_0 at model load time,
-- allowing bandwidth reduction without requiring a separate model file.
-- Attention weights remain Q8_0 (mmap'd, zero-copy).
--
-- Cost: ~6 seconds for 7B model (5.6B FFN params × 32 layers).
-- After downquantization, the original Q8 pages can be released via
-- @madvise(MADV_DONTNEED)@.
module Isl.Infer.Downquant
  ( downquantFFN
  , downquantBlock
  ) where

import Data.Bits ((.&.), shiftR, shiftL, (.|.))
import Data.Int (Int8)
import Data.Word (Word8, Word16)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peek, peekByteOff, poke, pokeByteOff)

-- | Downquant a Q8_0 weight tensor to Q4_0 in a new malloc'd buffer.
--
-- Both formats use 32-element blocks. Q8_0 block = 34 bytes (2 + 32).
-- Q4_0 block = 18 bytes (2 + 16). So for nBlocks blocks:
--   input:  nBlocks × 34 bytes
--   output: nBlocks × 18 bytes
--
-- Returns a pointer to the Q4_0 buffer. Caller owns the memory.
downquantFFN :: Ptr Word8  -- ^ Source Q8_0 weight data (mmap'd)
             -> Int        -- ^ Number of Q8_0 blocks (N × KB)
             -> IO (Ptr Word8)
downquantFFN srcPtr nBlocks = do
  let outBytes = nBlocks * 18
  dstPtr <- mallocBytes outBytes
  mapM_ (\i -> downquantBlock (srcPtr `plusPtr` (i * 34)) (dstPtr `plusPtr` (i * 18)))
        [0 .. nBlocks - 1]
  return dstPtr

-- | Convert a single Q8_0 block (34 bytes) to Q4_0 (18 bytes).
--
-- Q8_0: { uint16_t d; int8_t qs[32]; }
-- Q4_0: { uint16_t d; uint8_t qs[16]; }
--
-- The scale factor is preserved (same f16 value, reinterpreted for 4-bit range).
-- Each pair of int8 values is mapped to a packed byte of two 4-bit nibbles:
--   nibble = clamp(round(q8_val * (7.0 / 127.0)) + 8, 0, 15)
--
-- The Q4 dequantization produces: scale_q4 × (nibble - 8) where
-- scale_q4 = original_scale × (127.0 / 7.0) to compensate for the range change.
downquantBlock :: Ptr Word8 -> Ptr Word8 -> IO ()
downquantBlock src dst = do
  -- Read the Q8 scale (f16, 2 bytes) and adjust for Q4 range
  scaleF16 <- peek (castPtr src :: Ptr Word16)
  let scaleF32 = f16ToF32 scaleF16
      -- Q8 dequant: value = scale * q8_val (range: scale × [-128, 127])
      -- Q4 dequant: value = scale' * (nibble - 8) (range: scale' × [-8, 7])
      -- To match: scale' = scale * 127/7 ≈ scale * 18.14
      newScale = scaleF32 * (127.0 / 7.0)
      newScaleF16 = f32ToF16 newScale
  poke (castPtr dst :: Ptr Word16) newScaleF16

  -- Convert 32 int8 values → 16 packed nibble bytes
  mapM_ (\i -> do
    let srcOff = 2 + 2 * i  -- skip 2-byte scale, pairs of int8s
    q0 <- peekByteOff src srcOff :: IO Int8
    q1 <- peekByteOff src (srcOff + 1) :: IO Int8
    -- Map from Q8 range [-128..127] to Q4 range [0..15], centered at 8
    let toNibble :: Int8 -> Word8
        toNibble q = let v = round (fromIntegral q * (7.0 / 127.0) :: Float) + 8 :: Int
                     in fromIntegral (max 0 (min 15 v))
        n0 = toNibble q0
        n1 = toNibble q1
        packed = n0 .|. (n1 `shiftL` 4)
    pokeByteOff dst (2 + i) packed
    ) [0 .. 15]

-- | IEEE 754 half-precision to single-precision conversion.
f16ToF32 :: Word16 -> Float
f16ToF32 h =
  let sign = fromIntegral ((h `shiftR` 15) .&. 1) * (-2) + 1 :: Float
      expn = fromIntegral ((h `shiftR` 10) .&. 0x1F) :: Int
      mant = fromIntegral (h .&. 0x3FF) :: Float
  in if expn == 0 then sign * (2 ** (-14)) * (mant / 1024)
     else if expn == 31 then if mant == 0 then sign * (1/0) else 0/0
     else sign * (2 ** fromIntegral (expn - 15)) * (1 + mant / 1024)

-- | Single-precision to half-precision conversion.
f32ToF16 :: Float -> Word16
f32ToF16 f
  | isNaN f = 0x7E00
  | isInfinite f = if f > 0 then 0x7C00 else 0xFC00
  | abs f < 2 ** (-24) = 0  -- underflow to zero
  | otherwise =
    let sign = if f < 0 then 0x8000 else 0 :: Word16
        af = abs f
        expn = floor (logBase 2 af) :: Int
        mant = af / (2 ** fromIntegral expn) - 1
        biased = expn + 15
    in if biased >= 31 then sign .|. 0x7C00  -- overflow to inf
       else if biased <= 0 then sign  -- underflow to zero
       else sign .|. (fromIntegral biased `shiftL` 10) .|. (round (mant * 1024) .&. 0x3FF)
