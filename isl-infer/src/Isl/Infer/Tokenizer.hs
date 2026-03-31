-- | BPE tokenizer extracted from GGUF metadata.
--
-- Supports the Llama/Mistral BPE tokenizer format with
-- byte-fallback for unknown characters.
module Isl.Infer.Tokenizer
  ( Tokenizer(..)
  , loadTokenizer
  , encode
  , decode
  , decodeSingle
  ) where

import Data.Bits ((.&.), shiftL, (.|.))
import Data.Char (ord, chr)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import Data.Vector (Vector)

import Isl.Infer.GGUF

-- | BPE tokenizer.
data Tokenizer = Tokenizer
  { tokVocab   :: !(Vector String)       -- ^ id → string
  , tokLookup  :: !(Map String Int)      -- ^ string → id
  , tokScores  :: !(Vector Float)        -- ^ merge priorities
  , tokBOS     :: !Int                    -- ^ beginning of sequence
  , tokEOS     :: !Int                    -- ^ end of sequence
  } deriving (Show)

-- | Extract tokenizer from GGUF metadata.
loadTokenizer :: GGUFFile -> Tokenizer
loadTokenizer gf =
  let tokens = case lookupMetaStringArray gf "tokenizer.ggml.tokens" of
        Just ts -> ts
        Nothing -> error "loadTokenizer: no tokenizer.ggml.tokens"
      scores = case lookupMetaF32Array gf "tokenizer.ggml.scores" of
        Just ss -> ss
        Nothing -> replicate (length tokens) 0.0
      bos = maybe 1 fromIntegral (lookupMetaU32 gf "tokenizer.ggml.bos_token_id")
      eos = maybe 2 fromIntegral (lookupMetaU32 gf "tokenizer.ggml.eos_token_id")
      vocab = V.fromList tokens
      lut   = Map.fromList (zip tokens [0..])
  in Tokenizer
    { tokVocab  = vocab
    , tokLookup = lut
    , tokScores = V.fromList scores
    , tokBOS    = bos
    , tokEOS    = eos
    }

-- | Encode a string to token ids using BPE.
-- Prepends BOS token.
encode :: Tokenizer -> String -> [Int]
encode tok text =
  let -- Convert to bytes, look up byte-level tokens, then BPE merge.
      -- This works because the vocab stores raw UTF-8 byte sequences,
      -- and BPE merges them into the proper multi-byte tokens.
      initial = concatMap (charToTokens tok) text
      merged  = bpeMerge tok initial
  in tokBOS tok : merged

-- | Encode without SentencePiece preprocessing (raw bytes).
-- Used as fallback when SentencePiece encoding produces bad results.
encodeRaw :: Tokenizer -> String -> [Int]
encodeRaw tok text =
  let initial = concatMap (charToTokensRaw tok) text
      merged  = bpeMerge tok initial
  in tokBOS tok : merged

charToTokensRaw :: Tokenizer -> Char -> [Int]
charToTokensRaw tok c =
  let bytes = encodeUTF8Char c
  in map (byteFallback tok) bytes

-- | Convert a character to initial tokens.
-- If the character is in the vocab, use it directly.
-- Otherwise, use byte-level fallback tokens <0xHH>.
charToTokens :: Tokenizer -> Char -> [Int]
charToTokens tok c =
  case Map.lookup [c] (tokLookup tok) of
    Just tid -> [tid]
    Nothing  ->
      -- Try UTF-8 byte fallback
      let bytes = encodeUTF8Char c
      in map (byteFallback tok) bytes

byteFallback :: Tokenizer -> Int -> Int
byteFallback tok b =
  let hex = "<0x" ++ hexByte b ++ ">"
  in case Map.lookup hex (tokLookup tok) of
    Just tid -> tid
    Nothing  -> 0  -- unknown token

hexByte :: Int -> String
hexByte n =
  let hi = n `div` 16
      lo = n `mod` 16
      hexDigit d | d < 10    = chr (d + ord '0')
                 | otherwise = chr (d - 10 + ord 'A')
  in [hexDigit hi, hexDigit lo]

encodeUTF8Char :: Char -> [Int]
encodeUTF8Char c
  | n < 0x80    = [n]
  | n < 0x800   = [0xC0 + n `div` 64, 0x80 + n `mod` 64]
  | n < 0x10000 = [0xE0 + n `div` 4096, 0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]
  | otherwise   = [0xF0 + n `div` 262144, 0x80 + (n `div` 4096) `mod` 64,
                   0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]
  where n = ord c

-- | BPE merge: repeatedly merge the highest-scoring adjacent pair.
bpeMerge :: Tokenizer -> [Int] -> [Int]
bpeMerge _   []  = []
bpeMerge _   [x] = [x]
bpeMerge tok tokens = go tokens
  where
    go toks
      | length toks < 2 = toks
      | otherwise =
          case findBestMerge tok toks of
            Nothing          -> toks
            Just (idx, merged) -> go (applyMerge idx merged toks)

-- | Find the best merge: the adjacent pair with highest score.
findBestMerge :: Tokenizer -> [Int] -> Maybe (Int, Int)
findBestMerge tok toks =
  let pairs = zip [0..] (zip toks (tail toks))
      candidates = [(i, tid, sc) | (i, (a, b)) <- pairs,
                    let merged = V.unsafeIndex (tokVocab tok) a ++ V.unsafeIndex (tokVocab tok) b,
                    Just tid <- [Map.lookup merged (tokLookup tok)],
                    let sc = V.unsafeIndex (tokScores tok) tid]
  in case candidates of
    [] -> Nothing
    _  -> let (i, tid, _) = foldl' (\best@(_, _, bs) cur@(_, _, cs) ->
                               if cs > bs then cur else best) (head candidates) (tail candidates)
           in Just (i, tid)

-- | Apply a merge at position idx.
applyMerge :: Int -> Int -> [Int] -> [Int]
applyMerge idx merged toks =
  take idx toks ++ [merged] ++ drop (idx + 2) toks

-- | Decode a single token id to a string.
decodeSingle :: Tokenizer -> Int -> String
decodeSingle tok tid
  | tid >= 0 && tid < V.length (tokVocab tok) =
      let s = V.unsafeIndex (tokVocab tok) tid
      in decodeTokenStr s
  | otherwise = ""

-- | Decode a list of token ids to a string.
decode :: Tokenizer -> [Int] -> String
decode tok = concatMap (decodeSingle tok)

-- | Handle special token strings like byte fallbacks,
-- then reassemble multi-byte UTF-8 and replace SentencePiece markers.
decodeTokenStr :: String -> String
decodeTokenStr ('<':'0':'x':rest) =
  case span (/= '>') rest of
    (hex, ">") -> case readHex hex of
      Just b  -> [chr b]
      Nothing -> "<0x" ++ rest
    _ -> "<0x" ++ rest
decodeTokenStr s = replaceSentinels (reassembleUTF8 s)

-- | Reassemble UTF-8 from raw byte chars.
-- Token strings from GGUF are stored as individual bytes (each char < 256).
-- Decode multi-byte UTF-8 sequences back into proper Unicode chars.
reassembleUTF8 :: String -> String
reassembleUTF8 [] = []
reassembleUTF8 (c:cs)
  | ord c < 0x80  = c : reassembleUTF8 cs
  | ord c < 0xC0  = c : reassembleUTF8 cs  -- invalid start, pass through
  | ord c < 0xE0  = case cs of
      (c1:rest) | ord c1 >= 0x80 && ord c1 < 0xC0 ->
        chr ((ord c .&. 0x1F) `shiftL` 6 .|. (ord c1 .&. 0x3F)) : reassembleUTF8 rest
      _ -> c : reassembleUTF8 cs
  | ord c < 0xF0  = case cs of
      (c1:c2:rest) | ord c1 >= 0x80 && ord c2 >= 0x80 ->
        chr ((ord c .&. 0x0F) `shiftL` 12 .|. (ord c1 .&. 0x3F) `shiftL` 6
             .|. (ord c2 .&. 0x3F)) : reassembleUTF8 rest
      _ -> c : reassembleUTF8 cs
  | otherwise = case cs of
      (c1:c2:c3:rest) | ord c1 >= 0x80 && ord c2 >= 0x80 && ord c3 >= 0x80 ->
        chr ((ord c .&. 0x07) `shiftL` 18 .|. (ord c1 .&. 0x3F) `shiftL` 12
             .|. (ord c2 .&. 0x3F) `shiftL` 6 .|. (ord c3 .&. 0x3F)) : reassembleUTF8 rest
      _ -> c : reassembleUTF8 cs

-- | Replace SentencePiece sentinel characters.
-- '▁' (U+2581) stored as raw bytes 0xE2 0x96 0x81 → ' '
-- Also handle single-char U+2581 if vocab was UTF-8 decoded.
replaceSentinels :: String -> String
replaceSentinels [] = []
-- Raw UTF-8 bytes for ▁: 0xE2=226, 0x96=150, 0x81=129
replaceSentinels ('\xe2':'\x96':'\x81':rest) = ' ' : replaceSentinels rest
-- Single char (if vocab was UTF-8 decoded)
replaceSentinels ('\x2581':rest) = ' ' : replaceSentinels rest
replaceSentinels (c:rest) = c : replaceSentinels rest

readHex :: String -> Maybe Int
readHex = go 0
  where
    go n [] = Just n
    go n (c:cs)
      | c >= '0' && c <= '9' = go (n * 16 + ord c - ord '0') cs
      | c >= 'a' && c <= 'f' = go (n * 16 + ord c - ord 'a' + 10) cs
      | c >= 'A' && c <= 'F' = go (n * 16 + ord c - ord 'A' + 10) cs
      | otherwise = Nothing
