-- | Quick GGUF file inspector — prints metadata and tensor list.
module Main where

import Data.List (sortBy, isInfixOf)
import qualified Data.Vector as V
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Text.Printf (printf)

import Isl.Infer.GGUF
import Isl.Infer.Tokenizer (loadTokenizer, tokVocab)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    [path] -> inspect path
    _      -> putStrLn "Usage: gguf-info <model.gguf>"

inspect :: FilePath -> IO ()
inspect path = do
  putStrLn $ "Loading " ++ path ++ "..."
  gf <- loadGGUF path
  putStrLn $ "  Version: " ++ show (ggufVersion gf)
  putStrLn $ "  Tensors: " ++ show (ggufTensorCount gf)
  putStrLn $ "  Metadata: " ++ show (Map.size (ggufMetadata gf)) ++ " keys"
  putStrLn $ "  Data offset: " ++ show (ggufDataOffset gf)

  -- Architecture
  putStrLn ""
  putStrLn "--- Architecture ---"
  printMeta gf "general.architecture"
  printMeta gf "general.name"

  -- Model config
  let arch = case lookupMetaString gf "general.architecture" of
        Just a  -> a
        Nothing -> "llama"
  putStrLn ""
  putStrLn "--- Model config ---"
  mapM_ (printMeta gf)
    [ arch ++ ".context_length"
    , arch ++ ".embedding_length"
    , arch ++ ".feed_forward_length"
    , arch ++ ".block_count"
    , arch ++ ".attention.head_count"
    , arch ++ ".attention.head_count_kv"
    , arch ++ ".attention.layer_norm_rms_epsilon"
    , arch ++ ".rope.freq_base"
    , arch ++ ".rope.dimension_count"
    , arch ++ ".vocab_size"
    ]

  -- Tokenizer
  putStrLn ""
  putStrLn "--- Tokenizer ---"
  printMeta gf "tokenizer.ggml.model"
  case lookupMetaStringArray gf "tokenizer.ggml.tokens" of
    Just ts -> putStrLn $ "  tokenizer.ggml.tokens: " ++ show (length ts) ++ " tokens"
    Nothing -> putStrLn "  tokenizer.ggml.tokens: (not found)"
  printMeta gf "tokenizer.ggml.bos_token_id"
  printMeta gf "tokenizer.ggml.eos_token_id"

  -- Show first 10 vocab tokens + look for [INST]
  putStrLn ""
  putStrLn "--- Vocab (first 10) ---"
  let vocab = tokVocab (loadTokenizer gf)
  mapM_ (\i -> printf "  %d: %s\n" i (show (V.unsafeIndex vocab i))) [0..min 9 (V.length vocab - 1)]
  -- Search for INST tokens
  let instTokens = [(i, V.unsafeIndex vocab i) | i <- [0..V.length vocab-1],
                    let s = V.unsafeIndex vocab i, "[INST]" `isInfixOf` s || "[/INST]" `isInfixOf` s]
  putStrLn $ "  [INST]-like tokens: " ++ show instTokens

  -- Tensor summary
  putStrLn ""
  putStrLn "--- Tensors ---"
  let tensors = sortBy (comparing tiOffset) (Map.elems (ggufTensors gf))
      totalBytes = sum (map tensorByteSize tensors)
  printf "  %-40s %-10s %-20s %12s\n"
    ("Name" :: String) ("Type" :: String) ("Shape" :: String) ("Bytes" :: String)
  putStrLn $ "  " ++ replicate 82 '-'
  mapM_ (\ti -> do
    let shape = show (tiDims ti)
        bytes = tensorByteSize ti
    printf "  %-40s %-10s %-20s %12d\n"
      (tiName ti) (show (tiType ti)) shape bytes
    ) (take 20 tensors)  -- first 20
  when (length tensors > 20) $
    putStrLn $ "  ... (" ++ show (length tensors - 20) ++ " more)"
  putStrLn $ "  " ++ replicate 82 '-'
  printf "  Total: %d tensors, %.1f GB\n"
    (length tensors) (fromIntegral totalBytes / 1e9 :: Double)

printMeta :: GGUFFile -> String -> IO ()
printMeta gf key = case Map.lookup key (ggufMetadata gf) of
  Just v  -> putStrLn $ "  " ++ key ++ ": " ++ showVal v
  Nothing -> putStrLn $ "  " ++ key ++ ": (not found)"

showVal :: GGUFValue -> String
showVal (GVUInt8 n)    = show n
showVal (GVInt8 n)     = show n
showVal (GVUInt16 n)   = show n
showVal (GVInt16 n)    = show n
showVal (GVUInt32 n)   = show n
showVal (GVInt32 n)    = show n
showVal (GVUInt64 n)   = show n
showVal (GVInt64 n)    = show n
showVal (GVFloat32 f)  = show f
showVal (GVFloat64 f)  = show f
showVal (GVBool b)     = show b
showVal (GVString s)   = show s
showVal (GVArray vs)   = "[" ++ show (length vs) ++ " elements]"

when :: Bool -> IO () -> IO ()
when True  a = a
when False _ = return ()
