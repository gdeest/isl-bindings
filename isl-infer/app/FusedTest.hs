-- | Test: compile the polyhedral fused layer with GCC.
module Main where

import System.IO (hSetBuffering, stdout, BufferMode(..))
import Text.Printf (printf)

import Isl.Infer.Arch (zen5)
import Isl.Infer.Kernel.GEMM (WeightQuant(..))
import Isl.Infer.Kernel.FusedLayer (compileLayerPacked, CompiledLayer(..))
import Isl.Infer.Model (LlamaConfig(..))
import Isl.Infer.Schedule (kvFloat32)

-- | A minimal Llama-like config for testing (similar to a tiny model).
testConfig :: LlamaConfig
testConfig = LlamaConfig
  { lcDim          = 512
  , lcHiddenDim    = 1376   -- ~2.7x dim, rounded to 32
  , lcNLayers      = 4
  , lcNHeads       = 8
  , lcNKVHeads     = 4      -- GQA
  , lcHeadDim      = 64
  , lcVocabSize    = 32000
  , lcMaxSeqLen    = 2048
  , lcRopeFreqBase = 10000.0
  , lcRMSNormEps   = 1.0e-5
  , lcKVDim        = 256    -- 4 * 64
  }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "=== Fused Layer Compilation Test ==="
  putStrLn ""

  -- Compile the fused layer with polyhedral projections (non-VNNI float path)
  printf "Compiling fused layer (Q8, float path)...\n"
  printf "  dim=%d, kvd=%d, hid=%d, nh=%d, nkv=%d, hd=%d\n"
    (lcDim testConfig) (lcKVDim testConfig) (lcHiddenDim testConfig)
    (lcNHeads testConfig) (lcNKVHeads testConfig) (lcHeadDim testConfig)
  cl <- compileLayerPacked kvFloat32 WQ8 False zen5 testConfig
  printf "  Compiled OK, source length: %d chars\n" (length (clSource cl))

  -- Show a snippet of the generated C to verify polyhedral projections are there
  let srcLines = lines (clSource cl)
      polyLines = filter (\l -> "polyhedral" `isIn` l) srcLines
  if null polyLines
    then putStrLn "  WARNING: no polyhedral projection markers found!"
    else do
      printf "  Found %d polyhedral projection markers:\n" (length polyLines)
      mapM_ (\l -> putStrLn $ "    " ++ take 80 l) polyLines

  -- Also check that the ISL macros are present (they're emitted by the polyhedral path)
  let islMacros = filter (\l -> "ISL_FLOOR_DIV" `isIn` l || "ISL_CEIL_DIV" `isIn` l) srcLines
  if null islMacros
    then putStrLn "  NOTE: no ISL macros found (projections may still be hand-crafted)"
    else printf "  ISL macros present (%d references)\n" (length islMacros)

  putStrLn "\n*** PASS: fused layer compiled successfully ***"

isIn :: String -> String -> Bool
isIn [] _          = True
isIn _  []         = False
isIn needle hay@(_:hs)
  | isPrefixOf needle hay = True
  | otherwise             = isIn needle hs

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _          = True
isPrefixOf _  []         = False
isPrefixOf (a:as) (b:bs) = a == b && isPrefixOf as bs
