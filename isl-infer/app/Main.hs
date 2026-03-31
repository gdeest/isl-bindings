-- | ISL-Infer CLI: polyhedral LLM inference engine.
module Main where

import Control.Monad (when)
import Data.IORef
import Data.Int (Int64)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Ptr (Ptr, castPtr)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, hFlush, BufferMode(..))
import Text.Printf (printf)

import Isl.Infer.GGUF
import Isl.Infer.Model
import Isl.Infer.Tokenizer
import Isl.Infer.Forward
import Isl.Infer.Arch (zen5)
import Isl.Infer.Compile
import Isl.Infer.Schedule
import Isl.Infer.Kernel.Elementwise (sampleTopP)

-- ---------------------------------------------------------------------------
-- Wall-clock timing
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "clock_gettime"
  c_clock_gettime :: Int -> Ptr () -> IO Int

getTimeNs :: IO Word64
getTimeNs = do
  buf <- mallocArray 2 :: IO (Ptr CLong)
  _ <- c_clock_gettime 1 (castPtr buf)
  [sec, nsec] <- peekArray 2 buf
  free buf
  return (fromIntegral sec * 1000000000 + fromIntegral nsec)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    [path]            -> runChat path defaultSchedule
    [path, "--naive"] -> runChat path naiveSchedule
    _                 -> putStrLn "Usage: isl-infer <model.gguf> [--naive]"

runChat :: FilePath -> MatvecSchedule -> IO ()
runChat modelPath sch = do
  printf "Loading %s...\n" modelPath
  gf <- loadGGUF modelPath
  let cfg = extractConfig gf
  printf "  %s: dim=%d, layers=%d, heads=%d/%d, vocab=%d\n"
    (maybe "?" id (lookupMetaString gf "general.name"))
    (lcDim cfg) (lcNLayers cfg) (lcNHeads cfg) (lcNKVHeads cfg) (lcVocabSize cfg)

  let tok = loadTokenizer gf
  printf "  Tokenizer: %d tokens\n" (length (tokVocab tok))

  putStrLn ""
  kernels <- compileKernels cfg sch

  putStrLn "\nInitializing inference state..."
  is <- initInferState gf kernels
  putStrLn "Ready.\n"
  putStrLn "Commands: /quit, /schedule naive|default|large, /strategy original|packed"
  putStrLn ""

  chatLoop is tok gf 0

chatLoop :: InferState -> Tokenizer -> GGUFFile -> Int -> IO ()
chatLoop is tok gf pos = do
  putStr "> " >> hFlush stdout
  input <- getLine
  case input of
    "/quit" -> putStrLn "Bye."
    "/schedule naive"   -> recompile is tok gf pos naiveSchedule
    "/schedule default" -> recompile is tok gf pos defaultSchedule
    "/schedule small"   -> recompile is tok gf pos scheduleSmall
    "/schedule medium"  -> recompile is tok gf pos scheduleMedium
    "/schedule large"   -> recompile is tok gf pos scheduleLarge
    "/strategy original" -> recompileStrategy is tok gf pos StrategyOriginal defaultSchedule
    "/strategy packed"   -> recompileStrategy is tok gf pos StrategyPacked defaultSchedule
    "" -> chatLoop is tok gf pos
    _ -> do
      let userTokens = encode tok (" " ++ input ++ " ")
          tokens = [tokBOS tok, 3] ++ drop 1 userTokens ++ [4]
      printf "[%d tokens] " (length tokens)
      hFlush stdout

      -- Prefill (timed)
      t0 <- getTimeNs
      pos' <- prefill is tokens pos
      t1 <- getTimeNs
      let prefillMs = fromIntegral (t1 - t0) / 1e6 :: Double
          prefillToks = length tokens
      printf "prefill %.0fms (%.1f tok/s)\n" prefillMs
        (fromIntegral prefillToks / (prefillMs / 1000))
      hFlush stdout

      -- Generate (timed per-token)
      t2 <- getTimeNs
      (pos'', nGenerated) <- generate is tok pos' 512
      t3 <- getTimeNs
      let genMs = fromIntegral (t3 - t2) / 1e6 :: Double
          tokPerSec = fromIntegral nGenerated / (genMs / 1000) :: Double

      printf "\n  [%d tokens, %.0fms, %.2f tok/s]\n" nGenerated genMs tokPerSec
      chatLoop is tok gf pos''

recompile :: InferState -> Tokenizer -> GGUFFile -> Int -> MatvecSchedule -> IO ()
recompile is tok gf pos sch = do
  printf "Recompiling with schedule '%s'...\n" (schName sch)
  kernels <- compileKernels (isConfig is) sch
  let is' = is { isKernels = kernels }
  putStrLn "Done.\n"
  chatLoop is' tok gf pos

recompileStrategy :: InferState -> Tokenizer -> GGUFFile -> Int -> MatvecStrategy -> MatvecSchedule -> IO ()
recompileStrategy is tok gf pos strat sch = do
  printf "Recompiling with strategy '%s', schedule '%s'...\n" (show strat) (schName sch)
  kernels <- compileKernelsWith strat zen5 (isConfig is) sch
  let is' = is { isKernels = kernels }
  putStrLn "Done.\n"
  chatLoop is' tok gf pos

prefill :: InferState -> [Int] -> Int -> IO Int
prefill _  []     pos = return pos
prefill is (t:ts) pos = do
  forward is t pos
  prefill is ts (pos + 1)

sampleFromLogits :: InferState -> IO Int
sampleFromLogits is = do
  t <- sampleTopP (isLogits is) (fromIntegral (lcVocabSize (isConfig is))) 0.7 0.9 (isRNG is)
  return (fromIntegral t)

-- | Generate tokens, return (new_pos, tokens_generated).
generate :: InferState -> Tokenizer -> Int -> Int -> IO (Int, Int)
generate is tok startPos maxToks = go startPos 0
  where
    go pos n | n >= maxToks = return (pos, n)
    go pos n = do
      nextTok <- sampleFromLogits is
      let text = decodeSingle tok nextTok
      putStr text >> hFlush stdout
      if nextTok == tokEOS tok
        then return (pos + 1, n + 1)
        else do
          forward is nextTok pos
          go (pos + 1) (n + 1)
