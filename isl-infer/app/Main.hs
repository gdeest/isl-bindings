-- | ISL-Infer CLI: polyhedral LLM inference engine.
module Main where

import Control.Exception (try, IOException)
import Control.Monad (when)
import Data.List (isPrefixOf, stripPrefix)
import Data.IORef
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
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
import Isl.Infer.Kernel.GEMM (WeightQuant(..), GemmSchedule(..), GemmBatchPos(..)
                              , CompiledQ8Gemm(..)
                              , defaultGemmSchedule, compileQ8Gemm)
import Isl.Infer.Kernel.Elementwise (sampleTopP)
import Isl.Infer.Tune (tune, benchmarkAll, TuneResult(..), configLabel, toSchedule, TuneConfig(..))

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
-- Chat template auto-detection
-- ---------------------------------------------------------------------------

-- | Chat template format, auto-detected from tokenizer.
data ChatTemplate
  = MistralTemplate    -- ^ [INST] ... [/INST]  (BOS=1, INST=3, /INST=4)
  | Llama3Template     -- ^ <|start_header_id|>user<|end_header_id|> ... <|eot_id|>
    { ct3StartHeader :: !Int   -- <|start_header_id|>
    , ct3EndHeader   :: !Int   -- <|end_header_id|>
    , ct3EotId       :: !Int   -- <|eot_id|>
    }
  | PlainTemplate      -- ^ Fallback: just BOS + text + EOS
  deriving (Show)

-- | Detect chat template from tokenizer vocab.
detectTemplate :: Tokenizer -> ChatTemplate
detectTemplate tok
  | Just shId <- Map.lookup "<|start_header_id|>" (tokLookup tok)
  , Just ehId <- Map.lookup "<|end_header_id|>" (tokLookup tok)
  , Just eotId <- Map.lookup "<|eot_id|>" (tokLookup tok)
  = Llama3Template shId ehId eotId
  | tokBOS tok == 1  -- Mistral/Llama 2 style
  = MistralTemplate
  | otherwise
  = PlainTemplate

-- | Wrap user input into the chat template token sequence.
applyTemplate :: ChatTemplate -> Tokenizer -> String -> [Int]
applyTemplate MistralTemplate tok input =
  let userTokens = encode tok (" " ++ input ++ " ")
  in [tokBOS tok, 3] ++ drop 1 userTokens ++ [4]
applyTemplate (Llama3Template sh eh eot) tok input =
  let userTokens = encode tok input
  -- <|begin_of_text|><|start_header_id|>user<|end_header_id|>\n\n{input}<|eot_id|><|start_header_id|>assistant<|end_header_id|>\n\n
      userTag = encode tok "user"
      assistTag = encode tok "assistant"
      nlnl = encode tok "\n\n"
  in [tokBOS tok, sh] ++ drop 1 userTag ++ [eh] ++ drop 1 nlnl
     ++ drop 1 userTokens ++ [eot, sh] ++ drop 1 assistTag ++ [eh] ++ drop 1 nlnl
applyTemplate PlainTemplate tok input =
  let userTokens = encode tok input
  in [tokBOS tok] ++ drop 1 userTokens

-- | Check if a token signals end of generation for this template.
isEOG :: ChatTemplate -> Tokenizer -> Int -> Bool
isEOG tmpl tok t = t `elem` eosIds tmpl tok

-- | All end-of-generation token IDs for this template.
eosIds :: ChatTemplate -> Tokenizer -> [Int]
eosIds MistralTemplate tok = [tokEOS tok]
eosIds (Llama3Template _ _ eot) tok = [tokEOS tok, eot]
eosIds PlainTemplate tok = [tokEOS tok]

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let (ctxOverride, q4ffn, draftPath, rest) = parseArgs args
  case rest of
    [path]            -> runChat path defaultSchedule ctxOverride q4ffn draftPath
    [path, "--naive"] -> runChat path naiveSchedule ctxOverride q4ffn draftPath
    _                 -> putStrLn "Usage: isl-infer <model.gguf> [--naive] [--context N] [--q4-ffn] [--draft <draft.gguf>]"
  where
    parseArgs = go Nothing False Nothing
    go ctx q4 dr ("--context":n:xs) = go (Just (read n :: Int)) q4 dr xs
    go ctx _  dr ("--q4-ffn":xs)    = go ctx True dr xs
    go ctx q4 _  ("--draft":p:xs)   = go ctx q4 (Just p) xs
    go ctx q4 dr (x:xs)             = let (c, q, d, xs') = go ctx q4 dr xs in (c, q, d, x:xs')
    go ctx q4 dr []                 = (ctx, q4, dr, [])

runChat :: FilePath -> MatvecSchedule -> Maybe Int -> Bool -> Maybe FilePath -> IO ()
runChat modelPath sch ctxOverride q4ffn draftPath = do
  printf "Loading %s...\n" modelPath
  gf <- loadGGUF modelPath
  let cfg0 = extractConfig gf
      cfg = case ctxOverride of
        Nothing -> cfg0
        Just n  -> cfg0 { lcMaxSeqLen = n }
      ffnQ = if q4ffn then WQ4 else WQ8
  printf "  %s: dim=%d, layers=%d, heads=%d/%d, vocab=%d\n"
    (maybe "?" id (lookupMetaString gf "general.name"))
    (lcDim cfg) (lcNLayers cfg) (lcNHeads cfg) (lcNKVHeads cfg) (lcVocabSize cfg)

  when q4ffn $ putStrLn "  FFN weight quantization: Q4_0 (runtime downquant from Q8_0)"

  let tok = loadTokenizer gf
      tmpl = detectTemplate tok
  printf "  Tokenizer: %d tokens, template: %s\n" (length (tokVocab tok))
    (case tmpl of MistralTemplate -> "Mistral"; Llama3Template{} -> "Llama3"; PlainTemplate -> "Plain")

  putStrLn ""
  kernels <- compileKernelsWith StrategyOriginal zen5 kvFloat32 ffnQ cfg sch

  putStrLn "\nInitializing inference state..."
  is0 <- initInferState kvFloat32 cfg gf kernels
  is <- if q4ffn
    then downquantFFNWeights is0
    else return is0
  printf "  Context length: %d\n" (lcMaxSeqLen cfg)

  -- Load draft model if specified
  mDraft <- case draftPath of
    Nothing -> return Nothing
    Just dp -> do
      printf "\nLoading draft model %s...\n" dp
      dgf <- loadGGUF dp
      let dcfg0 = extractConfig dgf
          dcfg = dcfg0 { lcMaxSeqLen = lcMaxSeqLen cfg }  -- match main model context
      printf "  Draft: dim=%d, layers=%d, heads=%d/%d\n"
        (lcDim dcfg) (lcNLayers dcfg) (lcNHeads dcfg) (lcNKVHeads dcfg)
      dKernels <- compileKernelsWith StrategyOriginal zen5 kvFloat32 WQ8 dcfg sch
      dIs <- initInferState kvFloat32 dcfg dgf dKernels
      let draftWeightMB = lcNLayers dcfg * (lcDim dcfg * lcDim dcfg * 7 + lcHiddenDim dcfg * lcDim dcfg * 3) `div` (32 * 1024 * 1024) * 34
      printf "  Draft weight traffic: ~%d MB/token\n" draftWeightMB
      putStrLn "  Draft model ready."
      return (Just dIs)

  putStrLn "Ready.\n"
  putStrLn "Commands: /quit, /schedule naive|default|large, /strategy original|packed|packed-poly|autotuned"
  putStrLn "          /vnni on|off, /kvcache float32|q8|q4|k8v4|k4v8, /q4ffn"
  putStrLn "          /prefill batched|sequential, /tune, /read <filepath>"
  case mDraft of
    Nothing -> putStrLn "          /speculate 4|8 (n-gram only, no draft model)"
    Just _  -> putStrLn "          /speculate 4|8 (draft model), /speculate ngram 4|8"
  putStrLn ""

  chatLoop is tok tmpl gf 0 mDraft

chatLoop :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Int -> Maybe InferState -> IO ()
chatLoop is tok tmpl gf pos mDraft = do
  putStr "> " >> hFlush stdout
  input <- getLine
  case input of
    "/quit" -> putStrLn "Bye."
    "/schedule naive"   -> recompile is tok tmpl gf pos mDraft naiveSchedule
    "/schedule default" -> recompile is tok tmpl gf pos mDraft defaultSchedule
    "/schedule small"   -> recompile is tok tmpl gf pos mDraft scheduleSmall
    "/schedule medium"  -> recompile is tok tmpl gf pos mDraft scheduleMedium
    "/schedule large"   -> recompile is tok tmpl gf pos mDraft scheduleLarge
    "/strategy original"    -> recompileStrategy is tok tmpl gf pos mDraft StrategyOriginal defaultSchedule
    "/strategy packed"      -> recompileStrategy is tok tmpl gf pos mDraft StrategyPacked defaultSchedule
    "/strategy packed-poly" -> recompileStrategy is tok tmpl gf pos mDraft StrategyPackedPoly defaultSchedule
    "/strategy autotuned"   -> recompileStrategy is tok tmpl gf pos mDraft StrategyAutotuned defaultSchedule
    "/kvcache float32"    -> recompileKV is tok tmpl gf mDraft kvFloat32
    "/kvcache q8"         -> recompileKV is tok tmpl gf mDraft kvQ8
    "/kvcache q4"         -> recompileKV is tok tmpl gf mDraft kvQ4
    "/kvcache k8v4"       -> recompileKV is tok tmpl gf mDraft kvK8V4
    "/kvcache k4v8"       -> recompileKV is tok tmpl gf mDraft kvK4V8
    "/vnni on"            -> do putStrLn "Enabling VNNI int8×int8 accumulation..."
                                recompileStrategy is tok tmpl gf pos mDraft StrategyPacked
                                  (defaultSchedule { schVnni = True })
    "/vnni off"           -> do putStrLn "Disabling VNNI (float path)..."
                                recompileStrategy is tok tmpl gf pos mDraft StrategyPacked
                                  (defaultSchedule { schVnni = False })
    "/q4ffn"              -> recompileQ4FFN is tok tmpl gf mDraft
    "/prefill batched"    -> do putStrLn "Prefill mode: batched (polyhedral GEMM)"
                                chatLoopBatched is tok tmpl gf pos mDraft
    "/prefill sequential" -> do putStrLn "Prefill mode: sequential (fused layer)"
                                chatLoop is tok tmpl gf pos mDraft
    "/tune" -> do
      let ffnQ = WQ8
          kvM  = isKVMode is
      result <- tune zen5 (isConfig is) gf is ffnQ kvM 16
      let bestSch = toSchedule (trConfig result)
      putStrLn "Recompiling with tuned schedule..."
      kernels <- compileKernelsWith (tcStrategy (trConfig result)) zen5 kvM ffnQ (isConfig is) bestSch
      chatLoop (is { isKernels = kernels }) tok tmpl gf pos mDraft
    "/benchmark" -> do
      let ffnQ = WQ8
          kvM  = isKVMode is
      _ <- benchmarkAll zen5 (isConfig is) gf is ffnQ kvM 8
      chatLoop is tok tmpl gf pos mDraft
    "/speculate off"      -> do putStrLn "Speculative decode: OFF"
                                chatLoop is tok tmpl gf pos mDraft
    "/speculate 4"        -> do putStrLn $ "Speculative decode: K=4"
                                  ++ maybe " (n-gram)" (const " (draft model)") mDraft
                                chatLoopSpec is tok tmpl gf pos 4 mDraft
    "/speculate 8"        -> do putStrLn $ "Speculative decode: K=8"
                                  ++ maybe " (n-gram)" (const " (draft model)") mDraft
                                chatLoopSpec is tok tmpl gf pos 8 mDraft
    "/gemm" -> do putStrLn "GEMM schedule parameters:"
                  putStrLn "  /gemm batch outer|inner|between  — batch loop placement"
                  putStrLn "  /gemm tileJ <n>                  — J tile size (0=none)"
                  putStrLn "  /gemm tileKB <n>                 — KB tile size (0=none)"
                  putStrLn "  /gemm show                       — show current schedule"
                  chatLoop is tok tmpl gf pos mDraft
    "" -> chatLoop is tok tmpl gf pos mDraft
    _ | Just arg <- stripPrefix "/gemm batch " input ->
          case arg of
            "outer"   -> recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsBatchPos = BOutermost })
            "inner"   -> recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsBatchPos = BInnermost })
            "between" -> recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsBatchPos = BBetweenJK })
            _         -> do putStrLn "Unknown batch position. Use: outer|inner|between"
                            chatLoop is tok tmpl gf pos mDraft
      | Just arg <- stripPrefix "/gemm tileJ " input ->
          recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsTileJ = read arg })
      | Just arg <- stripPrefix "/gemm tileKB " input ->
          recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsTileKB = read arg })
      | "/gemm show" <- input -> do
          showGemmSchedule is
          chatLoop is tok tmpl gf pos mDraft
      | Just path <- stripPrefix "/read " input -> readFileCmd is tok tmpl gf pos mDraft (dropWhile (== ' ') path)
      | otherwise -> do
      let tokens = applyTemplate tmpl tok input
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
      (pos'', nGenerated) <- generate is tok tmpl pos' 512
      t3 <- getTimeNs
      let genMs = fromIntegral (t3 - t2) / 1e6 :: Double
          tokPerSec = fromIntegral nGenerated / (genMs / 1000) :: Double

      printf "\n  [%d tokens, %.0fms, %.2f tok/s]\n" nGenerated genMs tokPerSec
      chatLoop is tok tmpl gf pos'' mDraft

-- | Chat loop using batched prefill (polyhedral GEMM).
chatLoopBatched :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Int -> Maybe InferState -> IO ()
chatLoopBatched is tok tmpl gf pos mDraft = do
  putStr "[batched]> " >> hFlush stdout
  input <- getLine
  case input of
    "/quit" -> putStrLn "Bye."
    "/prefill sequential" -> do putStrLn "Prefill mode: sequential (fused layer)"
                                chatLoop is tok tmpl gf pos mDraft
    "/gemm" -> do putStrLn "GEMM schedule parameters:"
                  putStrLn "  /gemm batch outer|inner|between  — batch loop placement"
                  putStrLn "  /gemm tileJ <n>                  — J tile size (0=none)"
                  putStrLn "  /gemm tileKB <n>                 — KB tile size (0=none)"
                  putStrLn "  /gemm show                       — show current + recompile"
                  chatLoopBatched is tok tmpl gf pos mDraft
    _ | Just arg <- stripPrefix "/gemm batch " input ->
        case arg of
          "outer"   -> recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsBatchPos = BOutermost })
          "inner"   -> recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsBatchPos = BInnermost })
          "between" -> recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsBatchPos = BBetweenJK })
          _         -> do putStrLn "Unknown batch position. Use: outer|inner|between"
                          chatLoopBatched is tok tmpl gf pos mDraft
      | Just arg <- stripPrefix "/gemm tileJ " input ->
          recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsTileJ = read arg })
      | Just arg <- stripPrefix "/gemm tileKB " input ->
          recompileGemm is tok tmpl gf pos mDraft (\s -> s { gsTileKB = read arg })
      | "/gemm show" <- input -> do
          showGemmSchedule is
          chatLoopBatched is tok tmpl gf pos mDraft
    "" -> chatLoopBatched is tok tmpl gf pos mDraft
    _ -> do
      let tokens = applyTemplate tmpl tok input
      printf "[%d tokens] " (length tokens)
      hFlush stdout

      -- Batched prefill (timed)
      t0 <- getTimeNs
      pos' <- prefillBatched is tokens pos
      t1 <- getTimeNs
      let prefillMs = fromIntegral (t1 - t0) / 1e6 :: Double
          prefillToks = length tokens
      printf "prefill %.0fms (%.1f tok/s) [batched/polyhedral]\n" prefillMs
        (fromIntegral prefillToks / (prefillMs / 1000))
      hFlush stdout

      -- Generate (timed per-token, same as sequential)
      t2 <- getTimeNs
      (pos'', nGenerated) <- generate is tok tmpl pos' 512
      t3 <- getTimeNs
      let genMs = fromIntegral (t3 - t2) / 1e6 :: Double
          tokPerSec = fromIntegral nGenerated / (genMs / 1000) :: Double

      printf "\n  [%d tokens, %.0fms, %.2f tok/s]\n" nGenerated genMs tokPerSec
      chatLoopBatched is tok tmpl gf pos'' mDraft

-- | Chat loop with speculative N-gram decode.
-- Uses forwardBatchAllLogits for verification — weight traffic amortized K-fold.
chatLoopSpec :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Int -> Int -> Maybe InferState -> IO ()
chatLoopSpec is tok tmpl gf pos specK mDraft = do
  let modeTag = maybe "ngram" (const "draft") mDraft
  printf "[spec K=%d %s]> " specK modeTag >> hFlush stdout
  input <- getLine
  case input of
    "/quit" -> putStrLn "Bye."
    "/speculate off" -> do putStrLn "Speculative decode: OFF"
                           chatLoop is tok tmpl gf pos mDraft
    "" -> chatLoopSpec is tok tmpl gf pos specK mDraft
    _ | Just path <- stripPrefix "/read " input -> readFileCmd is tok tmpl gf pos mDraft (dropWhile (== ' ') path)
      | "/" `isPrefixOf` input -> do
          -- Forward slash commands to the main chatLoop handler
          putStrLn "Use /speculate off to return to normal mode for config commands."
          chatLoopSpec is tok tmpl gf pos specK mDraft
      | otherwise -> do
      let tokens = applyTemplate tmpl tok input
      printf "[%d tokens] " (length tokens)
      hFlush stdout

      -- Prefill both models (batched GEMM for speed)
      t0 <- getTimeNs
      pos' <- prefillBatched is tokens pos
      case mDraft of
        Just dIs -> do _ <- prefillBatched dIs tokens pos; return ()
        Nothing  -> return ()
      t1 <- getTimeNs
      let prefillMs = fromIntegral (t1 - t0) / 1e6 :: Double
      printf "prefill %.0fms (%.1f tok/s)\n" prefillMs
        (fromIntegral (length tokens) / (prefillMs / 1000))
      hFlush stdout

      -- Speculative generation (timed)
      t2 <- getTimeNs

      (pos'', nGenerated, stats) <- generateSpeculative is tok tmpl pos' 512 specK mDraft
      t3 <- getTimeNs
      let genMs = fromIntegral (t3 - t2) / 1e6 :: Double
          tokPerSec = fromIntegral nGenerated / (genMs / 1000) :: Double
          acceptRate = if ssDrafted stats > 0
                       then 100.0 * fromIntegral (ssAccepted stats) / fromIntegral (ssDrafted stats) :: Double
                       else 0.0

      printf "\n  [%d tokens, %.0fms, %.2f tok/s (spec K=%d)]\n"
        nGenerated genMs tokPerSec specK
      printf "  spec stats: %d cycles, %d drafted, %d accepted (%.1f%%), %d fallbacks\n"
        (ssCycles stats) (ssDrafted stats) (ssAccepted stats) acceptRate (ssFallbacks stats)
      chatLoopSpec is tok tmpl gf pos'' specK mDraft

recompile :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Int -> Maybe InferState -> MatvecSchedule -> IO ()
recompile is tok tmpl gf pos mDraft sch = do
  printf "Recompiling with schedule '%s'...\n" (schName sch)
  kernels <- compileKernels (isConfig is) sch
  let is' = is { isKernels = kernels }
  putStrLn "Done.\n"
  chatLoop is' tok tmpl gf pos mDraft

recompileStrategy :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Int -> Maybe InferState -> MatvecStrategy -> MatvecSchedule -> IO ()
recompileStrategy is tok tmpl gf pos mDraft strat sch = do
  printf "Recompiling with strategy '%s', schedule '%s'...\n" (show strat) (schName sch)
  kernels <- compileKernelsWith strat zen5 (isKVMode is) WQ8 (isConfig is) sch
  let is' = is { isKernels = kernels }
  putStrLn "Done.\n"
  chatLoop is' tok tmpl gf pos mDraft

recompileKV :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Maybe InferState -> KVCacheMode -> IO ()
recompileKV is tok tmpl gf mDraft kvMode = do
  printf "Switching KV cache to %s (resetting context)...\n" (show kvMode)
  -- Reallocate KV cache and recompile fused layer
  is' <- initInferState kvMode (isConfig is) gf (isKernels is)
  -- Recompile fused layer with new KV mode
  kernels <- compileKernelsWith StrategyPacked zen5 kvMode WQ8 (isConfig is) defaultSchedule
  let is'' = is' { isKernels = kernels }
  putStrLn "Done.\n"
  chatLoop is'' tok tmpl gf 0 mDraft  -- reset position

recompileQ4FFN :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Maybe InferState -> IO ()
recompileQ4FFN is tok tmpl gf mDraft = do
  let q4 = isWeightOverlay is
  if not (null q4)
    then do putStrLn "Already in Q4 FFN mode. Use /q8ffn to switch back."
            chatLoop is tok tmpl gf 0 mDraft
    else do putStrLn "Switching FFN to Q4 (runtime downquant + kernel recompile)..."
            kernels <- compileKernelsWith StrategyOriginal zen5 (isKVMode is) WQ4
                         (isConfig is) defaultSchedule
            let is' = is { isKernels = kernels }
            is'' <- downquantFFNWeights is'
            putStrLn "Done.\n"
            chatLoop is'' tok tmpl gf 0 mDraft

-- | Recompile prefill GEMM kernels with a modified schedule.
-- Returns the updated InferState (caller decides which chat loop to resume).
recompileGemm :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Int -> Maybe InferState
              -> (GemmSchedule -> GemmSchedule) -> IO ()
recompileGemm is tok tmpl gf pos mDraft modSch = do
  let oldSch = cqSchedule (ksPfQProj (isKernels is))
      newSch = (modSch oldSch) { gsName = "gemm_custom" }
  printf "Recompiling GEMM kernels (batch=%s, tileJ=%d, tileKB=%d)...\n"
    (show (gsBatchPos newSch)) (gsTileJ newSch) (gsTileKB newSch)
  let cfg  = isConfig is
      dim  = lcDim cfg
      kvd  = lcKVDim cfg
      hdim = lcHiddenDim cfg
      vocab = lcVocabSize cfg
      kb d = d `div` 32
      bMax = prefillBatchMax
      compile = compileQ8Gemm newSch bMax
  pqp  <- compile dim   (kb dim)
  pkp  <- compile kvd   (kb dim)
  pvp  <- compile kvd   (kb dim)
  pop  <- compile dim   (kb dim)
  pgp  <- compile hdim  (kb dim)
  pup  <- compile hdim  (kb dim)
  pdp  <- compile dim   (kb hdim)
  poutp <- compile vocab (kb dim)
  let ks' = (isKernels is)
        { ksPfQProj = pqp, ksPfKProj = pkp, ksPfVProj = pvp, ksPfOProj = pop
        , ksPfGate  = pgp, ksPfUp    = pup, ksPfDown  = pdp, ksPfOutput = poutp }
      is' = is { isKernels = ks' }
  putStrLn "Done."
  chatLoop is' tok tmpl gf pos mDraft

-- | Show current GEMM schedule.
showGemmSchedule :: InferState -> IO ()
showGemmSchedule is = do
  let sch = cqSchedule (ksPfQProj (isKernels is))
  printf "  GEMM schedule: %s\n" (gsName sch)
  printf "    batch position: %s\n" (show (gsBatchPos sch))
  printf "    tileJ:  %d\n" (gsTileJ sch)
  printf "    tileKB: %d\n" (gsTileKB sch)
  printf "    parallel: %s\n" (show (gsParallel sch))

readFileCmd :: InferState -> Tokenizer -> ChatTemplate -> GGUFFile -> Int -> Maybe InferState -> FilePath -> IO ()
readFileCmd is tok tmpl gf pos mDraft path = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left err -> do
      printf "Error: %s\n" (show err)
      chatLoop is tok tmpl gf pos mDraft
    Right contents -> do
      let nLines = length (lines contents)
          msg = "[File: " ++ path ++ " (" ++ show nLines ++ " lines)]\n" ++ contents
          userTokens = encode tok (" " ++ msg ++ " ")
          tokens = [tokBOS tok, 3] ++ drop 1 userTokens ++ [4]
      printf "[reading %s: %d lines, %d tokens]\n" path nLines (length tokens)
      hFlush stdout

      -- Prefill file contents into context
      t0 <- getTimeNs
      pos' <- prefill is tokens pos
      t1 <- getTimeNs
      let prefillMs = fromIntegral (t1 - t0) / 1e6 :: Double
      printf "prefill %.0fms (%.1f tok/s)\n" prefillMs
        (fromIntegral (length tokens) / (prefillMs / 1000))
      hFlush stdout

      -- Brief assistant acknowledgment
      t2 <- getTimeNs
      (pos'', nGenerated) <- generate is tok tmpl pos' 64
      t3 <- getTimeNs
      let genMs = fromIntegral (t3 - t2) / 1e6 :: Double
      printf "\n  [%d tokens, %.0fms, %.2f tok/s]\n" nGenerated genMs
        (fromIntegral nGenerated / (genMs / 1000))
      chatLoop is tok tmpl gf pos'' mDraft

prefill :: InferState -> [Int] -> Int -> IO Int
prefill _  []     pos = return pos
prefill is (t:ts) pos = do
  forward is t pos
  prefill is ts (pos + 1)

-- | Batched prefill using polyhedral Q8 GEMM kernels.
-- Processes tokens in chunks of B_MAX for weight matrix reuse.
prefillBatched :: InferState -> [Int] -> Int -> IO Int
prefillBatched _  []     pos = return pos
prefillBatched is tokens pos = go (chunksOf prefillBatchMax tokens) pos
  where
    go []           p = return p
    go (chunk:rest) p = do
      forwardBatch is chunk p
      go rest (p + length chunk)

-- | Split a list into chunks of at most n elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

sampleFromLogits :: InferState -> IO Int
sampleFromLogits is = do
  t <- sampleTopP (isLogits is) (fromIntegral (lcVocabSize (isConfig is))) 0.7 0.9 (isRNG is)
  return (fromIntegral t)

-- | Generate tokens, return (new_pos, tokens_generated).
generate :: InferState -> Tokenizer -> ChatTemplate -> Int -> Int -> IO (Int, Int)
generate is tok tmpl startPos maxToks = go startPos 0
  where
    go pos n | n >= maxToks = return (pos, n)
    go pos n = do
      nextTok <- sampleFromLogits is
      let text = decodeSingle tok nextTok
      putStr text >> hFlush stdout
      if isEOG tmpl tok nextTok
        then return (pos + 1, n + 1)
        else do
          forward is nextTok pos
          go (pos + 1) (n + 1)

-- ---------------------------------------------------------------------------
-- Speculative decode with N-gram drafting
-- ---------------------------------------------------------------------------

-- | Bigram table: maps (tok_{i-1}, tok_i) → frequency table of tok_{i+1}.
-- Used for zero-cost draft token prediction.
type NgramTable = Map.Map (Int, Int) (IntMap.IntMap Int)

-- | Insert an observed trigram into the table.
ngramInsert :: Int -> Int -> Int -> NgramTable -> NgramTable
ngramInsert t0 t1 t2 = Map.insertWith (IntMap.unionWith (+)) (t0, t1) (IntMap.singleton t2 1)

-- | Draft K tokens greedily from the n-gram table.
-- Returns as many as can be predicted (may be fewer than K).
ngramDraft :: NgramTable -> [Int] -> Int -> [Int]
ngramDraft _tbl hist k
  | length hist < 2 = []
  | otherwise       = go (hist !! (length hist - 2)) (last hist) k []
  where
    go _ _ 0 acc = reverse acc
    go prev1 prev2 n acc =
      case Map.lookup (prev1, prev2) _tbl of
        Nothing -> reverse acc  -- no prediction available
        Just fm ->
          let (bestTok, _) = IntMap.foldlWithKey'
                (\(bt, bc) tok cnt -> if cnt > bc then (tok, cnt) else (bt, bc))
                (-1, 0) fm
          in if bestTok < 0
             then reverse acc
             else go prev2 bestTok (n - 1) (bestTok : acc)

-- | Verify K draft tokens by running sequential forward passes (fused layer).
-- After each forward, isLogits has that token's logits. We sample immediately
-- rather than storing all logits first — this avoids copying and lets us
-- short-circuit on rejection.
--
-- Returns list of (draft_token, main_model_sampled_token) pairs.
verifyForward :: InferState -> [Int] -> Int -> IO [(Int, Int)]
verifyForward is tokens startPos =
  mapM (\(i, tok) -> do
    forward is tok (startPos + i)
    sampled <- sampleFromLogits is
    return (tok, sampled)
    ) (zip [0..] tokens)

-- | Sample from speculative logits buffer at a given batch index.
sampleFromSpecLogits :: InferState -> Int -> IO Int
sampleFromSpecLogits is idx = do
  let vs = lcVocabSize (isConfig is)
      ptr = isSpecLogits is `plusPtr` (idx * vs * 4)
  t <- sampleTopP ptr (fromIntegral vs) 0.7 0.9 (isRNG is)
  return (fromIntegral t)

-- | Speculation statistics for diagnostics.
data SpecStats = SpecStats
  { ssTokens    :: !Int  -- ^ Total tokens generated
  , ssCycles    :: !Int  -- ^ Total speculation cycles (draft+verify attempts)
  , ssDrafted   :: !Int  -- ^ Total tokens drafted (submitted to forwardBatch)
  , ssAccepted  :: !Int  -- ^ Total draft tokens accepted (matched sampled)
  , ssFallbacks :: !Int  -- ^ Times n-gram had no prediction (fell back to single forward)
  }

emptyStats :: SpecStats
emptyStats = SpecStats 0 0 0 0 0

-- | Speculative generation: draft K tokens, verify via batched forward pass,
-- accept prefix. Weight traffic amortized K-fold.
--
-- When a draft model is provided, uses it for drafting (high acceptance).
-- Otherwise falls back to n-gram table (zero draft cost, lower acceptance).
--
-- Returns (new_pos, tokens_generated, stats).
generateSpeculative :: InferState -> Tokenizer -> ChatTemplate -> Int -> Int -> Int -> Maybe InferState -> IO (Int, Int, SpecStats)
generateSpeculative is tok tmpl startPos maxToks specK mDraft = do
  -- First token: sample from prefill logits (already computed by forwardBatch)
  firstTok <- sampleFromLogits is
  let text = decodeSingle tok firstTok
  putStr text >> hFlush stdout
  if isEOG tmpl tok firstTok
    then return (startPos + 1, 1, emptyStats { ssTokens = 1, ssFallbacks = 1 })
    else do
      -- Forward both models with first token to update their KV caches
      forward is firstTok startPos
      case mDraft of
        Just dIs -> forward dIs firstTok startPos
        Nothing  -> return ()
      -- Enter main loop with lastTok set — draft model ready immediately
      goOuter (startPos + 1) 1 firstTok [firstTok] Map.empty
        (emptyStats { ssTokens = 1, ssFallbacks = 1 })
  where
    eos = eosIds tmpl tok

    goOuter pos n lastTok hist tbl stats
      | n >= maxToks = return (pos, n, stats)
      | otherwise = do
          -- Draft K tokens
          drafts <- case mDraft of
            Just dIs -> draftTokens dIs lastTok pos specK eos
            Nothing  -> return (ngramDraft tbl hist specK)
          if null drafts
            then do
              -- No prediction: fall back to single-token decode
              nextTok <- sampleFromLogits is
              let text' = decodeSingle tok nextTok
              putStr text' >> hFlush stdout
              let stats' = stats { ssTokens = ssTokens stats + 1
                                 , ssFallbacks = ssFallbacks stats + 1 }
              if isEOG tmpl tok nextTok
                then return (pos + 1, n + 1, stats')
                else do
                  forward is nextTok pos
                  case mDraft of
                    Just dIs -> forward dIs nextTok pos
                    Nothing  -> return ()
                  let hist' = hist ++ [nextTok]
                      tbl' = updateNgram hist nextTok tbl
                  goOuter (pos + 1) (n + 1) nextTok hist' tbl' stats'
            else do
              -- Verify drafts via packed GEMM batch (weight traffic = 1× for K tokens)
              let nDrafted = length drafts
              forwardBatchAllLogits is drafts pos
              -- Accept prefix: sample from specLogits, compare with drafts
              (accepted, nMatch, finalTok, newHist, newTbl) <- verifyAccept is tok tmpl drafts pos hist tbl
              let stats' = stats { ssTokens = ssTokens stats + accepted
                                 , ssCycles = ssCycles stats + 1
                                 , ssDrafted = ssDrafted stats + nDrafted
                                 , ssAccepted = ssAccepted stats + nMatch }
              -- Sync draft model (cheap: 1B forward)
              case mDraft of
                Just dIs | finalTok >= 0 && not (isEOG tmpl tok finalTok) ->
                  forward dIs finalTok (pos + accepted - 1)
                _ -> return ()
              goOuter (pos + accepted) (n + accepted) finalTok newHist newTbl stats'

    -- Sample from specLogits at each position, compare with drafts, emit accepted.
    verifyAccept mainIs tokenizer template drafts startP hist0 tbl0 = go drafts 0 hist0 tbl0
      where
        go [] _ hist tbl = return (0, 0, -1, hist, tbl)
        go (draft:rest) idx hist tbl = do
          sampled <- sampleFromSpecLogits mainIs idx
          let text' = decodeSingle tokenizer sampled
          putStr text' >> hFlush stdout
          let tbl' = updateNgram hist sampled tbl
              hist' = hist ++ [sampled]
          if isEOG template tokenizer sampled
            then return (1, 0, sampled, hist', tbl')
            else if sampled == draft
              then do
                (more, moreMatch, lt, h, t) <- go rest (idx + 1) hist' tbl'
                let lastTok' = if lt >= 0 then lt else sampled
                return (1 + more, 1 + moreMatch, lastTok', h, t)
              else do
                -- Rejected: accept sampled, forward main model to update isLogits
                forward mainIs sampled (startP + idx)
                return (1, 0, sampled, hist', tbl')

    updateNgram hist nextTok tbl
      | length hist >= 2 = ngramInsert (hist !! (length hist - 2)) (last hist) nextTok tbl
      | otherwise        = tbl
