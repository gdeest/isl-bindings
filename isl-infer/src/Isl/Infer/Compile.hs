-- | Kernel compilation orchestrator.
--
-- At model load time, compiles all needed matvec kernels for the
-- model's specific dimensions. Different weight matrix shapes get
-- different kernels (and potentially different schedules).
--
-- The 'KernelSet' is passed to the forward pass and can be swapped
-- at runtime when the user changes schedules.
module Isl.Infer.Compile
  ( KernelSet(..)
  , compileKernels
  , compileKernelsWith
  , callMatvec
  , prefillBatchMax
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)

import Isl.Infer.Arch
import Isl.Infer.Autotune (autotune)
import Isl.Infer.Model (LlamaConfig(..))
import Isl.Infer.Schedule
import Isl.Infer.Kernel.FusedLayer
import Isl.Infer.Kernel.GEMM
  ( WeightQuant(..), CompiledMatvec(..), compileMatvec, compileMatvecQ
  , CompiledQ8Gemm(..), GemmSchedule(..), GemmBatchPos(..), defaultGemmSchedule
  , compileQ8Gemm, compileGemmQ, compileGemmQV, callQ8Gemm
  )
import Isl.Infer.Kernel.Packed
import Isl.Infer.Kernel.PackedPoly (compilePackedMatvecPoly, CompiledPackedMatvecPoly(..))
import Isl.Infer.Kernel.Elementwise (q8Matvec)

-- | Maximum batch size for prefill GEMM kernels.
-- Kernels are specialized for this B at compile time; runtime B can be smaller.
prefillBatchMax :: Int
prefillBatchMax = 128

-- | All compiled kernels needed for one model.
-- Different shapes get different compilations.
data KernelSet = KernelSet
  { ksQProj    :: !CompiledMatvec   -- ^ [dim] → [dim]       (Q projection)
  , ksKProj    :: !CompiledMatvec   -- ^ [dim] → [kv_dim]    (K projection)
  , ksVProj    :: !CompiledMatvec   -- ^ [dim] → [kv_dim]    (V projection)
  , ksOProj    :: !CompiledMatvec   -- ^ [dim] → [dim]       (output projection)
  , ksGate     :: !CompiledMatvec   -- ^ [dim] → [hidden]    (FFN gate)
  , ksUp       :: !CompiledMatvec   -- ^ [dim] → [hidden]    (FFN up)
  , ksDown     :: !CompiledMatvec   -- ^ [hidden] → [dim]    (FFN down)
  , ksOutput   :: !CompiledMatvec   -- ^ [dim] → [vocab]     (final projection)
  , ksFusedLayer :: !CompiledLayer  -- ^ Fused per-layer kernel
    -- Prefill GEMM kernels (polyhedral, batched)
  , ksPfQProj  :: !CompiledQ8Gemm  -- ^ [B,dim] → [B,dim]   (Q, prefill)
  , ksPfKProj  :: !CompiledQ8Gemm  -- ^ [B,dim] → [B,kvd]   (K, prefill)
  , ksPfVProj  :: !CompiledQ8Gemm  -- ^ [B,dim] → [B,kvd]   (V, prefill)
  , ksPfOProj  :: !CompiledQ8Gemm  -- ^ [B,dim] → [B,dim]   (O, prefill)
  , ksPfGate   :: !CompiledQ8Gemm  -- ^ [B,dim] → [B,hid]   (Gate, prefill)
  , ksPfUp     :: !CompiledQ8Gemm  -- ^ [B,dim] → [B,hid]   (Up, prefill)
  , ksPfDown   :: !CompiledQ8Gemm  -- ^ [B,hid] → [B,dim]   (Down, prefill)
  , ksPfOutput :: !CompiledQ8Gemm  -- ^ [B,dim] → [B,vocab]  (Output, prefill)
    -- Packed GEMM kernels (panel-packed, for fast batch verify in speculative decode)
  , ksPkQProj  :: !CompiledPackedGemm  -- ^ [B,dim] → [B,dim]   (Q, packed)
  , ksPkKProj  :: !CompiledPackedGemm  -- ^ [B,dim] → [B,kvd]   (K, packed)
  , ksPkVProj  :: !CompiledPackedGemm  -- ^ [B,dim] → [B,kvd]   (V, packed)
  , ksPkOProj  :: !CompiledPackedGemm  -- ^ [B,dim] → [B,dim]   (O, packed)
  , ksPkGate   :: !CompiledPackedGemm  -- ^ [B,dim] → [B,hid]   (Gate, packed)
  , ksPkUp     :: !CompiledPackedGemm  -- ^ [B,dim] → [B,hid]   (Up, packed)
  , ksPkDown   :: !CompiledPackedGemm  -- ^ [B,hid] → [B,dim]   (Down, packed)
  , ksPkOutput :: !CompiledPackedGemm  -- ^ [B,dim] → [B,vocab]  (Output, packed)
  }

-- | Compile a matvec using the selected strategy (Q8_0).
-- StrategyOriginal: polyhedral scanner + Q8 inner loop (original path).
-- StrategyPacked: panel-packed microkernel with arch-derived tiles.
compileMatvecStrategy :: MatvecStrategy -> Arch -> MatvecSchedule -> Int -> Int -> IO CompiledMatvec
compileMatvecStrategy strat arch = compileMatvecStrategyQ WQ8 strat arch

-- | Compile a matvec with chosen weight quantization and strategy.
compileMatvecStrategyQ :: WeightQuant -> MatvecStrategy -> Arch -> MatvecSchedule -> Int -> Int -> IO CompiledMatvec
compileMatvecStrategyQ wq StrategyOriginal _arch sch n kb = compileMatvecQ wq sch n kb
compileMatvecStrategyQ wq StrategyPacked   arch  sch n kb = do
  cpm <- compilePackedMatvecQ wq arch n kb
  -- Wrap as CompiledMatvec for uniform interface
  return CompiledMatvec
    { cmFn       = cpmFn cpm
    , cmSchedule = sch
    , cmN        = n
    , cmKBlocks  = kb
    , cmKernel   = cpmKernel cpm
    , cmSource   = cpmSource cpm
    }
compileMatvecStrategyQ wq StrategyPackedPoly arch sch n kb = do
  let tiles = archTiles arch n (kb * 32)
  cpmp <- compilePackedMatvecPoly wq arch tiles n kb
  return CompiledMatvec
    { cmFn       = cpmpFn cpmp
    , cmSchedule = sch
    , cmN        = n
    , cmKBlocks  = kb
    , cmKernel   = cpmpKernel cpmp
    , cmSource   = cpmpSource cpmp
    }
compileMatvecStrategyQ wq StrategyAutotuned arch sch n kb = do
  tiles <- autotune arch n (kb * 32)
  cpm <- compilePackedMatvecWithQ wq arch tiles n kb
  return CompiledMatvec
    { cmFn       = cpmFn cpm
    , cmSchedule = sch
    , cmN        = n
    , cmKBlocks  = kb
    , cmKernel   = cpmKernel cpm
    , cmSource   = cpmSource cpm
    }

-- | Compile all kernels for a model's dimensions.
--
-- Each distinct (N, K) shape gets its own specialized kernel.
-- The schedule can differ per shape (e.g., small KV projections
-- don't benefit from parallelism).
--
-- The strategy selects between original (ymm) and packed (zmm) kernels.
-- Both use the same schedule parameters for tiling/parallelism;
-- the packed strategy additionally uses Arch for microkernel tile sizes.
compileKernels :: LlamaConfig -> MatvecSchedule -> IO KernelSet
compileKernels = compileKernelsWith StrategyOriginal zen5 kvFloat32 WQ8

-- | Like 'compileKernels' but with explicit strategy, architecture, KV cache mode,
-- and FFN weight quantization. Attention projections always use Q8; the ffnQuant
-- parameter controls gate/up/down weight quantization for bandwidth reduction.
compileKernelsWith :: MatvecStrategy -> Arch -> KVCacheMode -> WeightQuant
                   -> LlamaConfig -> MatvecSchedule -> IO KernelSet
compileKernelsWith strategy arch kvMode ffnQuant cfg sch = do
  let dim   = lcDim cfg
      kvd   = lcKVDim cfg
      hdim  = lcHiddenDim cfg
      vocab = lcVocabSize cfg
      kb d  = d `div` 32  -- K blocks
      vnni  = schVnni sch

      -- Choose schedule per shape: small matrices don't parallelize well
      schFor n
        | n < 256   = sch { schParallel = False, schName = schName sch ++ "_small" }
        | otherwise  = sch

  let compileQ8  = compileMatvecStrategyQ WQ8     strategy arch
      compileFFN = compileMatvecStrategyQ ffnQuant strategy arch
      stratLabel = case strategy of
        StrategyOriginal  -> "original"
        StrategyPacked    -> "packed/" ++ archName arch
        StrategyPackedPoly -> "packed-poly/" ++ archName arch
        StrategyAutotuned -> "autotuned/" ++ archName arch
      ffnLabel = case ffnQuant of WQ8 -> "Q8"; WQ4 -> "Q4"; WQ4K -> "Q4_K"
      vnniLabel = if vnni then ", VNNI" else ""

  putStrLn $ "  Compiling kernels (schedule: " ++ schName sch
           ++ ", strategy: " ++ stratLabel
           ++ ", FFN quant: " ++ ffnLabel ++ vnniLabel ++ ")..."

  -- Attention projections: always Q8
  qp <- compileQ8 (schFor dim)   dim   (kb dim)
  putStr "    Q" >> putStrLn (" [" ++ show dim ++ "→" ++ show dim ++ "] Q8 OK")

  kp <- compileQ8 (schFor kvd)   kvd   (kb dim)
  putStr "    K" >> putStrLn (" [" ++ show dim ++ "→" ++ show kvd ++ "] Q8 OK")

  vp <- compileQ8 (schFor kvd)   kvd   (kb dim)
  putStr "    V" >> putStrLn (" [" ++ show dim ++ "→" ++ show kvd ++ "] Q8 OK")

  op <- compileQ8 (schFor dim)   dim   (kb dim)
  putStr "    O" >> putStrLn (" [" ++ show dim ++ "→" ++ show dim ++ "] Q8 OK")

  -- FFN projections: configurable (Q8 or Q4)
  gp <- compileFFN (schFor hdim)  hdim  (kb dim)
  putStr "    Gate" >> putStrLn (" [" ++ show dim ++ "→" ++ show hdim ++ "] " ++ ffnLabel ++ " OK")

  up <- compileFFN (schFor hdim)  hdim  (kb dim)
  putStr "    Up" >> putStrLn (" [" ++ show dim ++ "→" ++ show hdim ++ "] " ++ ffnLabel ++ " OK")

  dp <- compileFFN (schFor dim)   dim   (kb hdim)
  putStr "    Down" >> putStrLn (" [" ++ show hdim ++ "→" ++ show dim ++ "] " ++ ffnLabel ++ " OK")

  outp <- compileQ8 (schFor vocab) vocab (kb dim)
  putStr "    Output" >> putStrLn (" [" ++ show dim ++ "→" ++ show vocab ++ "] Q8 OK")

  -- Fused layer kernel (packed version uses panel-packed matvec bodies)
  putStrLn $ "    Fused layer kernel (kv: " ++ show kvMode ++ ", FFN: " ++ ffnLabel ++ vnniLabel ++ ")..."
  fl <- case strategy of
    StrategyOriginal  -> compileLayer kvMode ffnQuant vnni cfg
    StrategyPacked    -> compileLayerPacked kvMode ffnQuant vnni arch cfg
    StrategyPackedPoly -> compileLayerPacked kvMode ffnQuant vnni arch cfg  -- fused layer not yet polyhedral
    StrategyAutotuned -> compileLayerPacked kvMode ffnQuant vnni arch cfg
  putStrLn "    Fused layer OK"

  -- Prefill GEMM kernels (polyhedral, batched)
  let bMax = prefillBatchMax
      gemmSch = defaultGemmSchedule
      compileGemmQ8  = compileGemmQV vnni WQ8
      compileGemmFFN = compileGemmQV vnni ffnQuant
  putStrLn $ "  Compiling prefill GEMM kernels (B_MAX=" ++ show bMax
           ++ ", schedule: " ++ gsName gemmSch ++ ")..."

  pqp  <- compileGemmQ8 gemmSch bMax dim   (kb dim)
  putStrLn $ "    PF-Q [B," ++ show dim ++ "→B," ++ show dim ++ "] Q8 OK"

  pkp  <- compileGemmQ8 gemmSch bMax kvd   (kb dim)
  putStrLn $ "    PF-K [B," ++ show dim ++ "→B," ++ show kvd ++ "] Q8 OK"

  pvp  <- compileGemmQ8 gemmSch bMax kvd   (kb dim)
  putStrLn $ "    PF-V [B," ++ show dim ++ "→B," ++ show kvd ++ "] Q8 OK"

  pop  <- compileGemmQ8 gemmSch bMax dim   (kb dim)
  putStrLn $ "    PF-O [B," ++ show dim ++ "→B," ++ show dim ++ "] Q8 OK"

  pgp  <- compileGemmFFN gemmSch bMax hdim  (kb dim)
  putStrLn $ "    PF-Gate [B," ++ show dim ++ "→B," ++ show hdim ++ "] " ++ ffnLabel ++ " OK"

  pup  <- compileGemmFFN gemmSch bMax hdim  (kb dim)
  putStrLn $ "    PF-Up [B," ++ show dim ++ "→B," ++ show hdim ++ "] " ++ ffnLabel ++ " OK"

  pdp  <- compileGemmFFN gemmSch bMax dim   (kb hdim)
  putStrLn $ "    PF-Down [B," ++ show hdim ++ "→B," ++ show dim ++ "] " ++ ffnLabel ++ " OK"

  poutp <- compileGemmQ8 gemmSch bMax vocab (kb dim)
  putStrLn $ "    PF-Output [B," ++ show dim ++ "→B," ++ show vocab ++ "] Q8 OK"

  -- Packed GEMM kernels (panel-packed, for fast batch verify)
  putStrLn "  Compiling packed GEMM kernels (panel-packed, for speculative verify)..."
  let pkCompile = compilePackedGemm arch
  pkqp  <- pkCompile dim   (kb dim)
  putStrLn $ "    PK-Q [B," ++ show dim ++ "→B," ++ show dim ++ "] OK"
  pkkp  <- pkCompile kvd   (kb dim)
  putStrLn $ "    PK-K [B," ++ show dim ++ "→B," ++ show kvd ++ "] OK"
  pkvp  <- pkCompile kvd   (kb dim)
  putStrLn $ "    PK-V [B," ++ show dim ++ "→B," ++ show kvd ++ "] OK"
  pkop  <- pkCompile dim   (kb dim)
  putStrLn $ "    PK-O [B," ++ show dim ++ "→B," ++ show dim ++ "] OK"
  pkgp  <- pkCompile hdim  (kb dim)
  putStrLn $ "    PK-Gate [B," ++ show dim ++ "→B," ++ show hdim ++ "] OK"
  pkup  <- pkCompile hdim  (kb dim)
  putStrLn $ "    PK-Up [B," ++ show dim ++ "→B," ++ show hdim ++ "] OK"
  pkdp  <- pkCompile dim   (kb hdim)
  putStrLn $ "    PK-Down [B," ++ show hdim ++ "→B," ++ show dim ++ "] OK"
  pkoutp <- pkCompile vocab (kb dim)
  putStrLn $ "    PK-Output [B," ++ show dim ++ "→B," ++ show vocab ++ "] OK"

  return KernelSet
    { ksQProj  = qp, ksKProj = kp, ksVProj = vp, ksOProj = op
    , ksGate   = gp, ksUp    = up, ksDown  = dp, ksOutput = outp
    , ksFusedLayer = fl
    , ksPfQProj = pqp, ksPfKProj = pkp, ksPfVProj = pvp, ksPfOProj = pop
    , ksPfGate  = pgp, ksPfUp    = pup, ksPfDown  = pdp, ksPfOutput = poutp
    , ksPkQProj = pkqp, ksPkKProj = pkkp, ksPkVProj = pkvp, ksPkOProj = pkop
    , ksPkGate  = pkgp, ksPkUp    = pkup, ksPkDown  = pkdp, ksPkOutput = pkoutp
    }

-- | Call a compiled matvec: out = W @ x
callMatvec :: CompiledMatvec -> Ptr Float -> Ptr Float -> Ptr Word8 -> IO ()
callMatvec cm outPtr xPtr wPtr =
  cmFn cm outPtr xPtr wPtr (fromIntegral (cmN cm)) (fromIntegral (cmKBlocks cm))
