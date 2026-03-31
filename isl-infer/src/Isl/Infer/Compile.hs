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
  , callMatvec
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)

import Isl.Infer.Model (LlamaConfig(..))
import Isl.Infer.Schedule
import Isl.Infer.Kernel.FusedLayer
import Isl.Infer.Kernel.GEMM
import Isl.Infer.Kernel.Elementwise (q8Matvec)

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
  }

-- | Compile all kernels for a model's dimensions.
--
-- Each distinct (N, K) shape gets its own specialized kernel.
-- The schedule can differ per shape (e.g., small KV projections
-- don't benefit from parallelism).
compileKernels :: LlamaConfig -> MatvecSchedule -> IO KernelSet
compileKernels cfg sch = do
  let dim   = lcDim cfg
      kvd   = lcKVDim cfg
      hdim  = lcHiddenDim cfg
      vocab = lcVocabSize cfg
      kb d  = d `div` 32  -- K blocks

      -- Choose schedule per shape: small matrices don't parallelize well
      schFor n
        | n < 256   = sch { schParallel = False, schName = schName sch ++ "_small" }
        | otherwise  = sch

  putStrLn $ "  Compiling kernels (schedule: " ++ schName sch ++ ")..."

  qp <- compileMatvec (schFor dim)   dim   (kb dim)
  putStr "    Q" >> putStrLn (" [" ++ show dim ++ "→" ++ show dim ++ "] OK")

  kp <- compileMatvec (schFor kvd)   kvd   (kb dim)
  putStr "    K" >> putStrLn (" [" ++ show dim ++ "→" ++ show kvd ++ "] OK")

  vp <- compileMatvec (schFor kvd)   kvd   (kb dim)
  putStr "    V" >> putStrLn (" [" ++ show dim ++ "→" ++ show kvd ++ "] OK")

  op <- compileMatvec (schFor dim)   dim   (kb dim)
  putStr "    O" >> putStrLn (" [" ++ show dim ++ "→" ++ show dim ++ "] OK")

  gp <- compileMatvec (schFor hdim)  hdim  (kb dim)
  putStr "    Gate" >> putStrLn (" [" ++ show dim ++ "→" ++ show hdim ++ "] OK")

  up <- compileMatvec (schFor hdim)  hdim  (kb dim)
  putStr "    Up" >> putStrLn (" [" ++ show dim ++ "→" ++ show hdim ++ "] OK")

  dp <- compileMatvec (schFor dim)   dim   (kb hdim)
  putStr "    Down" >> putStrLn (" [" ++ show hdim ++ "→" ++ show dim ++ "] OK")

  outp <- compileMatvec (schFor vocab) vocab (kb dim)
  putStr "    Output" >> putStrLn (" [" ++ show dim ++ "→" ++ show vocab ++ "] OK")

  -- Fused layer kernel
  putStrLn "    Fused layer kernel..."
  fl <- compileLayer cfg
  putStrLn "    Fused layer OK"

  return KernelSet
    { ksQProj  = qp, ksKProj = kp, ksVProj = vp, ksOProj = op
    , ksGate   = gp, ksUp    = up, ksDown  = dp, ksOutput = outp
    , ksFusedLayer = fl
    }

-- | Call a compiled matvec: out = W @ x
callMatvec :: CompiledMatvec -> Ptr Float -> Ptr Float -> Ptr Word8 -> IO ()
callMatvec cm outPtr xPtr wPtr =
  cmFn cm outPtr xPtr wPtr (fromIntegral (cmN cm)) (fromIntegral (cmKBlocks cm))
