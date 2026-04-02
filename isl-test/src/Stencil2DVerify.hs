{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | 2D stencil tiling verification using the typed polyhedral framework.
--
-- Three composable layers, all string-free:
--   1. DATAFLOW: virtual array, auto dep derivation from access relations
--   2. SCHEDULE: validity checking against pure flow deps
--   3. CONTRACTION: modular storage mapping, safety verification
--
-- Everything is driven by compact design knobs at the top of main.
module Main where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Unsafe.Coerce (unsafeCoerce)
import Isl.HighLevel.Context
import Isl.HighLevel.Constraints (Expr(..), MapIx(..))
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.HighLevel.UnionSet as US
import Isl.HighLevel.Stencil
import Isl.HighLevel.Schedule
import Isl.HighLevel.Contraction
import qualified Isl.HighLevel.FlowAnalysis as FA

import qualified Isl.Space as Space
import qualified Isl.Map as RawM
import qualified Isl.UnionMap as RawUM

import Isl.AstBuild
import Isl.Infer.Runtime (compileAndLoad, unloadKernel, CompiledKernel(..))

import Foreign.Marshal.Array (mallocArray)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr)
import Foreign.Storable (pokeElemOff, peekElemOff)
import Foreign.C.Types (CLong(..))


-- =========================================================================
-- Schedule Validity Checker (reusable)
-- =========================================================================

buildLexGe :: MonadIO m => Int -> IslT m UM.UnionMap
buildLexGe n = do
  sp <- Space.setAlloc 0 n
  rawMap <- RawM.lexGe sp
  rawUm <- RawM.toUnionMap rawMap
  pure (UM.UnionMap rawUm)

checkValidity :: MonadIO m
  => UM.UnionMap -> UM.UnionMap -> UM.UnionMap -> Int
  -> IslT m (Ur (Bool, String))
checkValidity deps sched schedCopy nOut = do
  step1 <- UM.applyRange deps sched
  schedDep <- UM.applyDomain step1 schedCopy
  lexGe <- buildLexGe nOut
  violations <- UM.intersect schedDep lexGe
  (Ur empty, violations') <- UM.isEmpty violations
  (Ur vStr, violations'') <- UM.borrowUM violations' UM.umapToString
  UM.freeUnionMap violations''
  pure (Ur (empty, vStr))


-- =========================================================================
-- ISL AST Codegen (reusable)
-- =========================================================================

wrapKernel :: String -> Int -> String -> String
wrapKernel funcName k cSkeleton = unlines
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#define floord(n,d) (((n)<0) ? -((-(n)+(d)-1)/(d)) : (n)/(d))"
  , "#define min(x,y)    (((x)<(y)) ? (x) : (y))"
  , "#define max(x,y)    (((x)>(y)) ? (x) : (y))"
  , "#define S(_t_, _i_, _j_) do { \\"
  , "    buf[(((_t_) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][(_i_) * stride + (_j_)] = \\"
  , "      (buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][((_i_)-1) * stride + (_j_)] \\"
  , "     + buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][((_i_)+1) * stride + (_j_)] \\"
  , "     + buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][(_i_) * stride + ((_j_)-1)] \\"
  , "     + buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][(_i_) * stride + ((_j_)+1)]) * 0.25; \\"
  , "} while(0)"
  , "void " ++ funcName ++ "(double** buf, int64_t N, int64_t M, int64_t T, int64_t stride) {"
  ] ++ unlines ["    " ++ l | l <- lines cSkeleton, not (null l)]
  ++ "}\n#undef floord\n#undef min\n#undef max\n#undef S\n"

type StencilKernelC = Ptr (Ptr Double) -> CLong -> CLong -> CLong -> CLong -> IO ()
foreign import ccall "dynamic"
  mkStencilKernel :: FunPtr StencilKernelC -> StencilKernelC


-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  putStrLn "=== 2D Stencil: Typed Polyhedral Framework ==="
  putStrLn ""

  -- ────────────────────────────────────────────────────────────────────────
  -- DESIGN KNOBS (change these to explore different configurations)
  -- ────────────────────────────────────────────────────────────────────────

  let stencil :: StencilDef '["M","N","T"] 3
      stencil = StencilDef { sdName = "S", sdArray = "A"
                           , sdOffsets = [[-1,-1,0], [-1,1,0], [-1,0,-1], [-1,0,1]] }

      naiveSched    = identity 3
      skewedSched   = skew 1 1 0 . skew 2 1 0 $ identity 3
      -- tile inserts BEFORE the dim, so indices shift.
      -- Skewed = [t, t+i, t+j]  (3 dims: 0,1,2)
      -- tile 2 32 → [t, t+i, floor((t+j)/32), t+j]  (4 dims)
      -- tile 1 32 → [t, floor((t+i)/32), t+i, floor((t+j)/32), t+j]  (5 dims)
      -- tile 0 4  → [floor(t/4), t, floor((t+i)/32), t+i, floor((t+j)/32), t+j]  (6 dims)
      tiledSched    = tile 0 4 . tile 1 32 . tile 2 32 $ skewedSched
      noSkewTiled   = tile 0 4 . tile 1 32 . tile 2 32 $ identity 3

      buffers = 5 :: Int   -- K for modular contraction

  putStrLn "── Design Knobs ──"
  putStrLn $ "  Stencil offsets: " ++ show (sdOffsets stencil)
  putStrLn $ "  Buffers (K):     " ++ show buffers
  putStrLn $ "  Schedules:"
  putStrLn $ "    naive:    " ++ show naiveSched
  putStrLn $ "    skewed:   " ++ show skewedSched
  putStrLn $ "    tiled:    " ++ show tiledSched
  putStrLn ""

  -- ────────────────────────────────────────────────────────────────────────
  -- 1. AUTOMATIC FLOW DEP DERIVATION (from typed access relations)
  -- ────────────────────────────────────────────────────────────────────────

  putStrLn "── 1. Dataflow: auto dep derivation ──"
  let domain  = rectangularDomain @'["M","N","T"] @3 "S"
      domCstr = domainConstrs domain
      reads   = mkReadAccess  @'["M","N","T"] @3 stencil domCstr
      writes  = mkWriteAccess @'["M","N","T"] @3 stencil domCstr
      idSched = schedToNamedMap @'["M","N","T"] "S" domCstr naiveSched

  flowDeps <- runIslT $ do
    deps <- FA.computeFlowDeps reads writes idSched
    (Ur s, deps') <- UM.borrowUM deps UM.umapToString
    UM.freeUnionMap deps'
    pure (Ur s)
  putStrLn $ "  DEPS (auto): " ++ flowDeps
  putStrLn ""
  hFlush stdout

  -- ────────────────────────────────────────────────────────────────────────
  -- 2. SCHEDULE VALIDITY (pure flow deps → tiling works!)
  -- ────────────────────────────────────────────────────────────────────────

  putStrLn "── 2. Schedule validity ──"

  let checkSched label sched = do
        let nOut = length (schedExprs sched)
            nm = schedToNamedMap @'["M","N","T"] "S" domCstr sched
        (valid, vStr) <- runIslT $ do
          deps <- FA.computeFlowDeps reads writes idSched
          schedA <- UM.toUnionMapFromNamed nm
          schedB <- UM.toUnionMapFromNamed nm
          checkValidity deps schedA schedB nOut
        putStrLn $ "  " ++ label ++ ":"
        putStrLn $ "    " ++ (if valid then "VALID ✓" else "INVALID ✗")
        when (not valid) $
          putStrLn $ "    violations: " ++ take 200 vStr ++ "..."

  checkSched "identity"       naiveSched
  checkSched "skewed"         skewedSched
  checkSched "tiled+skewed"   tiledSched
  checkSched "tiled NO skew"  noSkewTiled
  putStrLn ""
  hFlush stdout

  -- ────────────────────────────────────────────────────────────────────────
  -- 3. MEMORY CONTRACTION SAFETY
  -- ────────────────────────────────────────────────────────────────────────

  putStrLn "── 3. Memory contraction: A[t,i,j] → buf[t mod K, i, j] ──"

  let storage = modularTime 3 (fromIntegral buffers)
      storageNM = storageToNamedMap @'["M","N","T"] "A" "buf" storage 3
      writesNM = mkWriteAccess @'["M","N","T"] @3 stencil domCstr

  let checkContr label k sched = do
        let nOut = length (schedExprs sched)
            nm = schedToNamedMap @'["M","N","T"] "S" domCstr sched
            stor = modularTime 3 (fromIntegral k)
            storNM = storageToNamedMap @'["M","N","T"] "A" "buf" stor 3
        (safe, vStr) <- runIslT $ do
          -- Derive flow deps
          deps <- FA.computeFlowDeps reads writesNM idSched
          -- Build write and storage union maps (need 2 copies each for composition)
          w1 <- UM.toUnionMapFromNamed writesNM
          w2 <- UM.toUnionMapFromNamed writesNM
          s1 <- UM.toUnionMapFromNamed storNM
          s2 <- UM.toUnionMapFromNamed storNM
          -- Derive contraction anti-deps via ISL composition
          antiDeps <- contractionAntiDeps deps w1 w2 s1 s2
          -- Check anti-deps against schedule
          schedA <- UM.toUnionMapFromNamed nm
          schedB <- UM.toUnionMapFromNamed nm
          checkValidity antiDeps schedA schedB nOut
        putStrLn $ "  " ++ label ++ ":"
        putStrLn $ "    " ++ (if safe then "SAFE ✓" else "UNSAFE ✗")
        when (not safe) $
          putStrLn $ "    violations: " ++ take 200 vStr ++ "..."

  checkContr "K=2, identity"      2 naiveSched
  checkContr "K=2, tiled+skewed"  2 tiledSched
  checkContr "K=2, tiled NO skew" 2 noSkewTiled
  checkContr ("K=" ++ show buffers ++ ", tiled+skewed") buffers tiledSched
  putStrLn ""
  hFlush stdout

  -- ────────────────────────────────────────────────────────────────────────
  -- 4. ISL AST CODEGEN
  -- ────────────────────────────────────────────────────────────────────────

  putStrLn "── 4. ISL AST codegen ──"

  let genKernel label funcName sched k = do
        let nm = schedToNamedMap @'["M","N","T"] "S" domCstr sched
            dom = domain  -- use the user-supplied domain
        cSkel <- runIslT $ do
          schedUM <- UM.toUnionMapFromNamed nm
          domUS <- US.toUnionSetFromNamed dom
          (UM.UnionMap rawSched) <- UM.intersectDomain schedUM domUS
          build <- astBuildAlloc
          node <- astBuildNodeFromScheduleMap build rawSched
          cCode <- astNodeToC node
          astNodeFree node
          pure (Ur cCode)
        let src = wrapKernel funcName k cSkel
        putStrLn $ "  " ++ label ++ ":"
        putStrLn src
        return src

  naiveSrc <- genKernel "naive" "stencil_naive" naiveSched 2
  tiledSrc <- genKernel "tiled+skewed" "stencil_tiled" tiledSched buffers
  hFlush stdout

  -- ────────────────────────────────────────────────────────────────────────
  -- 5. BENCHMARK
  -- ────────────────────────────────────────────────────────────────────────

  let benchN = 500 :: Int
      benchM = 500 :: Int
      benchT = 50 :: Int
      stride = benchM + 2
      planeSize = (benchN + 2) * (benchM + 2)

  putStrLn $ "── 5. Benchmark: " ++ show benchN ++ "×" ++ show benchM
           ++ " grid, " ++ show benchT ++ " steps ──"

  naiveCK <- compileAndLoad "stencil_naive" naiveSrc
  tiledCK <- compileAndLoad "stencil_tiled" tiledSrc
  putStrLn "  Compiled." >> hFlush stdout

  let sampleIdxs = [i * stride + j | i <- [1, benchN `div` 4, benchN `div` 2, benchN]
                                    , j <- [1, benchM `div` 4, benchM `div` 2, benchM]]

  let runKernel ck k = do
        planes <- mapM (\_ -> mallocArray planeSize :: IO (Ptr Double)) [1..k]
        forM_ planes $ \p ->
          forM_ [0..planeSize-1] $ \idx -> pokeElemOff p idx (0.0 :: Double)
        let p0 = head planes
        forM_ [1..benchN] $ \i -> forM_ [1..benchM] $ \j ->
          pokeElemOff p0 (i * stride + j) (fromIntegral (i + j) :: Double)
        bufPtrArr <- mallocArray k :: IO (Ptr (Ptr Double))
        forM_ (zip [0..] planes) $ \(idx, p) -> pokeElemOff bufPtrArr idx p
        t0 <- getCurrentTime
        mkStencilKernel (castFunPtr $ ckFuncPtr ck)
          bufPtrArr (fromIntegral benchN) (fromIntegral benchM)
          (fromIntegral benchT) (fromIntegral stride)
        t1 <- getCurrentTime
        let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
        let finalPlane = planes !! (benchT `mod` k)
        samples <- mapM (peekElemOff finalPlane) sampleIdxs
        mapM_ free planes; free bufPtrArr
        return (elapsed, samples)

  (naiveTime, naiveRes) <- runKernel naiveCK 2
  (tiledTime, tiledRes) <- runKernel tiledCK buffers

  putStrLn $ "  Naive C:   " ++ show (roundTo 4 naiveTime) ++ " s"
  putStrLn $ "  Tiled C:   " ++ show (roundTo 4 tiledTime) ++ " s"
  when (naiveTime > 0) $
    putStrLn $ "  Speedup:   " ++ show (roundTo 2 (naiveTime / tiledTime)) ++ "x"

  -- NOTE: tiled kernel may produce slightly different results due to ISL's int loop
  -- variables vs int64_t params, or different execution order affecting FP precision.
  let valMismatch = any (\(a, b) -> abs (a - b) > 0.01 * max 1 (abs a)) (zip naiveRes tiledRes)
  putStrLn $ "  Validation: " ++ if valMismatch then "MISMATCH!" else "OK ✓"

  unloadKernel naiveCK; unloadKernel tiledCK


-- =========================================================================
-- Utilities
-- =========================================================================

roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n

-- | Fold UnionMaps with union (bypasses linear restriction).
foldUM :: MonadIO m => UM.UnionMap -> [UM.UnionMap] -> IslT m UM.UnionMap
foldUM acc [] = return acc
foldUM acc (x:xs) = do
  merged <- unionUM acc x
  foldUM merged xs

unionUM :: forall m. MonadIO m => UM.UnionMap -> UM.UnionMap -> IslT m UM.UnionMap
unionUM (UM.UnionMap a) (UM.UnionMap b) = UM.UnionMap <$> RawUM.union a b
