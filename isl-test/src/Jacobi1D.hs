{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Jacobi 1D stencil with time-skewed schedule.
--
-- Stencil: new[i] = (old[i-1] + old[i] + old[i+1]) / 3
--
-- Two statements over T time steps, N interior points:
--   S0[t,i]: compute new[i] from 3-point stencil
--   S1[t,i]: copy new[i] back to old[i]
--
-- Skewed schedule (wavefront parallelism):
--   S0[t,i] -> [t+i, t, 0]
--   S1[t,i] -> [t+i, t, 1]
--
-- The wavefront dimension t+i groups independent computations;
-- within a wavefront, time steps are ordered; S0 precedes S1.
module Main where

import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.IO (IOUArray)

import Isl.HighLevel.Context
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.Linear as Isl
import Isl.Scan

-- Params: N=0, T=1 (alphabetical)

s0Domain :: NamedSet
s0Domain = mkNamedPConjunction @"S0" @'["N","T"] @2 $
  \(np :- tp :- Nil) (t :- i :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np

s1Domain :: NamedSet
s1Domain = mkNamedPConjunction @"S1" @'["N","T"] @2 $
  \(np :- tp :- Nil) (t :- i :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np

-- Schedule: t outermost, then statement selector (all S0s before all S1s
-- within each time step), then spatial index.
--
--   S0[t,i] -> [t, 0, i]   -- compute all new values first
--   S1[t,i] -> [t, 1, i]   -- then copy all back to old
--
-- This respects the Jacobi data dependencies: within one time step,
-- all stencil reads use old values (S0 phase), then all writes update
-- old in the copy-back phase (S1 phase).

s0Sched :: NamedMap
s0Sched = mkNamedPMapConjunction @"S0" @'["N","T"] @2 @3 $
  \_ (t :- i :- Nil) (tt :- s :- ii :- Nil) ->
    idx tt ==: idx t
    &&: idx s ==: cst 0
    &&: idx ii ==: idx i

s1Sched :: NamedMap
s1Sched = mkNamedPMapConjunction @"S1" @'["N","T"] @2 @3 $
  \_ (t :- i :- Nil) (tt :- s :- ii :- Nil) ->
    idx tt ==: idx t
    &&: idx s ==: cst 1
    &&: idx ii ==: idx i

main :: IO ()
main = do
  let nSize = 8   -- interior points (indices 1..N)
      tSteps = 4  -- time steps

  -- Initial condition: old[0..N+1], boundaries fixed at 0
  let initArray = [0.0 :: Double] ++ replicate nSize 1.0 ++ [0.0]

  -- Build multi-scanner via ISL
  let (nsets, nmaps) = runIsl $ Isl.do
        s0us <- US.toUnionSetFromNamed s0Domain
        s0um <- UM.toUnionMapFromNamed s0Sched
        s0time <- UM.applyToSet s0us s0um
        (Ur ns0, s0t') <- US.decomposeUnionSetNamed s0time
        US.freeUnionSet s0t'

        s1us <- US.toUnionSetFromNamed s1Domain
        s1um <- UM.toUnionMapFromNamed s1Sched
        s1time <- UM.applyToSet s1us s1um
        (Ur ns1, s1t') <- US.decomposeUnionSetNamed s1time
        US.freeUnionSet s1t'

        s0um2 <- UM.toUnionMapFromNamed s0Sched
        s0inv <- UM.reverse s0um2
        (Ur nm0, s0i') <- UM.decomposeUnionMapNamed s0inv
        UM.freeUnionMap s0i'

        s1um2 <- UM.toUnionMapFromNamed s1Sched
        s1inv <- UM.reverse s1um2
        (Ur nm1, s1i') <- UM.decomposeUnionMapNamed s1inv
        UM.freeUnionMap s1i'

        let tag name nsl = [ns { nsName = Just name } | ns <- nsl]
        Isl.pure (Ur (tag "S0" ns0 ++ tag "S1" ns1, nm0 ++ nm1))

  let ms = mkMultiScannerFromNamed @2 nsets nmaps

  -- Print merged loop nest
  putStrLn "=== Jacobi 1D — phase-separated schedule ==="
  putStrLn $ prettyMultiScanner ["t", "s", "i"] ms

  -- Params: N=0, T=1 (alphabetical)
  let params = mkVec @2 [fromIntegral nSize, fromIntegral tSteps]

  -- Execute with the scanner
  old <- newListArray (0, nSize + 1) initArray :: IO (IOUArray Int Double)
  new <- newListArray (0, nSize + 1) initArray :: IO (IOUArray Int Double)

  scanMultiForM_ ms params $ \pt -> do
    let [t, i] = map fromIntegral (spOrigCoord pt)
    case spStmt pt of
      "S0" -> do
        a <- readArray old (i - 1)
        b <- readArray old i
        c <- readArray old (i + 1)
        writeArray new i ((a + b + c) / 3.0)
      "S1" -> do
        v <- readArray new i
        writeArray old i v
      _ -> return ()

  -- Read result
  putStrLn $ "\nAfter " ++ show tSteps ++ " Jacobi steps on " ++ show nSize ++ " interior points:"
  result <- mapM (readArray old) [0 .. nSize + 1]
  putStrLn $ "  " ++ show (map (roundTo 4) result)

  -- Naive implementation for verification
  naiveOld <- newListArray (0, nSize + 1) initArray :: IO (IOUArray Int Double)
  naiveNew <- newListArray (0, nSize + 1) initArray :: IO (IOUArray Int Double)
  mapM_ (\_ -> do
    mapM_ (\i -> do
      a <- readArray naiveOld (i - 1)
      b <- readArray naiveOld i
      c <- readArray naiveOld (i + 1)
      writeArray naiveNew i ((a + b + c) / 3.0)
      ) [1 .. nSize]
    mapM_ (\i -> do
      v <- readArray naiveNew i
      writeArray naiveOld i v
      ) [1 .. nSize]
    ) [1 .. tSteps]

  naiveResult <- mapM (readArray naiveOld) [0 .. nSize + 1]
  putStrLn $ if map (roundTo 8) result == map (roundTo 8) naiveResult
    then "Result matches naive implementation."
    else "MISMATCH!\n  naive: " ++ show (map (roundTo 4) naiveResult)

  -- Show first few scan points
  putStrLn "\n=== First 15 scan points ==="
  let points = scanMulti ms params
  mapM_ (\pt -> putStrLn $ "  " ++ spStmt pt
                ++ " (t=" ++ show (spOrigCoord pt !! 0)
                ++ ",i=" ++ show (spOrigCoord pt !! 1) ++ ")"
                ++ " @ time=" ++ show (spTimeCoord pt))
        (take 15 points)
  putStrLn $ "  ... (" ++ show (length points) ++ " total)"

  -- Cross-check
  putStrLn $ if scanMulti ms params == scanMultiPQ ms params
    then "Linear and PQ scanners agree."
    else "LINEAR/PQ MISMATCH!"

roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n
