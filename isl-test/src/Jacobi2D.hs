{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Jacobi 2D stencil with time-skewed schedule.
--
-- Stencil: new[i][j] = (old[i-1][j] + old[i+1][j] + old[i][j-1] + old[i][j+1] + old[i][j]) / 5
--
-- Two statements over T time steps, N×M interior grid:
--   S0[t,i,j]: compute from 5-point stencil
--   S1[t,i,j]: copy back
--
-- Skewed schedule (2D wavefront):
--   S0[t,i,j] -> [t+i, t+j, t, 0]
--   S1[t,i,j] -> [t+i, t+j, t, 1]
module Main where

import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)

import Isl.HighLevel.Context
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.Linear as Isl
import Isl.Scan

-- Params: M=0, N=1, T=2 (alphabetical)

s0Domain :: NamedSet
s0Domain = mkNamedPConjunction @"S0" @'["M","N","T"] @3 $
  \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np
    &&: idx j >=: cst 1 &&: idx j <=: idx mp

s1Domain :: NamedSet
s1Domain = mkNamedPConjunction @"S1" @'["M","N","T"] @3 $
  \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np
    &&: idx j >=: cst 1 &&: idx j <=: idx mp

-- Phase-separated schedule: all S0s (compute) before all S1s (copy) per time step.
--   S0[t,i,j] -> [t, 0, i, j]
--   S1[t,i,j] -> [t, 1, i, j]

s0Sched :: NamedMap
s0Sched = mkNamedPMapConjunction @"S0" @'["M","N","T"] @3 @4 $
  \_ (t :- i :- j :- Nil) (tt :- s :- ii :- jj :- Nil) ->
    idx tt ==: idx t
    &&: idx s ==: cst 0
    &&: idx ii ==: idx i
    &&: idx jj ==: idx j

s1Sched :: NamedMap
s1Sched = mkNamedPMapConjunction @"S1" @'["M","N","T"] @3 @4 $
  \_ (t :- i :- j :- Nil) (tt :- s :- ii :- jj :- Nil) ->
    idx tt ==: idx t
    &&: idx s ==: cst 1
    &&: idx ii ==: idx i
    &&: idx jj ==: idx j

main :: IO ()
main = do
  let nRows = 5  -- interior rows
      mCols = 6  -- interior cols
      tSteps = 3

  -- Grid (0..N+1) × (0..M+1), boundaries at 0
  old <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  new <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  -- Interior starts at 1.0
  mapM_ (\i -> mapM_ (\j -> do writeArray old (i,j) 1.0; writeArray new (i,j) 1.0) [1..mCols]) [1..nRows]

  -- Build multi-scanner
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

  let ms = mkMultiScannerFromNamed @3 nsets nmaps

  putStrLn "=== Jacobi 2D — skewed schedule ==="
  putStrLn $ prettyMultiScanner ["t", "s", "i", "j"] ms

  -- Params: M=0, N=1, T=2 (alphabetical)
  let params = mkVec @3 [fromIntegral mCols, fromIntegral nRows, fromIntegral tSteps]

  -- Execute
  scanMultiForM_ ms params $ \pt -> do
    let [t, i, j] = map fromIntegral (spOrigCoord pt)
    case spStmt pt of
      "S0" -> do
        a <- readArray old (i-1, j)
        b <- readArray old (i+1, j)
        c <- readArray old (i, j-1)
        d <- readArray old (i, j+1)
        e <- readArray old (i, j)
        writeArray new (i, j) ((a + b + c + d + e) / 5.0)
      "S1" -> do
        v <- readArray new (i, j)
        writeArray old (i, j) v
      _ -> return ()

  -- Read result
  putStrLn $ "\nAfter " ++ show tSteps ++ " Jacobi 2D steps on " ++ show nRows ++ "×" ++ show mCols ++ " grid:"
  mapM_ (\i -> do
    vals <- mapM (\j -> readArray old (i, j)) [0..mCols+1]
    putStrLn $ "  " ++ unwords (map (padLeft 7 . show . roundTo 3) vals)
    ) [0..nRows+1]

  -- Naive verification
  naiveOld <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  naiveNew <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  mapM_ (\i -> mapM_ (\j -> do writeArray naiveOld (i,j) 1.0; writeArray naiveNew (i,j) 1.0) [1..mCols]) [1..nRows]

  mapM_ (\_ -> do
    mapM_ (\i -> mapM_ (\j -> do
      a <- readArray naiveOld (i-1, j)
      b <- readArray naiveOld (i+1, j)
      c <- readArray naiveOld (i, j-1)
      d <- readArray naiveOld (i, j+1)
      e <- readArray naiveOld (i, j)
      writeArray naiveNew (i, j) ((a + b + c + d + e) / 5.0)
      ) [1..mCols]) [1..nRows]
    mapM_ (\i -> mapM_ (\j -> do
      v <- readArray naiveNew (i, j)
      writeArray naiveOld (i, j) v
      ) [1..mCols]) [1..nRows]
    ) [1..tSteps]

  -- Compare a few interior points
  mismatch <- or <$> sequence
    [ do a <- readArray old (i,j); b <- readArray naiveOld (i,j); return (roundTo 8 a /= roundTo 8 b)
    | i <- [1..nRows], j <- [1..mCols]]
  putStrLn $ if mismatch then "MISMATCH!" else "Result matches naive implementation."

  putStrLn $ if scanMulti ms params == scanMultiPQ ms params
    then "Linear and PQ scanners agree."
    else "LINEAR/PQ MISMATCH!"

  putStrLn $ "\n(" ++ show (length (scanMulti ms params)) ++ " total scan points)"

roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n

padLeft :: Int -> String -> String
padLeft w s = replicate (w - length s) ' ' ++ s
