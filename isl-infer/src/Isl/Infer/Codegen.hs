-- | Top-level C code generation from polyhedral loop nests.
--
-- Takes a 'CKernel' description (loop nest + body template + metadata)
-- and produces a complete C source file that can be compiled with
-- @gcc -O3 -march=native -fopenmp -shared -fPIC@.
module Isl.Infer.Codegen
  ( CKernel(..)
  , SomeLoopNest(..)
  , eraseLoopNest
  , eraseScanner
  , generateC
  , generateFunction
  ) where

import Data.Set (Set)
import Isl.Scan.Types (LoopLevel, LoopNest(..), Scanner(..))
import Isl.Infer.Codegen.Loop (loopNestToC)

-- | A kernel ready for C code generation.
--
-- The loop nests provide the iteration structure (from the scanner);
-- the body provides the innermost computation (operator-specific).
data CKernel = CKernel
  { ckName         :: String           -- ^ C function name
  , ckIncludes     :: [String]         -- ^ @#include@ directives
  , ckTypedefs     :: String           -- ^ Type definitions (structs, etc.)
  , ckMacros       :: [(String, String)]  -- ^ @#define name value@
  , ckReturnType   :: String           -- ^ Return type (usually "void")
  , ckFuncParams   :: [(String, String)]  -- ^ @(type, name)@ for function params
  , ckDimNames     :: [String]         -- ^ Variable names for loop dimensions
  , ckParamNames   :: [String]         -- ^ Variable names for polyhedron parameters
  , ckParallelDims :: Set Int          -- ^ Dimension indices with @#pragma omp parallel for@
  , ckSimdDims     :: Set Int          -- ^ Dimension indices with @#pragma omp simd@ (empty = auto-innermost)
  , ckPreamble     :: String           -- ^ Code before the loop nest
  , ckBody         :: String           -- ^ Innermost loop body (C code)
  , ckPostamble    :: String           -- ^ Code after the loop nest
  , ckLoopNests    :: [SomeLoopNest]   -- ^ One or more loop nests (union of convex pieces)
  }

-- | Type-erased loop nest (we only need the levels, not the phantom types).
data SomeLoopNest = SomeLoopNest
  { slnLevels :: [LoopLevel]
  , slnParams :: Int
  , slnDims   :: Int
  }

-- | Erase phantom types from a 'LoopNest'.
eraseLoopNest :: LoopNest ps n -> SomeLoopNest
eraseLoopNest ln = SomeLoopNest
  { slnLevels = lnLevels ln
  , slnParams = lnParams ln
  , slnDims   = lnDims ln
  }

-- | Erase phantom types from all loop nests in a 'Scanner'.
eraseScanner :: Scanner ps n -> [SomeLoopNest]
eraseScanner (Scanner nests) = map eraseLoopNest nests

-- | Generate a complete C source file from a 'CKernel'.
generateC :: CKernel -> String
generateC ck = unlines $
  -- Includes
  [ "#include <stdint.h>"
  , "#include <math.h>"
  ]
  ++ map (\h -> "#include " ++ h) (ckIncludes ck)
  ++ [""]
  -- Macros
  ++ [ "#define ISL_CEIL_DIV(a, b) (((a) >= 0) ? (((a) + (b) - 1) / (b)) : -(-(a) / (b)))"
     , "#define ISL_FLOOR_DIV(a, b) (((a) >= 0) ? ((a) / (b)) : -((-(a) + (b) - 1) / (b)))"
     , "#define ISL_MAX(a, b) ((a) > (b) ? (a) : (b))"
     , "#define ISL_MIN(a, b) ((a) < (b) ? (a) : (b))"
     ]
  ++ map (\(n, v) -> "#define " ++ n ++ " " ++ v) (ckMacros ck)
  ++ [""]
  -- Typedefs
  ++ (if null (ckTypedefs ck) then [] else [ckTypedefs ck, ""])
  -- Function
  ++ [generateFunction ck]

-- | Generate just the function (no includes/macros).
generateFunction :: CKernel -> String
generateFunction ck = unlines $
  [ ckReturnType ck ++ " " ++ ckName ck ++ "("
    ++ renderParams (ckFuncParams ck) ++ ") {"
  ]
  ++ (if null (ckPreamble ck) then [] else [ckPreamble ck])
  ++ concatMap (emitOneNest ck) (ckLoopNests ck)
  ++ (if null (ckPostamble ck) then [] else [ckPostamble ck])
  ++ ["}"]

emitOneNest :: CKernel -> SomeLoopNest -> [String]
emitOneNest ck sln =
  let fakeNest :: LoopNest '[] 0
      fakeNest = LoopNest
        { lnLevels = slnLevels sln
        , lnParams = slnParams sln
        , lnDims   = slnDims sln
        }
  in lines $ loopNestToC
       (ckDimNames ck)
       (ckParamNames ck)
       (ckParallelDims ck)
       (ckSimdDims ck)
       fakeNest
       (ckBody ck)

renderParams :: [(String, String)] -> String
renderParams [] = "void"
renderParams ps = go ps
  where
    go [] = ""
    go [x] = pair x
    go (x:xs) = pair x ++ ", " ++ go xs
    pair (t, n) = t ++ " " ++ n
