{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Pretty-printing for scanner types.
--
-- These functions render loop nests in a human-readable pseudo-code
-- format, useful for debugging and verifying scanner construction.
module Isl.Scan.Pretty
  ( prettyScanner
  , prettyLoopNest
  , prettyBound
  ) where

import Data.List (intercalate)
import GHC.TypeLits (Symbol, Nat)

import Isl.Scan.Types

-- | Pretty-print a scanner as pseudo-code loop nests.
--
-- @paramNames@ and @dimNames@ provide human-readable names for
-- parameters and dimensions respectively.
--
-- For non-convex scanners (multiple loop nests), each nest is
-- printed separately with a header.
prettyScanner :: [String] -> [String] -> Scanner ps n -> String
prettyScanner paramNames dimNames (Scanner nests) =
  case nests of
    [nest] -> prettyLoopNest paramNames dimNames nest
    _      -> unlines $ zipWith prettyOne [1 :: Int ..] nests
  where
    prettyOne i nest =
      "-- polyhedron " ++ show i ++ ":\n" ++ prettyLoopNest paramNames dimNames nest

-- | Pretty-print a single loop nest as pseudo-code.
prettyLoopNest :: [String] -> [String] -> LoopNest ps n -> String
prettyLoopNest paramNames dimNames nest =
  unlines $ concatMap (prettyLevel paramNames dimNames) (zip [0..] (lnLevels nest))
  ++ [replicate (2 * length (lnLevels nest)) ' '
     ++ "body(" ++ intercalate ", " dimNames ++ ")"]

prettyLevel :: [String] -> [String] -> (Int, LoopLevel) -> [String]
prettyLevel paramNames dimNames (depth, level) =
  let indent = replicate (2 * depth) ' '
      dimName = safeLookup dimNames (llDim level) ("d" ++ show (llDim level))
  in case llEquality level of
       Just eq ->
         [indent ++ "let " ++ dimName ++ " = "
          ++ prettyBound paramNames dimNames eq]
       Nothing ->
         let loBounds = map (prettyBound paramNames dimNames) (llLowerBounds level)
             hiBounds = map (prettyBound paramNames dimNames) (llUpperBounds level)
             lo = case loBounds of
                    [b] -> b
                    bs  -> "max(" ++ intercalate ", " bs ++ ")"
             hi = case hiBounds of
                    [b] -> b
                    bs  -> "min(" ++ intercalate ", " bs ++ ")"
             strideStr = if llStride level == 1 then ""
                         else " step " ++ show (llStride level)
         in [indent ++ "for " ++ dimName ++ " = " ++ lo
             ++ " to " ++ hi ++ strideStr ++ ":"]

-- | Pretty-print a single affine bound.
prettyBound :: [String] -> [String] -> AffineBound -> String
prettyBound paramNames dimNames (AffineBound loopCs paramCs constant divisor) =
  let loopTerms  = [prettyTerm c (safeLookup dimNames i ("d" ++ show i))
                    | (c, i) <- loopCs]
      paramTerms = [prettyTerm c (safeLookup paramNames i ("p" ++ show i))
                    | (c, i) <- paramCs]
      constTerm  = if constant == 0 then [] else [show constant]
      allTerms   = loopTerms ++ paramTerms ++ constTerm
      numerator  = if null allTerms then "0"
                   else formatTerms allTerms
  in if divisor == 1
     then numerator
     else "floor((" ++ numerator ++ ")/" ++ show divisor ++ ")"

prettyTerm :: Integer -> String -> String
prettyTerm 1 name    = name
prettyTerm (-1) name = "-" ++ name
prettyTerm c name    = show c ++ "*" ++ name

-- | Format a list of terms with proper sign handling.
formatTerms :: [String] -> String
formatTerms [] = "0"
formatTerms (t:ts) = t ++ concatMap addSign ts
  where
    addSign s@('-':_) = " " ++ s
    addSign s         = " + " ++ s

safeLookup :: [a] -> Int -> a -> a
safeLookup xs i def
  | i >= 0 && i < length xs = xs !! i
  | otherwise = def
