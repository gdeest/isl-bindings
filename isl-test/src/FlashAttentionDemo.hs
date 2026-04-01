{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

-- | Flash attention: polyhedral verification + loop nest generation.
--
-- Full pipeline:
--   1. Define masks and tiling map at the TYPE LEVEL (single source of truth)
--   2. Plugin verifies relationships at compile time
--   3. Singletons bridge type-level → ISL objects at runtime
--   4. Apply tiling map to mask domain → tiled domain
--   5. Decompose ISL sets → PDisjunction → Scanner → parametric loop nests
--
-- No manual duplication: value-level domains are DERIVED from types.
module Main where

import Isl.TypeLevel
import GHC.TypeLits (KnownNat)
import Isl.HighLevel.Context (runIslT, Ur(..), IslT)
import qualified Isl.HighLevel.Set as SetOp
import qualified Isl.HighLevel.Map as MapOp
import Isl.HighLevel.MultiAff (MultiAff(..))
import Isl.HighLevel.Params (Length, KnownSymbols)
import qualified Isl.BasicMap as RawBM
import qualified Isl.Map as RawM
import Control.Monad.IO.Class (MonadIO)
import Unsafe.Coerce (unsafeCoerce)
import Isl.Scan (mkScanner, prettyScanner, scanFold, mkVec, Vec(..))

-- =========================================================================
-- Parameter index instances
-- =========================================================================

instance ParamIndex "S" where paramIndex = 0
instance ParamIndex "W" where paramIndex = 1

-- =========================================================================
-- Section 1: Attention Masks (type-level, the SINGLE source of truth)
-- =========================================================================

-- | Full attention: { (q, k) : 0 ≤ q ≤ S, 0 ≤ k ≤ S }
type FullMask =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 1)
   ]

-- | Causal attention: { (q, k) : 0 ≤ q ≤ S, 0 ≤ k ≤ q }
type CausalMask =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TDim (D 0)     >=. 'TDim (D 1)
   ]

-- | Sliding window: { (q, k) : 0 ≤ q ≤ S, max(0, q−W) ≤ k ≤ q }
type SlidingWindowMask =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TDim (D 0)     >=. 'TDim (D 1)
   , 'TDim (D 1)     >=. ('TDim (D 0) -. 'TParam (P "W"))
   ]

-- =========================================================================
-- Section 2: Tiling Transformation (type-level map)
-- =========================================================================

-- | Strip-mining transformation: (q, k) → (tq, tk, q, k)
-- where tq = floor(q/64), tk = floor(k/64).
--
-- Expressed as a multi-aff: each output is an affine function of inputs.
-- This is the ISL-native representation for affine maps with floor division.
type Tile64 =
  '[ 'TFloorDiv ('TDim (D 0)) ('Pos 64)    -- tq = floor(q/64)
   , 'TFloorDiv ('TDim (D 1)) ('Pos 64)    -- tk = floor(k/64)
   , 'TDim (D 0)                            -- q' = q
   , 'TDim (D 1)                            -- k' = k
   ]

-- | Projection: (tq, tk, q, k) → (q, k) — drops tile indices.
type ProjectQK =
  '[ 'TDim (D 2)    -- q (3rd dim of 4D input)
   , 'TDim (D 3)    -- k (4th dim of 4D input)
   ]

-- | Identity: (q, k) → (q, k)
type Identity2D =
  '[ 'TDim (D 0)
   , 'TDim (D 1)
   ]

-- =========================================================================
-- Section 3: Compile-Time Proofs
-- =========================================================================

-- Mask containment
proofCausalInFull :: IslSubset '["S","W"] 2 CausalMask FullMask => ()
proofCausalInFull = ()

proofWindowInCausal :: IslSubset '["S","W"] 2 SlidingWindowMask CausalMask => ()
proofWindowInCausal = ()

proofWindowInFull :: IslSubset '["S","W"] 2 SlidingWindowMask FullMask => ()
proofWindowInFull = ()

proofCausalNonEmpty :: IslNonEmpty '["S","W"] 2 CausalMask => ()
proofCausalNonEmpty = ()

-- Tiling round-trip: project(tile(q,k)) = (q,k)
-- Proves the tiling map is a refinement — no information lost when projecting
-- out tile indices. Computed at compile time by the plugin via ISL.
type TileRoundTrip = IslComposeMultiAff '["S"] 2 4 2 ProjectQK Tile64

proofTileRoundTrip :: IslMultiAffEqual '["S"] 2 2 TileRoundTrip Identity2D => ()
proofTileRoundTrip = ()
_useTileRoundTrip :: ()
_useTileRoundTrip = proofTileRoundTrip

proofWindowNonEmpty :: IslNonEmpty '["S","W"] 2 SlidingWindowMask => ()
proofWindowNonEmpty = ()

-- =========================================================================
-- Section 4: Bug Detection (uncomment _use* to trigger compile errors)
-- =========================================================================

-- BUG 1: Inequality backwards → attends to FUTURE tokens
type BrokenCausalMask =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 1)
   , 'TDim (D 1)     >=. 'TDim (D 0)           -- k ≥ q — WRONG
   ]

proofBrokenSubset :: IslSubset '["S","W"] 2 BrokenCausalMask CausalMask => ()
proofBrokenSubset = ()
-- _useBroken :: ()
-- _useBroken = proofBrokenSubset

-- BUG 2: Off-by-one — strict inequality excludes self-attention
type StrictCausalMask =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TDim (D 0)     >=. ('TDim (D 1) +. 'TConst ('Pos 1))
   ]

proofStrictEqualsCausal :: IslEqual '["S","W"] 2 StrictCausalMask CausalMask => ()
proofStrictEqualsCausal = ()
-- _useStrict :: ()
-- _useStrict = proofStrictEqualsCausal

-- BUG 3: Contradictory → empty mask
type EmptyMask =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TConst ('Pos 0) >=. 'TDim (D 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   ]

proofEmptyNonEmpty :: IslNonEmpty '["S","W"] 2 EmptyMask => ()
proofEmptyNonEmpty = ()
-- _useEmpty :: ()
-- _useEmpty = proofEmptyNonEmpty

-- =========================================================================
-- Singletons (type-level → ISL objects)
-- =========================================================================

causalSing :: SBasicSet '["S","W"] 2 CausalMask
causalSing = sBasicSet

windowSing :: SBasicSet '["S","W"] 2 SlidingWindowMask
windowSing = sBasicSet

-- Tiling multi-aff singleton: type-level Tile64 → ISL MultiAff
tileSing :: SMultiAff '["S"] 2 4 Tile64
tileSing = sMultiAff

-- Causal mask in the '["S"] param space (for applying the tiling map)
causalSingS :: SBasicSet '["S"] 2
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) >=. 'TDim (D 1)
   ]
causalSingS = sBasicSet

-- | Evaluate a type-level multi-aff singleton to an ISL Map.
-- MultiAff natively supports floor division (unlike BasicMap constraints).
multiAffToMap :: forall ps ni no es m.
  (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => SMultiAff ps ni no es -> IslT m (MapOp.Map ps ni no)
multiAffToMap sing = do
  MultiAff ma <- evalSMultiAff sing
  bm <- RawBM.fromMultiAff ma
  rawMap <- RawM.fromBasicMap bm
  -- Map ps ni no is a newtype over Isl.Map; safe coercion
  pure (unsafeCoerce rawMap)

-- =========================================================================
-- Main: verify, transform, generate, execute
-- =========================================================================

main :: IO ()
main = do
  putStrLn "=== Flash Attention: Polyhedral Verification + Loop Generation ==="

  -- ── Compile-time proofs ──────────────────────────────────────────────
  putStrLn ""
  putStrLn "Compile-time proofs (verified by ISL plugin):"
  putStrLn $ "  Causal ⊆ Full:    " ++ show proofCausalInFull
  putStrLn $ "  Window ⊆ Causal:  " ++ show proofWindowInCausal
  putStrLn $ "  Window ⊆ Full:    " ++ show proofWindowInFull
  putStrLn $ "  Causal non-empty: " ++ show proofCausalNonEmpty
  putStrLn $ "  Window non-empty: " ++ show proofWindowNonEmpty
  putStrLn $ "  Tile round-trip:  " ++ show proofTileRoundTrip
  putStrLn   "    ^ project(tile(q,k)) = (q,k) — tiling is a refinement"

  -- ── ISL domain strings ──────────────────────────────────────────────
  putStrLn ""
  putStrLn "Attention masks (from type-level singletons):"

  strs <- runIslT $ do
    cs <- evalSBasicSet causalSing
    ws <- evalSBasicSet windowSing

    (Ur cStr, cs') <- SetOp.borrowSet cs SetOp.setToString
    SetOp.freeSet cs'
    (Ur wStr, ws') <- SetOp.borrowSet ws SetOp.setToString
    SetOp.freeSet ws'

    -- Causal \ Window
    cs2 <- evalSBasicSet causalSing
    ws2 <- evalSBasicSet windowSing
    diff <- SetOp.subtract cs2 ws2
    (Ur dStr, diff') <- SetOp.borrowSet diff SetOp.setToString
    SetOp.freeSet diff'

    pure (Ur (cStr, wStr, dStr))

  let (cStr, wStr, dStr) = strs
  putStrLn $ "  Causal:           " ++ cStr
  putStrLn $ "  Sliding Window:   " ++ wStr
  putStrLn $ "  Causal \\ Window:  " ++ dStr

  -- ── Tiling transformation ───────────────────────────────────────────
  putStrLn ""
  putStrLn "Tiling transformation:"

  (tmStr, tiledStr) <- runIslT $ do
    tm <- multiAffToMap tileSing
    (Ur tmStr, tm') <- MapOp.borrowMap tm MapOp.mapToString

    cs <- evalSBasicSet causalSingS
    tiled <- MapOp.apply tm' cs
    (Ur tStr, tiled') <- SetOp.borrowSet tiled SetOp.setToString
    SetOp.freeSet tiled'

    pure (Ur (tmStr, tStr))
  putStrLn $ "  Tile64 map:       " ++ tmStr
  putStrLn $ "  Tile64(Causal):   " ++ tiledStr

  -- ── Parametric loop nests (derived from type-level via singletons) ──
  putStrLn ""
  putStrLn "Generated loop nests (derived from type-level definitions):"

  -- Derive scanners from type-level singletons:
  --   evalSBasicSet → Set → decomposeSet → PDisjunction → mkScanner
  -- No manual PConjunction duplication — value-level derived from types!
  (cDisj, wDisj, tDisj) <- runIslT $ do
    -- Causal (2D) — derived from CausalMask type
    cs <- evalSBasicSet causalSing
    (Ur cDisj, cs') <- SetOp.decomposeSet cs
    SetOp.freeSet cs'

    -- Sliding window (2D) — derived from SlidingWindowMask type
    ws <- evalSBasicSet windowSing
    (Ur wDisj, ws') <- SetOp.decomposeSet ws
    SetOp.freeSet ws'

    -- Tiled causal (4D): apply Tile64 map to CausalMask, then decompose.
    -- The scanner's FM elimination derives tq ≥ 0 from tk ≥ 0 ∧ tk ≤ tq.
    tm <- multiAffToMap tileSing
    cs2 <- evalSBasicSet causalSingS
    tiled <- MapOp.apply tm cs2
    (Ur tDisj, tiled') <- SetOp.decomposeSet tiled
    SetOp.freeSet tiled'

    pure (Ur (cDisj, wDisj, tDisj))

  let causalScanner = mkScanner cDisj
      windowScanner = mkScanner wDisj
      tiledScanner  = mkScanner tDisj

  putStrLn ""
  putStrLn "  ── Causal attention ──"
  putStrLn $ prettyScanner ["S", "W"] ["q", "k"] causalScanner

  putStrLn "  ── Sliding window attention ──"
  putStrLn $ prettyScanner ["S", "W"] ["q", "k"] windowScanner

  putStrLn "  ── Tiled causal (Tile64 ∘ CausalMask) ──"
  putStrLn $ prettyScanner ["S"] ["tq", "tk", "q", "k"] tiledScanner

  -- ── Concrete execution ──────────────────────────────────────────────
  let s = 255 :: Integer
      w = 63  :: Integer
  putStrLn $ "Point counts (S=" ++ show s ++ ", W=" ++ show w ++ "):"

  let cCount = scanFold causalScanner (mkVec @2 [s, w])
                 (\acc _ -> acc + 1) (0 :: Int)
  putStrLn $ "  Causal:          " ++ show cCount ++ " pairs"

  let wCount = scanFold windowScanner (mkVec @2 [s, w])
                 (\acc _ -> acc + 1) (0 :: Int)
  putStrLn $ "  Sliding window:  " ++ show wCount ++ " pairs"

  let tCount = scanFold tiledScanner (mkVec @1 [s])
                 (\acc _ -> acc + 1) (0 :: Int)
  putStrLn $ "  Tiled causal:    " ++ show tCount ++ " pairs"
    ++ if tCount == cCount
       then "  (matches — tiling is complete)"
       else "  WARNING: mismatch!"

  let totalTiles = ((s `div` 64) + 1) ^ (2 :: Integer)
      (nActive, _, _) = scanFold tiledScanner (mkVec @1 [s])
                           countTiles (0 :: Integer, -1, -1)
  putStrLn $ "  Tile skip ratio:  " ++ show (totalTiles - nActive)
    ++ "/" ++ show totalTiles ++ " tiles skipped"

  putStrLn ""
  putStrLn "All proofs verified. Uncomment _use* bindings to see errors."

countTiles :: (Integer, Integer, Integer) -> Vec 4 Integer -> (Integer, Integer, Integer)
countTiles (n, prevTq, prevTk) v =
  let [tq, tk, _, _] = toList v
  in if tq /= prevTq || tk /= prevTk
     then (n + 1, tq, tk)
     else (n, prevTq, prevTk)
