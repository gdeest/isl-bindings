{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Smoke tests for the Phase A.3b Surface → Core.V2 elaborator and
-- the Core.V2 bridge.
--
-- Coverage:
--
--   * @elaborate@ on a well-formed 1D Surface system produces 'Right'.
--   * The elaborated system's totality count (inputs + outputs +
--     locals + eqs) matches the source system's declared shape.
--   * The 'toV2' round-trip preserves total-count fields.
--
-- Negative path note.  The task spec asks for an "intentionally
-- malformed Surface system; assert @Left (ImageSubsetFails …)@".
-- Surface's compile-time plugin obligations prevent constructing
-- such a system: an access-out-of-bounds is rejected at type-check
-- time, so there is no value-level fixture to pass through
-- 'elaborate' that would trigger a 'Left'.  The primitive-level
-- negative checker path (malformed map image → 'ImageSubsetFails')
-- is covered in "TokensSpec" ('checkImageSubset: identity on …
-- ⊄ [0..4]').  Phase B grows an untyped-surface API that admits
-- malformed systems; those negatives land then.
module ElaborateSpec (elaborateSpec) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Alpha.Core        as Core
import qualified Alpha.Core.V2     as V2
import           Alpha.Core.Bridge (toV2)
import           Alpha.Surface.Elaborate (elaborate)

import qualified Examples.Copy1D   as Copy1D


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Fixtures
-- ═══════════════════════════════════════════════════════════════════════

-- | Re-use the 1D copy system from Examples.Copy1D.  One input (@x@),
-- one output (@y = x[i]@), no locals, one equation.  Known to compile
-- — lifts as a proven-well-formed witness for the elaborator to
-- bridge.
copy1DSystem :: _
copy1DSystem = Copy1D.copy1D


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Helpers
-- ═══════════════════════════════════════════════════════════════════════

-- | Run @elaborate@ and return the resulting system's shape counts
-- (inputs, outputs, locals, equations).  The continuation's type is
-- rank-N in the system skolem; we collapse to counts so the caller
-- never names @sys@.
elaborateShape
  :: forall ps pctx ins outs locs a.
     _
  => Core.System ps pctx ins outs locs
  -> Either String (Int, Int, Int, Int)  -- (in, out, loc, eqs)
elaborateShape sys =
  elaborate @ps @pctx @ins @outs @locs @a sys $ \case
    Left err  -> Left (show err)
    Right sys' ->
      Right ( length (V2.sysInputs  sys')
            , length (V2.sysOutputs sys')
            , length (V2.sysLocals  sys')
            , length (V2.sysEqs     sys')
            )


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Tests
-- ═══════════════════════════════════════════════════════════════════════

elaborateSpec :: TestTree
elaborateSpec = testGroup "Alpha.Surface.Elaborate / Alpha.Core.Bridge"
  [ testCase "elaborate copy1D succeeds" $
      case elaborateShape @_ @_ @_ @_ @_ @Double copy1DSystem of
        Left err                 -> assertFailure ("elaborate: " ++ err)
        Right (nIn, nOut, nLoc, nEq) -> do
          assertEqual "input count"    1 nIn
          assertEqual "output count"   1 nOut
          assertEqual "local count"    0 nLoc
          assertEqual "equation count" 1 nEq

  , testCase "toV2 copy1D matches elaborate counts" $
      let shape = toV2 @_ @_ @_ @_ @_ @Double copy1DSystem $ \case
            Left err  -> Left (show err)
            Right sys ->
              Right ( length (V2.sysInputs  sys)
                    , length (V2.sysOutputs sys)
                    , length (V2.sysLocals  sys)
                    , length (V2.sysEqs     sys)
                    )
      in case shape of
           Left err -> assertFailure ("toV2: " ++ err)
           Right s  -> s @?= (1, 1, 0, 1)

  , testCase "toV2 . elaborate round-trip: shapes agree" $ do
      -- elaborate calls toV2; structurally this is a no-op composition
      -- but it exercises the CPS threading twice.  We assert the two
      -- independently-opened skolem universes produce the same shape.
      let r1 = elaborateShape @_ @_ @_ @_ @_ @Double copy1DSystem
          r2 = toV2 @_ @_ @_ @_ @_ @Double copy1DSystem $ \case
                 Left err -> Left (show err)
                 Right s  ->
                   Right ( length (V2.sysInputs  s)
                         , length (V2.sysOutputs s)
                         , length (V2.sysLocals  s)
                         , length (V2.sysEqs     s)
                         )
      r1 @?= r2
  ]
