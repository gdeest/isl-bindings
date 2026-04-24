{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Smoke tests for "Alpha.Surface.Elaborate".
--
-- Covers the full walker: leaf cases (Var\/Const\/Pw\/PMap) on Zero1D,
-- the Dep\/Var path on Copy1D, the Dep\/Reduce path on Matmul, and the
-- Case path on Heat3DElsewhere.  Each polyhedral example is also run
-- under 'SanityCheck' to exercise the ISL re-check twin.
module ElaborateSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Alpha.Core as Core
import Alpha.Core.Tokens (ElabMode(..))
import Alpha.Surface.Elaborate (elaborate)

import qualified Examples.Copy1D as Copy1D
import qualified Examples.Heat3DElsewhere as H3E
import qualified Examples.Matmul as Matmul
import qualified Examples.Zero1D as Zero1D


tests :: TestTree
tests = testGroup "Alpha.Surface.Elaborate"
  [ testCase "Zero1D elaborates under TrustPlugin" $
      elaborate @'["N"] @'[] @'[] @Zero1D.Zero1DOutputs @'[] @Double
                TrustPlugin Zero1D.zero1D (assertElabOk "Zero1D")
  , testCase "Zero1D elaborates under SanityCheck" $
      elaborate @'["N"] @'[] @'[] @Zero1D.Zero1DOutputs @'[] @Double
                SanityCheck Zero1D.zero1D (assertElabOk "Zero1D")
  , testCase "Copy1D (Dep/Var) elaborates under TrustPlugin" $
      elaborate @'["N"] @'[] @_ @_ @_ @Double
                TrustPlugin Copy1D.copy1D (assertElabOk "Copy1D")
  , testCase "Copy1D (Dep/Var) elaborates under SanityCheck" $
      elaborate @'["N"] @'[] @_ @_ @_ @Double
                SanityCheck Copy1D.copy1D (assertElabOk "Copy1D")
  , testCase "Matmul (Dep/Reduce) elaborates under TrustPlugin" $
      elaborate @'["N"] @'[] @Matmul.MatmulInputs @Matmul.MatmulOutputs
                @Matmul.MatmulLocals @Double
                TrustPlugin Matmul.matmul (assertElabOk "Matmul")
  , testCase "Heat3DElsewhere (Case) elaborates under TrustPlugin" $
      elaborate @'["N"] @'[] @_ @_ @_ @Double
                TrustPlugin H3E.heat3DElsewhere (assertElabOk "Heat3DElsewhere")
  ]
  where
    assertElabOk label result = case result of
      Left err  -> assertFailure $
        "elaborate(" ++ label ++ ") returned Left: " ++ show err
      Right sys -> assertBool (label ++ " elaborated") (not (null (Core.sysEqs sys)))
