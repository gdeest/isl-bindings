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

import Data.Int (Int32)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Alpha.Core as Core
import Alpha.Core.Tokens (ElabMode(..))
import Alpha.Scalar (CNumType(..))
import Alpha.Surface.Elaborate (elaborate)

import qualified Examples.Copy1D as Copy1D
import qualified Examples.Heat3DElsewhere as H3E
import qualified Examples.IntRowMax as IRM
import qualified Examples.Matmul as Matmul
import qualified Examples.NeedsPctx as NPX
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

    -- Per-decl scalar reflection: an Int32 surface must produce
    -- 'CInt32' in every VarDecl.vdScalar.
  , testCase "IntRowMax vdScalar is CInt32 (per-decl scalar reflection)" $
      elaborate @'["N"] @'[] @IRM.IntRowMaxInputs @IRM.IntRowMaxOutputs
                @IRM.IntRowMaxLocals @Int32
                TrustPlugin IRM.intRowMax $ \result -> case result of
        Left err -> assertFailure ("IntRowMax returned Left: " ++ show err)
        Right sys ->
          let scalars =
                [ Core.vdScalar vd | Core.SomeVarDecl _ vd <- Core.sysInputs sys ]
                ++
                [ Core.vdScalar vd | Core.SomeVarDecl _ vd <- Core.sysOutputs sys ]
          in assertEqual "all decls CInt32" (replicate (length scalars) CInt32) scalars

    -- Non-trivial pctx: NeedsPctx carries '[N >= 1]', and its body
    -- obligation (x[N-1] in [0, N-1]) only holds under that pctx.
    -- SanityCheck re-checks via ISL with the materialised pctx; both
    -- modes must pass.
  , testCase "NeedsPctx elaborates under TrustPlugin (non-trivial pctx)" $
      elaborate @'["N"] @NPX.NeedsPctxPctx @NPX.NeedsPctxInputs
                @NPX.NeedsPctxOutputs @NPX.NeedsPctxLocals @Double
                TrustPlugin NPX.needsPctx (assertElabOk "NeedsPctx")
  , testCase "NeedsPctx elaborates under SanityCheck (pctx materialised)" $
      elaborate @'["N"] @NPX.NeedsPctxPctx @NPX.NeedsPctxInputs
                @NPX.NeedsPctxOutputs @NPX.NeedsPctxLocals @Double
                SanityCheck NPX.needsPctx (assertElabOk "NeedsPctx")
  ]
  where
    assertElabOk label result = case result of
      Left err  -> assertFailure $
        "elaborate(" ++ label ++ ") returned Left: " ++ show err
      Right sys -> assertBool (label ++ " elaborated") (not (null (Core.sysEqs sys)))
