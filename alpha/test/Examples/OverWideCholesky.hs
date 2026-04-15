{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Positive regression test for the v5.2 effective-domain rewrite.
--
-- @Alpha.Core.BCons@ types branch bodies at
-- @EffectiveDomTag d amb = d ∩ amb@, so declaring a branch that
-- extends outside the ambient is *semantically harmless* — the
-- effective domain is exactly what a well-formed branch would have
-- produced.  The retired 'IslBranchFit' check (D22) used to reject
-- these; the effective-domain rewrite accepts them.
--
-- This file exercises the positive path: a cholesky-shaped 'Case'
-- whose diagonal branch has an off-by-one upper bound
-- (@i <= N@ instead of @i <= N - 1@), paired with the ordinary
-- 'StrictLowerN' under ambient 'LowerTri'.  The branch body is
-- 'Const' 0 (no reads), so the test exercises only the 'Case'
-- partitions obligation — no 'IslImageSubsetD' cross-checks fire.
--
-- Partitions check (discharged by the plugin at compile time):
--
--   * Coverage: LowerTri ⊆ OverWideDiagN ∪ StrictLowerN.
--     @OverWideDiagN ⊇ DiagN@ and @StrictLowerN@ is the strict lower,
--     together covering 'LowerTri'.  ✓
--   * Disjointness (within LowerTri):
--     @(OverWideDiagN ∩ StrictLowerN) ∩ LowerTri@.
--     The two branches have @j = i@ vs @j ≤ i - 1@ — unsatisfiable.
--     Empty. ✓
--
-- If this module ever fails to compile, the effective-domain rewrite
-- has regressed and v5.2 needs revisiting.
module Examples.OverWideCholesky
  ( OverWideDiagN
  , overWideCaseExpr
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Core
  ( Branches(..)
  , Expr(..)
  , VarDecl(..)
  )
import Examples.Cholesky
  ( LowerTri
  , StrictLowerN
  )
import Isl.TypeLevel.Constraint
  ( TConstraint
  , type (>=.)
  , type (<=.)
  , type (==.)
  )
import Isl.TypeLevel.Expr
  ( D
  , P
  , TExpr(..)
  , Z(..)
  )
import Isl.TypeLevel.Reflection (DomTag(..))

-- Import Examples.Matmul for its 'ParamIndex "N"' orphan instance
-- (transitive side-effect import).
import Examples.Matmul ()


-- | Cholesky's diagonal with an off-by-one upper bound on @i@.
-- The legitimate 'Examples.Cholesky.DiagN' has @i <= N - 1@; this
-- has @i <= N@, which puts the point @(N, N)@ outside 'LowerTri' at
-- non-degenerate parameter values.  Under the v5.2 effective-domain
-- rewrite, the branch body sees @OverWideDiagN ∩ LowerTri = DiagN@,
-- so the extra point is invisible and the body is well-typed.
type OverWideDiagN =
  '[ 'TDim (D 1) ==. 'TDim (D 0)
   , 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. 'TParam (P "N")
   ] :: [TConstraint '["N"] 2]


-- | A minimal decl list — one output @L@ declared on 'LowerTri' —
-- so the positive test can stand alone without pulling in the rest
-- of 'Examples.Cholesky'.
type TestDecls =
  '[ 'VarDecl @'["N"] @"L" @2 @('Literal LowerTri) @Double ]


-- | A 2-branch 'Case' with one over-wide diagonal branch and one
-- well-formed strict-lower branch.  Bodies are 'Const' 0 so no
-- 'IslImageSubsetD' obligations fire.  The only obligation is
-- 'IslPartitionsD' on 'LowerTri' against
-- @'[OverWideDiagN, StrictLowerN]@.
overWideCaseExpr
  :: Expr '["N"] TestDecls 2 ('Literal LowerTri) Double
overWideCaseExpr =
  Case
    ( BCons (Proxy :: Proxy ('Literal OverWideDiagN))  (Const 0)
    ( BCons (Proxy :: Proxy ('Literal StrictLowerN))   (Const 0)
      BNil ))
