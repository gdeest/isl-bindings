{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-partial-type-signatures #-}

-- | Matmul with locally transposed B, via the 'introduce' transform.
--
-- @
-- Bt[j, k] = B[k, j]                      -- local copy (phase 0)
-- C[i, j]  = ∑_k A[i, k] * Bt[j, k]      -- matmul (phase 1)
-- @
--
-- The B transpose makes the inner‐loop access to Bt contiguous.
module Examples.MatmulTransposed
  ( matmulT
  ) where

import Examples.Matmul (matmul)
import Alpha.Transform.Introduce (introduce)
import Alpha.Transform.Types (TransformError(..))
import Isl.TypeLevel.Expr (D, TExpr(..))


-- The transpose map: Bt[j,k] → B[k,j], i.e. [D 1, D 0].
type TransposeMap = '[ 'TDim (D 1), 'TDim (D 0) ] :: [TExpr '["N"] 2]

-- Apply the introduce transform to the matmul system.
-- This adds local Bt[j,k] = B[k,j] and rewrites C's equation
-- to read Bt[j,k] instead of B[k,j].
matmulT :: _
matmulT = case introduce @"B" @"Bt" @_ @'["N"] @_ @TransposeMap matmul of
  Right sys -> sys
  Left err  -> error $ "matmulT: " ++ show err
