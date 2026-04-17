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

-- | @A[i,j] = B[i+j]@ on the square @[0,N-1] × [0,N-1]@.
-- B is 1D over @[0, 2N-2]@.  Exercises evalBoundStr's
-- juxtaposition handling (#4): ISL's dim_max emits @"2N - 2"@
-- (or @"-2 + 2N"@) for B's upper bound, which the pre-fix parser
-- under-allocated.
module Examples.SumIndex2DSquare (sumIndex2DSquare) where

import Data.Proxy (Proxy(..))

import Alpha.Surface


-- @{ (i,j) | 0 <= i < N, 0 <= j < N }@
squareN :: DomExpr '["i", "j"] _
squareN = range0 @"N" #i /\ range0 @"N" #j

-- @{ k | 0 <= k <= 2N - 2 }@  — written as @N + N - 2@ at the surface,
-- ISL normalises to @"2N - 2"@ in its printed form.
lineDoubleN :: DomExpr '["k"] _
lineDoubleN = (#k .>=. lit @0)
           /\ (#k .<=. ((par @"N" +. par @"N") -. lit @2))

sumIndex2DSquare :: System '["N"] '[] _ _ _
sumIndex2DSquare = system
  ( Decls
      { dInputs  = input @"B" lineDoubleN (Proxy @Double) :> Nil
      , dOutputs = output @"A" squareN (Proxy @Double) :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"A" @'["i", "j"]
      (at @"B" (ix1 (#i +. #j)))
   :& EqNil )
