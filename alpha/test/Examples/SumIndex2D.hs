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

-- | @A[i,j] = B[i+j]@ on the triangle @{ 0 ≤ i, 0 ≤ j, i+j ≤ N-1 }@.
-- B is 1D over @[0, N-1]@.  Exercises multi-term output-expr
-- reconstruction in the Dep subscript extractor: the reified map
-- equality is @OutDim 0 - InDim 0 - InDim 1 = 0@.
module Examples.SumIndex2D (sumIndex2D) where

import Data.Proxy (Proxy(..))

import Alpha.Surface


-- @{ (i,j) | 0 <= i, 0 <= j, i + j <= N - 1 }@
triN :: DomExpr '["i", "j"] _
triN = (#i .>=. lit @0)
    /\ (#j .>=. lit @0)
    /\ ((#i +. #j) .<=. (par @"N" -. lit @1))

lineN :: DomExpr '["k"] _
lineN = range0 @"N" #k

sumIndex2D :: System '["N"] '[] _ _ _
sumIndex2D = system
  ( Decls
      { dInputs  = input @"B" lineN (Proxy @Double) :> Nil
      , dOutputs = output @"A" triN (Proxy @Double) :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"A" @'["i", "j"]
      (at @"B" (ix1 (#i +. #j)))
   :& EqNil )
