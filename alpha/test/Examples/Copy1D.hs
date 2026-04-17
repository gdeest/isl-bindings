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

-- | Minimal 1D SARE with an input and a Dep body.
--
-- @
-- y[i] = x[i]   on  { 0 <= i <= N-1 }
-- @
--
-- Unlike 'Examples.Zero1D', @y@'s body is a 'Dep'/'Var' composition
-- (not 'Const'), so 'Alpha.Lower.exprDomInfo' correctly recovers the
-- 1-dim iteration space.  Used by the post-contraction WAW regression
-- tests which need a non-trivial 1D iteration domain for the modular
-- storage map to exhibit aliasing.
module Examples.Copy1D
  ( copy1D
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))


-- | 1D line domain @{ i | 0 <= i <= N - 1 }@.
lineN :: DomExpr '["i"] _
lineN = range0 @"N" #i

-- | @y[i] = x[i]@ on a 1D line.
copy1D :: System '["N"] '[] _ _ _
copy1D = system
  ( Decls
      { dInputs  = input @"x" lineN (Proxy @Double) :> Nil
      , dOutputs = output @"y" lineN (Proxy @Double) :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"y" @'["i"] (at @"x" (ix1 #i))
   :& EqNil )
