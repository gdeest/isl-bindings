{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Heat3D rewritten with 'caseWithElsewhere': 3 branches instead of 8.
-- The boundary (zero Dirichlet) is the elsewhere catch-all.
module Examples.Heat3DElsewhere (testElsewhereDom) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)

import Alpha.Surface
import Isl.TypeLevel (IslDifferenceSetU, IslToStringU)
import Isl.TypeLevel.Sing (KnownDisjunction, knownDisjunction, STDisjunction)

-- Re-use domain definitions from the original Heat3D
import Examples.Heat3D (H3Decls, TimeBox, T0, Interior)

-- ═══════════════════════════════════════════════════════════════════════
-- Domain expressions (same as Heat3D but only the ones we need)
-- ═══════════════════════════════════════════════════════════════════════

spaceBox :: DomExpr '["i", "j", "k"] _
spaceBox = range0 @"N" #i /\ range0 @"N" #j /\ range0 @"N" #k

timeBox :: DomExpr '["t", "i", "j", "k"] _
timeBox = range0 @"T" #t /\ range0 @"N" #i /\ range0 @"N" #j /\ range0 @"N" #k

t0Dom :: DomExpr '["t", "i", "j", "k"] _
t0Dom = #t .==. lit @0 /\ range0 @"N" #i /\ range0 @"N" #j /\ range0 @"N" #k

interiorDom :: DomExpr '["t", "i", "j", "k"] _
interiorDom =
  between (lit @1) (par @"T" -. lit @1) #t
  /\ between (lit @1) (par @"N" -. lit @2) #i
  /\ between (lit @1) (par @"N" -. lit @2) #j
  /\ between (lit @1) (par @"N" -. lit @2) #k


-- ═══════════════════════════════════════════════════════════════════════
-- The system: 3 branches instead of 8
-- ═══════════════════════════════════════════════════════════════════════

-- Direct test: does IslDifferenceSetU reduce for Heat3D domains?
type Boundary = IslDifferenceSetU '["N","T"] 4
  '[TimeBox]
  '[T0, Interior]

testElsewhereDom :: String
testElsewhereDom = symbolVal (Proxy @(IslToStringU '["N","T"] 4 Boundary))


-- Minimal 1D test: two branches + elsewhere
-- Domain: { [i] : 0 <= i <= N-1 }
-- Branch 1: { [i] : i = 0 }  →  42
-- Branch 2: { [i] : 1 <= i <= N-2 }  →  1
-- Elsewhere: { [i] : i = N-1 }  →  0

lineDom :: DomExpr '["i"] _
lineDom = range0 @"N" #i

leftPt :: DomExpr '["i"] _
leftPt = #i .==. lit @0

midDom :: DomExpr '["i"] _
midDom = between (lit @1) (par @"N" -. lit @2) #i

-- TODO: IslPartitionsU not being solved for caseWithElsewhere.
-- The plugin's classifyWanted matches IslPartitionsU but the solver
-- isn't being invoked. Needs investigation: the IslPartitionsD
-- instance dispatches to IslPartitionsU which is plugin-solved,
-- but GHC may not be presenting it as a wanted to the plugin.
--
-- heat3DElsewhere :: System '["N"] _ _ _
-- heat3DElsewhere = system
--   ( Decls
--       { dInputs  = Nil
--       , dOutputs = output @"x" lineDom (Proxy @Double) :> Nil
--       , dLocals  = Nil } )
--   ( def @"x" @'["i"]
--       (caseWithElsewhere $ elsewhere (litB 0)
--        $ when_ leftPt (litB 42) $ when_ midDom (litB 1) SBNil)
--   :& EqNil )
