-- | Type-level polyhedral DSL for ISL.
--
-- Define polyhedra at the type level, verify properties at compile time
-- via the isl-plugin, and evaluate to ISL objects at runtime via singletons.
--
-- = Quick start
--
-- @
-- {\-# OPTIONS_GHC -fplugin=Isl.Plugin #-\}
--
-- type Triangle =
--   '[ 'TDim (D 0)  '>=.' 'TConst ('Pos 0)
--    , 'TDim (D 0)  '<=.' 'TParam (P "N")
--    , 'TDim (D 1)  '>=.' 'TConst ('Pos 0)
--    , 'TDim (D 1)  '<=.' 'TDim (D 0)
--    ]
--
-- instance ParamIndex "N" where paramIndex = 0
--
-- -- The plugin proves this at compile time:
-- _ :: 'IslSubset' '["N"] 2 Triangle Rectangle => ()
-- _ = ()
--
-- -- Singleton-carrying value, auto-derived:
-- triangle :: 'SBasicSet' '["N"] 2 Triangle
-- triangle = 'sBasicSet'
--
-- -- Evaluate to ISL object:
-- main = runIslT $ do
--   s <- 'evalSBasicSet' triangle
--   ...
-- @
module Isl.TypeLevel
  ( -- * Type-level signed integers
    Z(..)
    -- * Bounded indices
  , Idx(..), PIdx(..)
  , D, P
    -- * Type-level expressions (space-indexed)
  , TExpr(..)
  , type (+.), type (-.), type (*.)
    -- * Type-level constraints (space-indexed)
  , TConstraint(..)
  , type (>=.), type (<=.), type (==.)
    -- * Validation
  , ValidExpr, AllValid, AllValidCSS, ValidConstraint
    -- * Type-level polyhedra
  , TBasicSet(..), TSet(..), TBasicMap(..), TMap(..)
    -- * Set proof obligations (plugin-solved)
  , IslSubset, IslNonEmpty, IslEqual, IslDomainOf
    -- * Map proof obligations (plugin-solved)
  , IslMapSubset, IslMapEqual, IslRangeOf, IslImageSubset
    -- * Type-level computations (plugin-rewritten)
  , IslIntersectSet, IslComplementSet, IslDifferenceSet
  , IslApply, IslDomainTF, IslRangeTF
  , IslCompose, IslReverseMap
  , IslProjectOut, IslFromString
  , IslToString, IslMapToString
    -- * Singletons
  , STExpr(..), STConstraint(..), STConstraints(..)
    -- * Auto-derivation
  , KnownZ(..), ParamIndex(..)
  , KnownExpr(..), KnownConstraint(..), KnownConstraints(..)
    -- * Singleton-carrying polyhedra
  , SBasicSet(..), sBasicSet
  , SBasicMap(..), sBasicMap
    -- * Evaluation
  , evalSBasicSet, evalSBasicMap
  ) where

import Isl.TypeLevel.Expr
import Isl.TypeLevel.Constraint
import Isl.TypeLevel.Sing
