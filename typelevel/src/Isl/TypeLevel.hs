-- | Type-level polyhedral constraints for ISL.
--
-- This module re-exports everything needed to define polyhedra at the type
-- level and express proof obligations that the isl-plugin can solve at
-- compile time.
--
-- = Quick start
--
-- @
-- {\-# OPTIONS_GHC -fplugin=Isl.Plugin #-\}
--
-- type Triangle = 'TBasicSet' '["N"] 2
--   '[ 'TDim' 0  '>=.' 'TConst' (''Pos' 0)
--    , 'TDim' 0  '<=.' 'TParam' "N"
--    , 'TDim' 1  '>=.' 'TConst' (''Pos' 0)
--    , 'TDim' 1  '<=.' 'TDim' 0
--    ]
--
-- -- The plugin proves this at compile time:
-- proof :: 'IslNonEmpty' '["N"] 2 (Constraints Triangle) => ()
-- proof = ()
-- @
module Isl.TypeLevel
  ( -- * Type-level signed integers
    Z(..)
    -- * Type-level expressions
  , TExpr(..)
  , type (+.), type (-.), type (*.)
    -- * Type-level constraints
  , TConstraint(..)
  , type (>=.), type (<=.), type (==.)
    -- * Validation
  , ValidExpr, ValidConstraint, AllValid, AllValidCSS
  , CheckDim, CheckParam
    -- * Type-level polyhedra (validated)
  , TBasicSet(..)
  , TSet(..)
  , TBasicMap(..)
    -- * Proof obligations (solved by isl-plugin)
  , IslSubset
  , IslNonEmpty
  , IslEqual
  , IslDomainOf
    -- * Reification (type-level → value-level)
  , KnownZ(..)
  , ParamIndex(..)
  , ReifyExpr(..)
  , ReifyTConstraint(..)
  , ReifyTConstraints(..)
  , reifyBasicSet
  , reifyBasicMap
  ) where

import Isl.TypeLevel.Expr
import Isl.TypeLevel.Constraint
import Isl.TypeLevel.Sing
