{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unsafe escape hatch for code that cannot use 'IslT' (e.g. GHC plugins
-- operating in pure IO via unsafePerformIO). Prefer 'Isl.Linear' for
-- all normal usage.
--
-- Two kinds of escape hatch live here:
--
-- * 'consume', 'borrow', 'dup' — raw resource primitives that bypass
--   the linear monad's sequencing guarantees.
-- * The @*Ref@ constructors — re-exported so trusted code can coerce an
--   owned value to a borrowed ref without going through 'borrow'. The
--   caller is responsible for ensuring the owned outlives every use of
--   the ref. Normal user code should never import this module.
module Isl.Unsafe
  ( -- * Raw linear primitives
    consume
  , borrow
  , dup
    -- * Ref constructors (escape hatch)
  , AffRef(..), ValRef(..), IdRef(..)
  , SetRef(..), BasicSetRef(..), UnionSetRef(..)
  , MapRef(..), BasicMapRef(..), UnionMapRef(..)
  , ConstraintRef(..), SpaceRef(..), LocalSpaceRef(..)
  , PwAffRef(..), MultiAffRef(..), PwMultiAffRef(..), AffListRef(..)
  , AstBuildRef(..), AstNodeRef(..), AstExprRef(..), AstNodeListRef(..)
  ) where

import Isl.Types.Internal (Consumable(consume), Borrow(borrow), Dupable(dup))
import Isl.Types.Raw
  ( AffRef(..), ValRef(..), IdRef(..)
  , SetRef(..), BasicSetRef(..), UnionSetRef(..)
  , MapRef(..), BasicMapRef(..), UnionMapRef(..)
  , ConstraintRef(..), SpaceRef(..), LocalSpaceRef(..)
  , PwAffRef(..), MultiAffRef(..), PwMultiAffRef(..), AffListRef(..)
  , AstBuildRef(..), AstNodeRef(..), AstExprRef(..), AstNodeListRef(..)
  )
