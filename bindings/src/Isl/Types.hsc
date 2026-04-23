{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

-- | Public ISL type surface.  Re-exports newtype wrappers from
-- "Isl.Types.Raw"; @Ref@ constructors are deliberately hidden — use
-- 'Isl.Linear.query' \/ 'Isl.Linear.queryM' \/ 'Isl.Foreach' to obtain
-- refs.  Escape-hatch ref construction is in "Isl.Unsafe".
module Isl.Types
  ( -- * Owned types (linear — consumed by isl_take functions)
    Ctx(..)
  , Aff(..), Val(..), Id(..)
  , Set(..), BasicSet(..), UnionSet(..)
  , Map(..), BasicMap(..), UnionMap(..)
  , Constraint(..), Space(..), LocalSpace(..)
  , PwAff(..), MultiAff(..), PwMultiAff(..), AffList(..)
    -- * Borrowed reference types (region-indexed; constructors hidden)
  , AffRef, ValRef, IdRef
  , SetRef, BasicSetRef, UnionSetRef
  , MapRef, BasicMapRef, UnionMapRef
  , ConstraintRef, SpaceRef, LocalSpaceRef
  , PwAffRef, MultiAffRef, PwMultiAffRef, AffListRef
    -- * AST code generation types
  , AstBuild(..), AstNode(..), AstExpr(..)
  , AstNodeList(..)
  , AstBuildRef, AstNodeRef, AstExprRef, AstNodeListRef
    -- * Null checking
  , CheckIslNonNull(..), checkIslNonNull
    -- * Resource management classes (methods hidden — use Isl.Linear)
  , Consumable, Borrow, Dupable
    -- * Context FFI
  , c_ctx_alloc, c_ctx_free
  , c_options_set_on_error
  , c_ctx_last_error, c_ctx_last_error_msg
  , c_ctx_last_error_file, c_ctx_last_error_line
  , c_ctx_reset_error
    -- * DimType
  , DimType(..)
  , islDimCst, islDimParam, islDimIn, islDimOut
  , islDimSet, islDimDiv, islDimAll
  ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Isl.Types.Raw
import Isl.Types.Internal (Consumable, Borrow, Dupable)

-- Context allocation
foreign import ccall "isl/ctx.h isl_ctx_alloc"
    c_ctx_alloc :: IO (Ptr Ctx)

foreign import ccall "isl/ctx.h isl_ctx_free"
    c_ctx_free :: Ptr Ctx -> IO ()

-- | Set ISL error behavior.  0 = warn (default), 1 = continue, 2 = abort.
foreign import ccall "isl/options.h isl_options_set_on_error"
    c_options_set_on_error :: Ptr Ctx -> CInt -> IO CInt

-- Error introspection — used by runIslT to crash on any ISL error.
foreign import ccall "isl/ctx.h isl_ctx_last_error"
    c_ctx_last_error :: Ptr Ctx -> IO CInt

foreign import ccall "isl/ctx.h isl_ctx_last_error_msg"
    c_ctx_last_error_msg :: Ptr Ctx -> IO (Ptr CChar)

foreign import ccall "isl/ctx.h isl_ctx_last_error_file"
    c_ctx_last_error_file :: Ptr Ctx -> IO (Ptr CChar)

foreign import ccall "isl/ctx.h isl_ctx_last_error_line"
    c_ctx_last_error_line :: Ptr Ctx -> IO CInt

foreign import ccall "isl/ctx.h isl_ctx_reset_error"
    c_ctx_reset_error :: Ptr Ctx -> IO ()

-- DimType enum
#include <isl/space_type.h>

newtype DimType = DimType CInt deriving (Eq, Storable)

#enum DimType,DimType \
  , isl_dim_cst \
  , isl_dim_param \
  , isl_dim_in \
  , isl_dim_out \
  , isl_dim_set \
  , isl_dim_div \
  , isl_dim_all
