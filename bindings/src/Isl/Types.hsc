{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

module Isl.Types
  ( -- * Owned types (linear — consumed by isl_take functions)
    Ctx(..)
  , Aff(..), Val(..), Id(..)
  , Set(..), BasicSet(..), UnionSet(..)
  , Map(..), BasicMap(..), UnionMap(..)
  , Constraint(..), Space(..), LocalSpace(..)
  , PwAff(..), MultiAff(..), PwMultiAff(..), AffList(..)
    -- * Borrowed reference types (unrestricted — used by isl_keep functions)
  , AffRef(..), ValRef(..), IdRef(..)
  , SetRef(..), BasicSetRef(..), UnionSetRef(..)
  , MapRef(..), BasicMapRef(..), UnionMapRef(..)
  , ConstraintRef(..), SpaceRef(..), LocalSpaceRef(..)
  , PwAffRef(..), MultiAffRef(..), PwMultiAffRef(..), AffListRef(..)
    -- * AST code generation types
  , AstBuild(..), AstNode(..), AstExpr(..)
  , AstBuildRef(..), AstNodeRef(..), AstExprRef(..)
  , AstNodeList(..), AstNodeListRef(..)
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
import Control.DeepSeq (NFData)

import Isl.Types.Internal (Consumable, Borrow, Dupable)

-- Owned types (linear — consumed by isl_take functions)
newtype Ctx = Ctx { unCtx :: Ptr Ctx }
  deriving (Storable)
newtype Aff = Aff { unAff :: Ptr Aff }
  deriving (Storable, NFData)
newtype Val = Val { unVal :: Ptr Val }
  deriving (Storable, NFData)
newtype Id = Id { unId :: Ptr Id }
  deriving (Storable, NFData)
newtype Set = Set { unSet :: Ptr Set }
  deriving (Storable, NFData)
newtype BasicSet = BasicSet { unBasicSet :: Ptr BasicSet }
  deriving (Storable, NFData)
newtype UnionSet = UnionSet { unUnionSet :: Ptr UnionSet }
  deriving (Storable, NFData)
newtype Map = Map { unMap :: Ptr Map }
  deriving (Storable, NFData)
newtype BasicMap = BasicMap { unBasicMap :: Ptr BasicMap }
  deriving (Storable, NFData)
newtype UnionMap = UnionMap { unUnionMap :: Ptr UnionMap }
  deriving (Storable, NFData)
newtype Constraint = Constraint { unConstraint :: Ptr Constraint }
  deriving (Storable, NFData)
newtype Space = Space { unSpace :: Ptr Space }
  deriving (Storable, NFData)
newtype LocalSpace = LocalSpace { unLocalSpace :: Ptr LocalSpace }
  deriving (Storable, NFData)
newtype PwAff = PwAff { unPwAff :: Ptr PwAff }
  deriving (Storable, NFData)
newtype MultiAff = MultiAff { unMultiAff :: Ptr MultiAff }
  deriving (Storable, NFData)
newtype PwMultiAff = PwMultiAff { unPwMultiAff :: Ptr PwMultiAff }
  deriving (Storable, NFData)
newtype AffList = AffList { unAffList :: Ptr AffList }
  deriving (Storable, NFData)

-- Borrowed reference types (unrestricted — used by isl_keep functions)
newtype AffRef = AffRef (Ptr Aff)
  deriving (Storable)
newtype ValRef = ValRef (Ptr Val)
  deriving (Storable)
newtype IdRef = IdRef (Ptr Id)
  deriving (Storable)
newtype SetRef = SetRef (Ptr Set)
  deriving (Storable)
newtype BasicSetRef = BasicSetRef (Ptr BasicSet)
  deriving (Storable)
newtype UnionSetRef = UnionSetRef (Ptr UnionSet)
  deriving (Storable)
newtype MapRef = MapRef (Ptr Map)
  deriving (Storable)
newtype BasicMapRef = BasicMapRef (Ptr BasicMap)
  deriving (Storable)
newtype UnionMapRef = UnionMapRef (Ptr UnionMap)
  deriving (Storable)
newtype ConstraintRef = ConstraintRef (Ptr Constraint)
  deriving (Storable)
newtype SpaceRef = SpaceRef (Ptr Space)
  deriving (Storable)
newtype LocalSpaceRef = LocalSpaceRef (Ptr LocalSpace)
  deriving (Storable)
newtype PwAffRef = PwAffRef (Ptr PwAff)
  deriving (Storable)
newtype MultiAffRef = MultiAffRef (Ptr MultiAff)
  deriving (Storable)
newtype PwMultiAffRef = PwMultiAffRef (Ptr PwMultiAff)
  deriving (Storable)
newtype AffListRef = AffListRef (Ptr AffList)
  deriving (Storable)

-- AST code generation types
newtype AstBuild = AstBuild { unAstBuild :: Ptr AstBuild }
  deriving (Storable)
newtype AstNode = AstNode { unAstNode :: Ptr AstNode }
  deriving (Storable)
newtype AstExpr = AstExpr { unAstExpr :: Ptr AstExpr }
  deriving (Storable)

newtype AstBuildRef = AstBuildRef (Ptr AstBuild)
  deriving (Storable)
newtype AstNodeRef = AstNodeRef (Ptr AstNode)
  deriving (Storable)
newtype AstExprRef = AstExprRef (Ptr AstExpr)
  deriving (Storable)
newtype AstNodeList = AstNodeList { unAstNodeList :: Ptr AstNodeList }
  deriving (Storable)
newtype AstNodeListRef = AstNodeListRef (Ptr AstNodeList)
  deriving (Storable)

-- | Check that an ISL pointer is non-null.  Every ISL @read_from_str@
-- function returns NULL on parse failure; this turns a silent null into
-- a loud crash.  Use: @checkIslNonNull "isl_map_read_from_str" str result@.
class CheckIslNonNull a where
  islIsNull :: a -> Bool

instance CheckIslNonNull Aff       where islIsNull (Aff p)       = p == nullPtr
instance CheckIslNonNull Val       where islIsNull (Val p)       = p == nullPtr
instance CheckIslNonNull Id        where islIsNull (Id p)        = p == nullPtr
instance CheckIslNonNull Set       where islIsNull (Set p)       = p == nullPtr
instance CheckIslNonNull BasicSet  where islIsNull (BasicSet p)  = p == nullPtr
instance CheckIslNonNull UnionSet  where islIsNull (UnionSet p)  = p == nullPtr
instance CheckIslNonNull Map       where islIsNull (Map p)       = p == nullPtr
instance CheckIslNonNull BasicMap  where islIsNull (BasicMap p)  = p == nullPtr
instance CheckIslNonNull UnionMap  where islIsNull (UnionMap p)  = p == nullPtr
instance CheckIslNonNull Space     where islIsNull (Space p)     = p == nullPtr
instance CheckIslNonNull PwAff     where islIsNull (PwAff p)     = p == nullPtr
instance CheckIslNonNull MultiAff  where islIsNull (MultiAff p)  = p == nullPtr
instance CheckIslNonNull PwMultiAff where islIsNull (PwMultiAff p) = p == nullPtr
instance CheckIslNonNull AffList   where islIsNull (AffList p)   = p == nullPtr

-- | Crash if an ISL pointer is null.  Call sites pass the C function
-- name and the input string for a clear error message.
checkIslNonNull :: CheckIslNonNull a => String -> String -> a -> a
checkIslNonNull func input x
  | islIsNull x = error $ func ++ ": ISL returned NULL for input: " ++ input
  | otherwise    = x

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
