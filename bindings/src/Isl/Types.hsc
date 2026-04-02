{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Isl.Types where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

-- Owned types (linear — consumed by isl_take functions)
newtype Ctx = Ctx { unCtx :: Ptr Ctx }
  deriving (Storable)
newtype Aff = Aff { unAff :: Ptr Aff }
  deriving (Storable)
newtype Val = Val { unVal :: Ptr Val }
  deriving (Storable)
newtype Id = Id { unId :: Ptr Id }
  deriving (Storable)
newtype Set = Set { unSet :: Ptr Set }
  deriving (Storable)
newtype BasicSet = BasicSet { unBasicSet :: Ptr BasicSet }
  deriving (Storable)
newtype UnionSet = UnionSet { unUnionSet :: Ptr UnionSet }
  deriving (Storable)
newtype Map = Map { unMap :: Ptr Map }
  deriving (Storable)
newtype BasicMap = BasicMap { unBasicMap :: Ptr BasicMap }
  deriving (Storable)
newtype UnionMap = UnionMap { unUnionMap :: Ptr UnionMap }
  deriving (Storable)
newtype Constraint = Constraint { unConstraint :: Ptr Constraint }
  deriving (Storable)
newtype Space = Space { unSpace :: Ptr Space }
  deriving (Storable)
newtype LocalSpace = LocalSpace { unLocalSpace :: Ptr LocalSpace }
  deriving (Storable)
newtype PwAff = PwAff { unPwAff :: Ptr PwAff }
  deriving (Storable)
newtype MultiAff = MultiAff { unMultiAff :: Ptr MultiAff }
  deriving (Storable)
newtype PwMultiAff = PwMultiAff { unPwMultiAff :: Ptr PwMultiAff }
  deriving (Storable)
newtype AffList = AffList { unAffList :: Ptr AffList }
  deriving (Storable)

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

-- Typeclasses for linear resource management

class Borrow owned ref | owned -> ref where
  borrow :: owned %1 -> (ref -> a) -> (a, owned)

class Consumable a where
  consume :: a %1 -> ()

class Consumable a => Dupable a where
  dup :: a %1 -> (a, a)

-- Context allocation
foreign import ccall "isl/ctx.h isl_ctx_alloc"
    c_ctx_alloc :: IO (Ptr Ctx)

foreign import ccall "isl/ctx.h isl_ctx_free"
    c_ctx_free :: Ptr Ctx -> IO ()

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
