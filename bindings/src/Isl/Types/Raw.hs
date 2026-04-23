{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Strict #-}

-- | Internal module — defines the raw ISL newtypes with their constructors.
-- Listed in @other-modules@; user code imports 'Isl.Types' (which hides the
-- @Ref@ constructors) or, for escape-hatch use in trusted code,
-- 'Isl.Unsafe' (which re-exports them).
--
-- Every @*Ref@ is parameterised by a phantom region @s@ with a @nominal@ role:
--   * @s@ cannot be replaced by coercion, so distinct regions stay distinct.
--   * Because the @Ref@ constructor is not in scope outside this module (and
--     'Isl.Unsafe'), external code cannot coerce a @FooRef s@ to the owned
--     @Foo@ even though both wrap the same @Ptr Foo@.
module Isl.Types.Raw
  ( -- * Owned types (linear — consumed by isl_take functions)
    Ctx(..)
  , Aff(..), Val(..), Id(..)
  , Set(..), BasicSet(..), UnionSet(..)
  , Map(..), BasicMap(..), UnionMap(..)
  , Constraint(..), Space(..), LocalSpace(..)
  , PwAff(..), MultiAff(..), PwMultiAff(..), AffList(..)
    -- * Borrowed reference types (region-indexed)
  , AffRef(..), ValRef(..), IdRef(..)
  , SetRef(..), BasicSetRef(..), UnionSetRef(..)
  , MapRef(..), BasicMapRef(..), UnionMapRef(..)
  , ConstraintRef(..), SpaceRef(..), LocalSpaceRef(..)
  , PwAffRef(..), MultiAffRef(..), PwMultiAffRef(..), AffListRef(..)
    -- * AST code generation types
  , AstBuild(..), AstNode(..), AstExpr(..), AstNodeList(..)
  , AstBuildRef(..), AstNodeRef(..), AstExprRef(..), AstNodeListRef(..)
    -- * Null checking
  , CheckIslNonNull(..), checkIslNonNull
  ) where

import Control.DeepSeq (NFData)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable)

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

-- Borrowed reference types — phantom @s@ with nominal role blocks cross-region
-- coercion.  Lack of constructor visibility in @Isl.Types@ blocks
-- @coerce :: FooRef s -> Foo@ in user code.
newtype AffRef        s = AffRef        (Ptr Aff)        deriving (Storable)
newtype ValRef        s = ValRef        (Ptr Val)        deriving (Storable)
newtype IdRef         s = IdRef         (Ptr Id)         deriving (Storable)
newtype SetRef        s = SetRef        (Ptr Set)        deriving (Storable)
newtype BasicSetRef   s = BasicSetRef   (Ptr BasicSet)   deriving (Storable)
newtype UnionSetRef   s = UnionSetRef   (Ptr UnionSet)   deriving (Storable)
newtype MapRef        s = MapRef        (Ptr Map)        deriving (Storable)
newtype BasicMapRef   s = BasicMapRef   (Ptr BasicMap)   deriving (Storable)
newtype UnionMapRef   s = UnionMapRef   (Ptr UnionMap)   deriving (Storable)
newtype ConstraintRef s = ConstraintRef (Ptr Constraint) deriving (Storable)
newtype SpaceRef      s = SpaceRef      (Ptr Space)      deriving (Storable)
newtype LocalSpaceRef s = LocalSpaceRef (Ptr LocalSpace) deriving (Storable)
newtype PwAffRef      s = PwAffRef      (Ptr PwAff)      deriving (Storable)
newtype MultiAffRef   s = MultiAffRef   (Ptr MultiAff)   deriving (Storable)
newtype PwMultiAffRef s = PwMultiAffRef (Ptr PwMultiAff) deriving (Storable)
newtype AffListRef    s = AffListRef    (Ptr AffList)    deriving (Storable)

type role AffRef        nominal
type role ValRef        nominal
type role IdRef         nominal
type role SetRef        nominal
type role BasicSetRef   nominal
type role UnionSetRef   nominal
type role MapRef        nominal
type role BasicMapRef   nominal
type role UnionMapRef   nominal
type role ConstraintRef nominal
type role SpaceRef      nominal
type role LocalSpaceRef nominal
type role PwAffRef      nominal
type role MultiAffRef   nominal
type role PwMultiAffRef nominal
type role AffListRef    nominal

-- AST code generation types
newtype AstBuild = AstBuild { unAstBuild :: Ptr AstBuild }
  deriving (Storable)
newtype AstNode = AstNode { unAstNode :: Ptr AstNode }
  deriving (Storable)
newtype AstExpr = AstExpr { unAstExpr :: Ptr AstExpr }
  deriving (Storable)
newtype AstNodeList = AstNodeList { unAstNodeList :: Ptr AstNodeList }
  deriving (Storable)

newtype AstBuildRef    s = AstBuildRef    (Ptr AstBuild)    deriving (Storable)
newtype AstNodeRef     s = AstNodeRef     (Ptr AstNode)     deriving (Storable)
newtype AstExprRef     s = AstExprRef     (Ptr AstExpr)     deriving (Storable)
newtype AstNodeListRef s = AstNodeListRef (Ptr AstNodeList) deriving (Storable)

type role AstBuildRef    nominal
type role AstNodeRef     nominal
type role AstExprRef     nominal
type role AstNodeListRef nominal

-- | Check that an ISL pointer is non-null.  Every ISL @read_from_str@
-- function returns NULL on parse failure; this turns a silent null into
-- a loud crash.  Use: @checkIslNonNull "isl_map_read_from_str" str result@.
class CheckIslNonNull a where
  islIsNull :: a -> Bool

instance CheckIslNonNull Aff        where islIsNull (Aff p)        = p == nullPtr
instance CheckIslNonNull Val        where islIsNull (Val p)        = p == nullPtr
instance CheckIslNonNull Id         where islIsNull (Id p)         = p == nullPtr
instance CheckIslNonNull Set        where islIsNull (Set p)        = p == nullPtr
instance CheckIslNonNull BasicSet   where islIsNull (BasicSet p)   = p == nullPtr
instance CheckIslNonNull UnionSet   where islIsNull (UnionSet p)   = p == nullPtr
instance CheckIslNonNull Map        where islIsNull (Map p)        = p == nullPtr
instance CheckIslNonNull BasicMap   where islIsNull (BasicMap p)   = p == nullPtr
instance CheckIslNonNull UnionMap   where islIsNull (UnionMap p)   = p == nullPtr
instance CheckIslNonNull Space      where islIsNull (Space p)      = p == nullPtr
instance CheckIslNonNull PwAff      where islIsNull (PwAff p)      = p == nullPtr
instance CheckIslNonNull MultiAff   where islIsNull (MultiAff p)   = p == nullPtr
instance CheckIslNonNull PwMultiAff where islIsNull (PwMultiAff p) = p == nullPtr
instance CheckIslNonNull AffList    where islIsNull (AffList p)    = p == nullPtr

-- | Crash if an ISL pointer is null.  Call sites pass the C function
-- name and the input string for a clear error message.
checkIslNonNull :: CheckIslNonNull a => String -> String -> a -> a
checkIslNonNull func input x
  | islIsNull x = error $ func ++ ": ISL returned NULL for input: " ++ input
  | otherwise    = x
