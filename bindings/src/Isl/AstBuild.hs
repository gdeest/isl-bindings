{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal bindings for ISL AST code generation.
--
-- The ISL AST builder converts a schedule (union map) into an AST with
-- proper imperfect nesting, guards, and loop structure. This is the
-- principled path from polyhedral schedules to C code.
--
-- Key pipeline:
--   1. @astBuildAlloc@ — create a builder
--   2. @astBuildNodeFromScheduleMap@ — schedule → AST
--   3. @astNodeToC@ — AST → C string (with @S0(c0,c1,c2)@ calls for user stmts)
--   4. Substitute statement macros and wrap in function
module Isl.AstBuild
  ( -- * Construction
    astBuildAlloc
  , astBuildFromContext
    -- * AST generation
  , astBuildNodeFromScheduleMap
    -- * C output
  , astNodeToC
  , astNodeToStr
    -- * High-level
  , scheduleMapToC
    -- * Cleanup
  , astBuildFree
  , astNodeFree
    -- * Instances
  , Consumable(..)
  ) where

import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Types
import Isl.Monad (IslT(..), unsafeIslFromIO)


-- Raw C imports

foreign import ccall "isl_ast_build_alloc"
  c_ast_build_alloc :: Ctx -> IO AstBuild

foreign import ccall "isl_ast_build_from_context"
  c_ast_build_from_context :: Set -> IO AstBuild

foreign import ccall "isl_ast_build_node_from_schedule_map"
  c_ast_build_node_from_schedule_map :: AstBuild -> UnionMap -> IO AstNode

foreign import ccall "isl_ast_node_to_C_str"
  c_ast_node_to_C_str :: AstNodeRef -> IO (Ptr CChar)

foreign import ccall "isl_ast_node_to_str"
  c_ast_node_to_str :: AstNodeRef -> IO (Ptr CChar)

foreign import ccall "isl_ast_build_free"
  c_ast_build_free :: AstBuild -> IO ()

foreign import ccall "isl_ast_node_free"
  c_ast_node_free :: AstNode -> IO ()


-- Raw union map import (for scheduleMapToC)
foreign import ccall "isl_union_map_read_from_str"
  c_union_map_read_from_str :: Ctx -> Ptr CChar -> IO UnionMap

-- | Convert a schedule map (ISL string) to C code in one shot.
-- All ISL objects are created and freed within this call — no ownership leaks.
-- The schedule map string should include domain constraints, e.g.:
-- @"{ S0[t,i,j] -> [t,0,i,j] : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M; ... }"@
scheduleMapToC :: forall m. MonadIO m => String -> IslT m String
scheduleMapToC schedStr = unsafeIslFromIO $ \ctx -> do
  umap <- withCString schedStr $ \cstr ->
    c_union_map_read_from_str ctx cstr
  build <- c_ast_build_alloc ctx
  node <- c_ast_build_node_from_schedule_map build umap
  let nodeRef = AstNodeRef (unAstNode node)
  cCodePtr <- c_ast_node_to_C_str nodeRef
  cCode <- peekCString cCodePtr
  free cCodePtr
  c_ast_node_free node
  return cCode


-- Consumable instances

instance Consumable AstBuild where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_ast_build_free x)

instance Consumable AstNode where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_ast_node_free x)

instance Borrow AstNode AstNodeRef where
  borrow = unsafeCoerce $ \(AstNode p) f ->
    let result = f (AstNodeRef p)
    in (result, AstNode p)


-- Haskell wrappers

astBuildAlloc :: forall m. MonadIO m => IslT m AstBuild
astBuildAlloc = unsafeIslFromIO $ \ctx -> c_ast_build_alloc ctx

astBuildFromContext :: forall m. MonadIO m => Set %1 -> IslT m AstBuild
astBuildFromContext = unsafeCoerce go where
  go :: Set -> IslT m AstBuild
  go s = unsafeIslFromIO $ \_ -> c_ast_build_from_context s

-- | Generate an AST from a schedule map.
--
-- The schedule map has the form @{ S0[i,j] -> [c0,c1,...]; S1[...] -> [...] }@.
-- ISL produces an AST with for-loops, if-conditions, and user statement calls.
-- __Consumes both__ the build and the schedule map.
astBuildNodeFromScheduleMap :: forall m. MonadIO m
  => AstBuild %1 -> UnionMap %1 -> IslT m AstNode
astBuildNodeFromScheduleMap = unsafeCoerce go where
  go :: AstBuild -> UnionMap -> IslT m AstNode
  go build umap = unsafeIslFromIO $ \_ ->
    c_ast_build_node_from_schedule_map build umap

-- | Convert an AST node to a C code string.
-- User statements are printed as function calls: @S0(c0, c1, c2);@
-- __Borrows__ the node (does not consume it).
astNodeToC :: forall m. MonadIO m => AstNode -> IslT m String
astNodeToC (AstNode ptr) = unsafeIslFromIO $ \_ -> do
  cstr <- c_ast_node_to_C_str (AstNodeRef ptr)
  s <- peekCString cstr
  free cstr
  return s

-- | Convert an AST node to ISL's internal string representation.
astNodeToStr :: forall m. MonadIO m => AstNode -> IslT m String
astNodeToStr (AstNode ptr) = unsafeIslFromIO $ \_ -> do
  cstr <- c_ast_node_to_str (AstNodeRef ptr)
  s <- peekCString cstr
  free cstr
  return s

astBuildFree :: forall m. MonadIO m => AstBuild %1 -> IslT m ()
astBuildFree = unsafeCoerce go where
  go :: AstBuild -> IslT m ()
  go b = IslT $ \_ -> liftIO (c_ast_build_free b)

astNodeFree :: forall m. MonadIO m => AstNode %1 -> IslT m ()
astNodeFree = unsafeCoerce go where
  go :: AstNode -> IslT m ()
  go n = IslT $ \_ -> liftIO (c_ast_node_free n)
