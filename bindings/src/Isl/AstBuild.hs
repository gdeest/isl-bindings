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
    -- * AST tree walking
  , CNode(..)
  , walkAstNode
  , astExprToC
    -- * Cleanup
  , astBuildFree
  , astNodeFree
  ) where

import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Types
import Isl.Types.Internal (Consumable(..), Borrow(..))
import Isl.Monad.Internal (IslT(..), unsafeIslFromIO)


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

-- AST tree walking: node type, for-loop accessors, block children, expressions
foreign import ccall "isl_ast_node_get_type"
  c_ast_node_get_type :: AstNodeRef -> IO Int

foreign import ccall "isl_ast_node_for_get_iterator"
  c_ast_node_for_get_iterator :: AstNodeRef -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_init"
  c_ast_node_for_get_init :: AstNodeRef -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_cond"
  c_ast_node_for_get_cond :: AstNodeRef -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_inc"
  c_ast_node_for_get_inc :: AstNodeRef -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_body"
  c_ast_node_for_get_body :: AstNodeRef -> IO AstNode

foreign import ccall "isl_ast_node_if_get_cond"
  c_ast_node_if_get_cond :: AstNodeRef -> IO AstExpr
foreign import ccall "isl_ast_node_if_get_then_node"
  c_ast_node_if_get_then_node :: AstNodeRef -> IO AstNode
foreign import ccall "isl_ast_node_if_has_else_node"
  c_ast_node_if_has_else :: AstNodeRef -> IO Int
foreign import ccall "isl_ast_node_if_get_else_node"
  c_ast_node_if_get_else_node :: AstNodeRef -> IO AstNode

foreign import ccall "isl_ast_node_block_get_children"
  c_ast_node_block_get_children :: AstNodeRef -> IO AstNodeList
foreign import ccall "isl_ast_node_list_n_ast_node"
  c_ast_node_list_n :: AstNodeListRef -> IO Int
foreign import ccall "isl_ast_node_list_get_at"
  c_ast_node_list_get_at :: AstNodeListRef -> Int -> IO AstNode
foreign import ccall "isl_ast_node_list_free"
  c_ast_node_list_free :: AstNodeList -> IO ()

foreign import ccall "isl_ast_node_user_get_expr"
  c_ast_node_user_get_expr :: AstNodeRef -> IO AstExpr

foreign import ccall "isl_ast_node_mark_get_node"
  c_ast_node_mark_get_node :: AstNodeRef -> IO AstNode

foreign import ccall "isl_ast_expr_to_C_str"
  c_ast_expr_to_C_str :: AstExprRef -> IO (Ptr CChar)
foreign import ccall "isl_ast_expr_free"
  c_ast_expr_free :: AstExpr -> IO ()



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

-- ═══════════════════════════════════════════════════════════════
-- AST tree walking
-- ═══════════════════════════════════════════════════════════════

-- | Pure Haskell representation of an ISL AST node.
-- Built by 'walkAstNode' from the ISL C data structure.
data CNode
  = CFor  !String !String !String !String CNode
    -- ^ @CFor iterator init cond inc body@
  | CIf   !String CNode (Maybe CNode)
    -- ^ @CIf condition then_body else_body@
  | CBlock [CNode]
    -- ^ Sequence of statements
  | CUser !String
    -- ^ Statement call (e.g., @"Acc(c1, c2, c3)"@)
  deriving (Show)

-- | Walk an ISL AST node tree and convert to 'CNode'.
-- __Borrows__ the node (does not consume it).
walkAstNode :: forall m. MonadIO m => AstNode -> IslT m CNode
walkAstNode (AstNode ptr) = unsafeIslFromIO $ \_ -> walkIO (AstNodeRef ptr)
  where
    walkIO :: AstNodeRef -> IO CNode
    walkIO ref = do
      ty <- c_ast_node_get_type ref
      case ty of
        1 -> walkFor ref     -- isl_ast_node_for
        2 -> walkIf ref      -- isl_ast_node_if
        3 -> walkBlock ref   -- isl_ast_node_block
        4 -> walkMark ref    -- isl_ast_node_mark (unwrap to inner node)
        5 -> walkUser ref    -- isl_ast_node_user
        _ -> pure (CUser ("/* unknown node type " ++ show ty ++ " */"))

    walkFor ref = do
      iter <- c_ast_node_for_get_iterator ref
      ini  <- c_ast_node_for_get_init ref
      cond <- c_ast_node_for_get_cond ref
      inc  <- c_ast_node_for_get_inc ref
      body <- c_ast_node_for_get_body ref
      iterS <- exprToC iter
      iniS  <- exprToC ini
      condS <- exprToC cond
      incS  <- exprToC inc
      bodyN <- walkIO (AstNodeRef (unAstNode body))
      c_ast_node_free body
      pure (CFor iterS iniS condS incS bodyN)

    walkIf ref = do
      cond <- c_ast_node_if_get_cond ref
      condS <- exprToC cond
      thenN <- c_ast_node_if_get_then_node ref
      thenC <- walkIO (AstNodeRef (unAstNode thenN))
      c_ast_node_free thenN
      hasElse <- c_ast_node_if_has_else ref
      elseC <- if hasElse == 1
        then do
          elseN <- c_ast_node_if_get_else_node ref
          ec <- walkIO (AstNodeRef (unAstNode elseN))
          c_ast_node_free elseN
          pure (Just ec)
        else pure Nothing
      pure (CIf condS thenC elseC)

    walkBlock ref = do
      list <- c_ast_node_block_get_children ref
      let listRef = AstNodeListRef (unAstNodeList list)
      n <- c_ast_node_list_n listRef
      children <- mapM (\i -> do
        child <- c_ast_node_list_get_at listRef i
        cn <- walkIO (AstNodeRef (unAstNode child))
        c_ast_node_free child
        pure cn
        ) [0 .. n - 1]
      c_ast_node_list_free list
      -- Flatten nested blocks: { { a; b } c } → { a; b; c }
      pure (CBlock (concatMap flattenBlock children))

    flattenBlock :: CNode -> [CNode]
    flattenBlock (CBlock cs) = concatMap flattenBlock cs
    flattenBlock n = [n]

    walkUser ref = do
      expr <- c_ast_node_user_get_expr ref
      s <- exprToC expr
      pure (CUser (s ++ ";"))

    walkMark ref = do
      inner <- c_ast_node_mark_get_node ref
      cn <- walkIO (AstNodeRef (unAstNode inner))
      c_ast_node_free inner
      pure cn

    exprToC :: AstExpr -> IO String
    exprToC expr = do
      cstr <- c_ast_expr_to_C_str (AstExprRef (unAstExpr expr))
      s <- peekCString cstr
      free cstr
      c_ast_expr_free expr
      pure s

-- | Convert an ISL AST expression to a C string.
-- __Consumes__ the expression.
astExprToC :: forall m. MonadIO m => AstExpr -> IslT m String
astExprToC expr = unsafeIslFromIO $ \_ -> do
  cstr <- c_ast_expr_to_C_str (AstExprRef (unAstExpr expr))
  s <- peekCString cstr
  free cstr
  c_ast_expr_free expr
  pure s
