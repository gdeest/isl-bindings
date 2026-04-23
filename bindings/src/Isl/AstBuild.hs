{-# LANGUAGE BangPatterns #-}
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

import Control.Exception (bracket)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar, CInt(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Types.Raw
import Isl.Types.Internal (Consumable(..), Borrow(..))
import Isl.Monad.Internal (IslT(..), unsafeIslFromIO)
import qualified Isl.Id.Generated as Id


-- Raw C imports

foreign import ccall "isl_ast_build_alloc"
  c_ast_build_alloc :: Ctx -> IO AstBuild

foreign import ccall "isl_ast_build_from_context"
  c_ast_build_from_context :: Set -> IO AstBuild

foreign import ccall "isl_ast_build_node_from_schedule_map"
  c_ast_build_node_from_schedule_map :: AstBuild -> UnionMap -> IO AstNode

foreign import ccall "isl_ast_node_to_C_str"
  c_ast_node_to_C_str :: AstNodeRef s -> IO (Ptr CChar)

foreign import ccall "isl_ast_node_to_str"
  c_ast_node_to_str :: AstNodeRef s -> IO (Ptr CChar)

foreign import ccall "isl_ast_build_free"
  c_ast_build_free :: AstBuild -> IO ()

foreign import ccall "isl_ast_node_free"
  c_ast_node_free :: AstNode -> IO ()

-- AST tree walking: node type, for-loop accessors, block children, expressions
foreign import ccall "isl_ast_node_get_type"
  c_ast_node_get_type :: AstNodeRef s -> IO Int

foreign import ccall "isl_ast_node_for_get_iterator"
  c_ast_node_for_get_iterator :: AstNodeRef s -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_init"
  c_ast_node_for_get_init :: AstNodeRef s -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_cond"
  c_ast_node_for_get_cond :: AstNodeRef s -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_inc"
  c_ast_node_for_get_inc :: AstNodeRef s -> IO AstExpr
foreign import ccall "isl_ast_node_for_get_body"
  c_ast_node_for_get_body :: AstNodeRef s -> IO AstNode

foreign import ccall "isl_ast_node_if_get_cond"
  c_ast_node_if_get_cond :: AstNodeRef s -> IO AstExpr
foreign import ccall "isl_ast_node_if_get_then_node"
  c_ast_node_if_get_then_node :: AstNodeRef s -> IO AstNode
foreign import ccall "isl_ast_node_if_has_else_node"
  c_ast_node_if_has_else :: AstNodeRef s -> IO Int
foreign import ccall "isl_ast_node_if_get_else_node"
  c_ast_node_if_get_else_node :: AstNodeRef s -> IO AstNode

foreign import ccall "isl_ast_node_block_get_children"
  c_ast_node_block_get_children :: AstNodeRef s -> IO AstNodeList
foreign import ccall "isl_ast_node_list_n_ast_node"
  c_ast_node_list_n :: AstNodeListRef s -> IO Int
foreign import ccall "isl_ast_node_list_get_at"
  c_ast_node_list_get_at :: AstNodeListRef s -> Int -> IO AstNode
foreign import ccall "isl_ast_node_list_free"
  c_ast_node_list_free :: AstNodeList -> IO ()

foreign import ccall "isl_ast_node_user_get_expr"
  c_ast_node_user_get_expr :: AstNodeRef s -> IO AstExpr

foreign import ccall "isl_ast_node_mark_get_node"
  c_ast_node_mark_get_node :: AstNodeRef s -> IO AstNode

foreign import ccall "isl_ast_expr_to_C_str"
  c_ast_expr_to_C_str :: AstExprRef s -> IO (Ptr CChar)
foreign import ccall "isl_ast_expr_free"
  c_ast_expr_free :: AstExpr -> IO ()

-- Structured access to call expressions (see ISL's isl/ast.h).
-- For a call expression @S(a0, a1, …)@: arg 0 is the callee id, args 1..n-1
-- are the call arguments.
foreign import ccall "isl_ast_expr_get_op_n_arg"
  c_ast_expr_get_op_n_arg :: AstExprRef s -> IO CInt
foreign import ccall "isl_ast_expr_get_op_arg"
  c_ast_expr_get_op_arg :: AstExprRef s -> CInt -> IO AstExpr
foreign import ccall "isl_ast_expr_get_id"
  c_ast_expr_get_id :: AstExprRef s -> IO Id



-- Consumable instances

instance Consumable AstBuild where
  consume = unsafeCoerce c_ast_build_free

instance Consumable AstNode where
  consume = unsafeCoerce c_ast_node_free

instance Consumable AstExpr where
  consume = unsafeCoerce c_ast_expr_free

instance Consumable AstNodeList where
  consume = unsafeCoerce c_ast_node_list_free

instance Borrow AstNode AstNodeRef where
  borrow = unsafeCoerce $ \(AstNode p) f ->
    let !r = f (AstNodeRef p) in (r, AstNode p)


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
-- ISL signature: @isl_ast_build_node_from_schedule_map(__isl_keep build,
-- __isl_take schedule)@ — the build is *not* freed by the C call, only
-- the schedule is.  The %1 signature claims both are consumed, so the
-- binding itself must free the build to honour that contract; otherwise
-- the build leaks into the enclosing 'runIslT' ctx and triggers
-- @isl_ctx not freed as some objects still reference it@ at shutdown.
astBuildNodeFromScheduleMap :: forall m. MonadIO m
  => AstBuild %1 -> UnionMap %1 -> IslT m AstNode
astBuildNodeFromScheduleMap = unsafeCoerce go where
  go :: AstBuild -> UnionMap -> IslT m AstNode
  go build umap = unsafeIslFromIO $ \_ -> do
    node <- c_ast_build_node_from_schedule_map build umap
    c_ast_build_free build
    pure node

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
  | CUser !String ![String]
    -- ^ @CUser stmtName argExprs@ — callee name and rendered argument
    -- expressions, extracted structurally via 'isl_ast_expr_get_op_arg'.
  deriving (Show)

-- | Walk an ISL AST node tree and convert to 'CNode'.
-- __Borrows__ the node (does not consume it).
walkAstNode :: forall m. MonadIO m => AstNode -> IslT m CNode
walkAstNode (AstNode ptr) = unsafeIslFromIO $ \_ -> walkIO (AstNodeRef ptr)
  where
    walkIO :: forall s. AstNodeRef s -> IO CNode
    walkIO ref = do
      ty <- c_ast_node_get_type ref
      case ty of
        1 -> walkFor ref     -- isl_ast_node_for
        2 -> walkIf ref      -- isl_ast_node_if
        3 -> walkBlock ref   -- isl_ast_node_block
        4 -> walkMark ref    -- isl_ast_node_mark (unwrap to inner node)
        5 -> walkUser ref    -- isl_ast_node_user
        _ -> fail ("walkAstNode: unexpected ISL AST node type " ++ show ty)

    -- Each owned child (AstExpr / AstNode / AstNodeList / Id) lives inside
    -- a bracket so an exception in any inner step still frees it.

    walkFor ref =
      bracket (c_ast_node_for_get_iterator ref) c_ast_expr_free $ \iter ->
      bracket (c_ast_node_for_get_init     ref) c_ast_expr_free $ \ini  ->
      bracket (c_ast_node_for_get_cond     ref) c_ast_expr_free $ \cond ->
      bracket (c_ast_node_for_get_inc      ref) c_ast_expr_free $ \inc  ->
      bracket (c_ast_node_for_get_body     ref) c_ast_node_free $ \body -> do
        iterS <- exprRefToC iter
        iniS  <- exprRefToC ini
        condS <- exprRefToC cond
        incS  <- exprRefToC inc
        bodyN <- walkIO (AstNodeRef (unAstNode body))
        pure (CFor iterS iniS condS incS bodyN)

    walkIf ref =
      bracket (c_ast_node_if_get_cond ref) c_ast_expr_free $ \cond -> do
        condS <- exprRefToC cond
        thenC <-
          bracket (c_ast_node_if_get_then_node ref) c_ast_node_free $ \thenN ->
            walkIO (AstNodeRef (unAstNode thenN))
        hasElse <- c_ast_node_if_has_else ref
        elseC <- if hasElse == 1
          then fmap Just $
            bracket (c_ast_node_if_get_else_node ref) c_ast_node_free $ \elseN ->
              walkIO (AstNodeRef (unAstNode elseN))
          else pure Nothing
        pure (CIf condS thenC elseC)

    walkBlock ref =
      bracket (c_ast_node_block_get_children ref) c_ast_node_list_free $ \list -> do
        let listRef = AstNodeListRef (unAstNodeList list)
        n <- c_ast_node_list_n listRef
        children <- mapM (\i ->
          bracket (c_ast_node_list_get_at listRef i) c_ast_node_free $ \child ->
            walkIO (AstNodeRef (unAstNode child))
          ) [0 .. n - 1]
        -- Flatten nested blocks: { { a; b } c } → { a; b; c }
        pure (CBlock (concatMap flattenBlock children))

    flattenBlock :: CNode -> [CNode]
    flattenBlock (CBlock cs) = concatMap flattenBlock cs
    flattenBlock n = [n]

    walkUser ref =
      bracket (c_ast_node_user_get_expr ref) c_ast_expr_free $ \expr -> do
        let exprRef = AstExprRef (unAstExpr expr)
        nArgs <- c_ast_expr_get_op_n_arg exprRef
        -- Arg 0 is the callee id; args 1..n-1 are the call arguments.
        name <-
          bracket (c_ast_expr_get_op_arg exprRef 0) c_ast_expr_free $ \nameExpr ->
          bracket (c_ast_expr_get_id (AstExprRef (unAstExpr nameExpr))) Id.c_free $ \ident -> do
            let !nm = Id.getName (IdRef (unId ident))
            pure nm
        args <- mapM (\i ->
            bracket (c_ast_expr_get_op_arg exprRef i) c_ast_expr_free exprRefToC
          ) [1 .. nArgs - 1]
        pure (CUser name args)

    walkMark ref =
      bracket (c_ast_node_mark_get_node ref) c_ast_node_free $ \inner ->
        walkIO (AstNodeRef (unAstNode inner))

    exprRefToC :: AstExpr -> IO String
    exprRefToC expr = do
      cstr <- c_ast_expr_to_C_str (AstExprRef (unAstExpr expr))
      s <- peekCString cstr
      free cstr
      pure s

-- | Convert an ISL AST expression to a C string.
-- __Consumes__ the expression.
astExprToC :: forall m. MonadIO m => AstExpr -> IslT m String
astExprToC expr = unsafeIslFromIO $ \_ ->
  bracket (pure expr) c_ast_expr_free $ \e -> do
    cstr <- c_ast_expr_to_C_str (AstExprRef (unAstExpr e))
    s <- peekCString cstr
    free cstr
    pure s
