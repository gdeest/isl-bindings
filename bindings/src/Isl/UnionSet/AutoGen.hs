{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.UnionSet.AutoGen where

import Control.Monad
import Data.Reflection
import Isl.Types
import Debug.Trace

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall "isl_union_set_contains" c_contains :: UnionSet -> Space -> IO C.CInt


contains :: (Given Ctx) => UnionSet -> Space -> Int
contains = \uset' space' -> trace "contains" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      uset <- (return) uset'
      space <- (return) space'

      let ctx = given :: Ctx
      c_contains uset space


foreign import ccall "isl_union_set_get_ctx" c_getCtx :: UnionSet -> IO Ctx


getCtx :: (Given Ctx) => UnionSet -> Ctx
getCtx = \uset' -> trace "getCtx" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_getCtx uset


foreign import ccall "isl_union_set_dump" c_dump :: UnionSet -> IO ()


dump :: (Given Ctx) => UnionSet -> ()
dump = \uset' -> trace "dump" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_dump uset


foreign import ccall "isl_union_set_is_params" c_isParams :: UnionSet -> IO C.CBool


isParams :: (Given Ctx) => UnionSet -> Bool
isParams = \uset' -> trace "isParams" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_isParams uset


foreign import ccall "isl_union_set_lex_ge_union_set" c_lexGeUnionSet :: UnionSet -> UnionSet -> IO UnionMap


lexGeUnionSet :: (Given Ctx) => UnionSet -> UnionSet -> UnionMap
lexGeUnionSet = \uset1' uset2' -> trace "lexGeUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_lexGeUnionSet uset1 uset2


foreign import ccall "isl_union_set_lex_gt_union_set" c_lexGtUnionSet :: UnionSet -> UnionSet -> IO UnionMap


lexGtUnionSet :: (Given Ctx) => UnionSet -> UnionSet -> UnionMap
lexGtUnionSet = \uset1' uset2' -> trace "lexGtUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_lexGtUnionSet uset1 uset2


foreign import ccall "isl_union_set_lex_le_union_set" c_lexLeUnionSet :: UnionSet -> UnionSet -> IO UnionMap


lexLeUnionSet :: (Given Ctx) => UnionSet -> UnionSet -> UnionMap
lexLeUnionSet = \uset1' uset2' -> trace "lexLeUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_lexLeUnionSet uset1 uset2


foreign import ccall "isl_union_set_lex_lt_union_set" c_lexLtUnionSet :: UnionSet -> UnionSet -> IO UnionMap


lexLtUnionSet :: (Given Ctx) => UnionSet -> UnionSet -> UnionMap
lexLtUnionSet = \uset1' uset2' -> trace "lexLtUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_lexLtUnionSet uset1 uset2


foreign import ccall "isl_union_set_wrapped_domain_map" c_wrappedDomainMap :: UnionSet -> IO UnionMap


wrappedDomainMap :: (Given Ctx) => UnionSet -> UnionMap
wrappedDomainMap = \uset' -> trace "wrappedDomainMap" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_wrappedDomainMap uset


foreign import ccall "isl_union_set_sample" c_sample :: UnionSet -> IO BasicSet


sample :: (Given Ctx) => UnionSet -> BasicSet
sample = \uset' -> trace "sample" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_sample uset


foreign import ccall "isl_union_set_add_set" c_addSet :: UnionSet -> Set -> IO UnionSet


addSet :: (Given Ctx) => UnionSet -> Set -> UnionSet
addSet = \uset' set' -> trace "addSet" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      set <- (return) set'

      let ctx = given :: Ctx
      c_addSet uset set


foreign import ccall "isl_union_set_align_params" c_alignParams :: UnionSet -> Space -> IO UnionSet


alignParams :: (Given Ctx) => UnionSet -> Space -> UnionSet
alignParams = \uset' model' -> trace "alignParams" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      model <- (return) model'

      let ctx = given :: Ctx
      c_alignParams uset model


foreign import ccall "isl_union_set_coefficients" c_coefficients :: UnionSet -> IO UnionSet


coefficients :: (Given Ctx) => UnionSet -> UnionSet
coefficients = \bset' -> trace "coefficients" $ 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_coefficients bset


foreign import ccall "isl_union_set_copy" c_copy :: UnionSet -> IO UnionSet


copy :: (Given Ctx) => UnionSet -> UnionSet
copy = \uset' -> trace "copy" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_copy uset


foreign import ccall "isl_union_set_empty" c_empty :: Space -> IO UnionSet


empty :: (Given Ctx) => Space -> UnionSet
empty = \space' -> trace "empty" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_empty space


foreign import ccall "isl_union_set_empty_ctx" c_emptyCtx :: Ctx -> IO UnionSet


emptyCtx :: (Given Ctx) => UnionSet
emptyCtx =  trace "emptyCtx" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_emptyCtx ctx


foreign import ccall "isl_union_set_empty_space" c_emptySpace :: Space -> IO UnionSet


emptySpace :: (Given Ctx) => Space -> UnionSet
emptySpace = \space' -> trace "emptySpace" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_emptySpace space


foreign import ccall "isl_union_set_lift" c_lift :: UnionSet -> IO UnionSet


lift :: (Given Ctx) => UnionSet -> UnionSet
lift = \uset' -> trace "lift" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_lift uset


foreign import ccall "isl_union_set_product" c_product :: UnionSet -> UnionSet -> IO UnionSet


product :: (Given Ctx) => UnionSet -> UnionSet -> UnionSet
product = \uset1' uset2' -> trace "product" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_product uset1 uset2


foreign import ccall "isl_union_set_project_out" c_projectOut :: UnionSet -> DimType -> C.CUInt -> C.CUInt -> IO UnionSet


projectOut :: (Given Ctx) => UnionSet -> DimType -> Int -> Int -> UnionSet
projectOut = \uset' typ' first' n' -> trace "projectOut" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_projectOut uset typ first n


foreign import ccall "isl_union_set_remove_divs" c_removeDivs :: UnionSet -> IO UnionSet


removeDivs :: (Given Ctx) => UnionSet -> UnionSet
removeDivs = \bset' -> trace "removeDivs" $ 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_removeDivs bset


foreign import ccall "isl_union_set_remove_redundancies" c_removeRedundancies :: UnionSet -> IO UnionSet


removeRedundancies :: (Given Ctx) => UnionSet -> UnionSet
removeRedundancies = \uset' -> trace "removeRedundancies" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_removeRedundancies uset


foreign import ccall "isl_union_set_reset_user" c_resetUser :: UnionSet -> IO UnionSet


resetUser :: (Given Ctx) => UnionSet -> UnionSet
resetUser = \uset' -> trace "resetUser" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_resetUser uset


foreign import ccall "isl_union_set_simple_hull" c_simpleHull :: UnionSet -> IO UnionSet


simpleHull :: (Given Ctx) => UnionSet -> UnionSet
simpleHull = \uset' -> trace "simpleHull" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_simpleHull uset


foreign import ccall "isl_union_set_solutions" c_solutions :: UnionSet -> IO UnionSet


solutions :: (Given Ctx) => UnionSet -> UnionSet
solutions = \bset' -> trace "solutions" $ 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_solutions bset


foreign import ccall "isl_union_set_to_str" c_toStr :: UnionSet -> IO C.CString


toStr :: (Given Ctx) => UnionSet -> String
toStr = \uset' -> trace "toStr" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_toStr uset


foreign import ccall "isl_union_set_isa_set" c_isaSet :: UnionSet -> IO C.CInt


isaSet :: (Given Ctx) => UnionSet -> Int
isaSet = \uset' -> trace "isaSet" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_isaSet uset


foreign import ccall "isl_union_set_is_disjoint" c_isDisjoint :: UnionSet -> UnionSet -> IO C.CBool


isDisjoint :: (Given Ctx) => UnionSet -> UnionSet -> Bool
isDisjoint = \uset1' uset2' -> trace "isDisjoint" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_isDisjoint uset1 uset2


foreign import ccall "isl_union_set_is_empty" c_isEmpty :: UnionSet -> IO C.CBool


isEmpty :: (Given Ctx) => UnionSet -> Bool
isEmpty = \uset' -> trace "isEmpty" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_isEmpty uset


foreign import ccall "isl_union_set_is_equal" c_isEqual :: UnionSet -> UnionSet -> IO C.CBool


isEqual :: (Given Ctx) => UnionSet -> UnionSet -> Bool
isEqual = \uset1' uset2' -> trace "isEqual" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_isEqual uset1 uset2


foreign import ccall "isl_union_set_is_strict_subset" c_isStrictSubset :: UnionSet -> UnionSet -> IO C.CBool


isStrictSubset :: (Given Ctx) => UnionSet -> UnionSet -> Bool
isStrictSubset = \uset1' uset2' -> trace "isStrictSubset" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_isStrictSubset uset1 uset2


foreign import ccall "isl_union_set_is_subset" c_isSubset :: UnionSet -> UnionSet -> IO C.CBool


isSubset :: (Given Ctx) => UnionSet -> UnionSet -> Bool
isSubset = \uset1' uset2' -> trace "isSubset" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_isSubset uset1 uset2


foreign import ccall "isl_union_set_as_set" c_asSet :: UnionSet -> IO Set


asSet :: (Given Ctx) => UnionSet -> Set
asSet = \uset' -> trace "asSet" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_asSet uset


foreign import ccall "isl_union_set_extract_set" c_extractSet :: UnionSet -> Space -> IO Set


extractSet :: (Given Ctx) => UnionSet -> Space -> Set
extractSet = \uset' space' -> trace "extractSet" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      space <- (return) space'

      let ctx = given :: Ctx
      c_extractSet uset space


foreign import ccall "isl_union_set_params" c_params :: UnionSet -> IO Set


params :: (Given Ctx) => UnionSet -> Set
params = \uset' -> trace "params" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_params uset


foreign import ccall "isl_union_set_get_space" c_getSpace :: UnionSet -> IO Space


getSpace :: (Given Ctx) => UnionSet -> Space
getSpace = \uset' -> trace "getSpace" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_getSpace uset


foreign import ccall "isl_union_set_identity" c_identity :: UnionSet -> IO UnionMap


identity :: (Given Ctx) => UnionSet -> UnionMap
identity = \uset' -> trace "identity" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_identity uset


foreign import ccall "isl_union_set_unwrap" c_unwrap :: UnionSet -> IO UnionMap


unwrap :: (Given Ctx) => UnionSet -> UnionMap
unwrap = \uset' -> trace "unwrap" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_unwrap uset


foreign import ccall "isl_union_set_affine_hull" c_affineHull :: UnionSet -> IO UnionSet


affineHull :: (Given Ctx) => UnionSet -> UnionSet
affineHull = \uset' -> trace "affineHull" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_affineHull uset


foreign import ccall "isl_union_set_apply" c_apply :: UnionSet -> UnionMap -> IO UnionSet


apply :: (Given Ctx) => UnionSet -> UnionMap -> UnionSet
apply = \uset' umap' -> trace "apply" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_apply uset umap


foreign import ccall "isl_union_set_coalesce" c_coalesce :: UnionSet -> IO UnionSet


coalesce :: (Given Ctx) => UnionSet -> UnionSet
coalesce = \uset' -> trace "coalesce" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_coalesce uset


foreign import ccall "isl_union_set_compute_divs" c_computeDivs :: UnionSet -> IO UnionSet


computeDivs :: (Given Ctx) => UnionSet -> UnionSet
computeDivs = \uset' -> trace "computeDivs" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_computeDivs uset


foreign import ccall "isl_union_set_detect_equalities" c_detectEqualities :: UnionSet -> IO UnionSet


detectEqualities :: (Given Ctx) => UnionSet -> UnionSet
detectEqualities = \uset' -> trace "detectEqualities" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_detectEqualities uset


foreign import ccall "isl_union_set_drop_unused_params" c_dropUnusedParams :: UnionSet -> IO UnionSet


dropUnusedParams :: (Given Ctx) => UnionSet -> UnionSet
dropUnusedParams = \uset' -> trace "dropUnusedParams" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_dropUnusedParams uset


foreign import ccall "isl_union_set_gist" c_gist :: UnionSet -> UnionSet -> IO UnionSet


gist :: (Given Ctx) => UnionSet -> UnionSet -> UnionSet
gist = \uset' context' -> trace "gist" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gist uset context


foreign import ccall "isl_union_set_gist_params" c_gistParams :: UnionSet -> Set -> IO UnionSet


gistParams :: (Given Ctx) => UnionSet -> Set -> UnionSet
gistParams = \uset' set' -> trace "gistParams" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      set <- (return) set'

      let ctx = given :: Ctx
      c_gistParams uset set


foreign import ccall "isl_union_set_intersect" c_intersect :: UnionSet -> UnionSet -> IO UnionSet


intersect :: (Given Ctx) => UnionSet -> UnionSet -> UnionSet
intersect = \uset1' uset2' -> trace "intersect" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_intersect uset1 uset2


foreign import ccall "isl_union_set_intersect_params" c_intersectParams :: UnionSet -> Set -> IO UnionSet


intersectParams :: (Given Ctx) => UnionSet -> Set -> UnionSet
intersectParams = \uset' set' -> trace "intersectParams" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'
      set <- (return) set'

      let ctx = given :: Ctx
      c_intersectParams uset set


foreign import ccall "isl_union_set_lexmax" c_lexmax :: UnionSet -> IO UnionSet


lexmax :: (Given Ctx) => UnionSet -> UnionSet
lexmax = \uset' -> trace "lexmax" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_lexmax uset


foreign import ccall "isl_union_set_lexmin" c_lexmin :: UnionSet -> IO UnionSet


lexmin :: (Given Ctx) => UnionSet -> UnionSet
lexmin = \uset' -> trace "lexmin" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_lexmin uset


foreign import ccall "isl_union_set_polyhedral_hull" c_polyhedralHull :: UnionSet -> IO UnionSet


polyhedralHull :: (Given Ctx) => UnionSet -> UnionSet
polyhedralHull = \uset' -> trace "polyhedralHull" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_polyhedralHull uset


foreign import ccall "isl_union_set_project_out_all_params" c_projectOutAllParams :: UnionSet -> IO UnionSet


projectOutAllParams :: (Given Ctx) => UnionSet -> UnionSet
projectOutAllParams = \uset' -> trace "projectOutAllParams" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_projectOutAllParams uset


foreign import ccall "isl_union_set_subtract" c_subtract :: UnionSet -> UnionSet -> IO UnionSet


subtract :: (Given Ctx) => UnionSet -> UnionSet -> UnionSet
subtract = \uset1' uset2' -> trace "subtract" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_subtract uset1 uset2


foreign import ccall "isl_union_set_union" c_union :: UnionSet -> UnionSet -> IO UnionSet


union :: (Given Ctx) => UnionSet -> UnionSet -> UnionSet
union = \uset1' uset2' -> trace "union" $ 
    unsafePerformIO $ (return) =<< do
      uset1 <- (return) uset1'
      uset2 <- (return) uset2'

      let ctx = given :: Ctx
      c_union uset1 uset2


foreign import ccall "isl_union_set_universe" c_universe :: UnionSet -> IO UnionSet


universe :: (Given Ctx) => UnionSet -> UnionSet
universe = \uset' -> trace "universe" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_universe uset


foreign import ccall "isl_union_set_from_basic_set" c_fromBasicSet :: BasicSet -> IO UnionSet


fromBasicSet :: (Given Ctx) => BasicSet -> UnionSet
fromBasicSet = \bset' -> trace "fromBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_fromBasicSet bset


foreign import ccall "isl_union_set_from_set" c_fromSet :: Set -> IO UnionSet


fromSet :: (Given Ctx) => Set -> UnionSet
fromSet = \set' -> trace "fromSet" $ 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_fromSet set


foreign import ccall "isl_union_set_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO UnionSet


readFromStr :: (Given Ctx) => String -> UnionSet
readFromStr = \str' -> trace "readFromStr" $ 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


