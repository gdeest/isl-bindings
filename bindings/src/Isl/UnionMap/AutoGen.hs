{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.UnionMap.AutoGen where

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

foreign import ccall "isl_union_map_contains" c_contains :: UnionMap -> Space -> IO C.CInt


contains :: (Given Ctx) => UnionMap -> Space -> Int
contains = \umap' space' -> trace "contains" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      umap <- (return) umap'
      space <- (return) space'

      let ctx = given :: Ctx
      c_contains umap space


foreign import ccall "isl_union_map_find_dim_by_name" c_findDimByName :: UnionMap -> DimType -> C.CString -> IO C.CInt


findDimByName :: (Given Ctx) => UnionMap -> DimType -> String -> Int
findDimByName = \umap' typ' name' -> trace "findDimByName" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      umap <- (return) umap'
      typ <- (return) typ'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_findDimByName umap typ name


foreign import ccall "isl_union_map_involves_dims" c_involvesDims :: UnionMap -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt


involvesDims :: (Given Ctx) => UnionMap -> DimType -> Int -> Int -> Int
involvesDims = \umap' typ' first' n' -> trace "involvesDims" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      umap <- (return) umap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_involvesDims umap typ first n


foreign import ccall "isl_union_map_get_ctx" c_getCtx :: UnionMap -> IO Ctx


getCtx :: (Given Ctx) => UnionMap -> Ctx
getCtx = \umap' -> trace "getCtx" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_getCtx umap


foreign import ccall "isl_union_map_dump" c_dump :: UnionMap -> IO ()


dump :: (Given Ctx) => UnionMap -> ()
dump = \umap' -> trace "dump" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_dump umap


foreign import ccall "isl_union_map_is_identity" c_isIdentity :: UnionMap -> IO C.CBool


isIdentity :: (Given Ctx) => UnionMap -> Bool
isIdentity = \umap' -> trace "isIdentity" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_isIdentity umap


foreign import ccall "isl_union_map_plain_is_empty" c_plainIsEmpty :: UnionMap -> IO C.CBool


plainIsEmpty :: (Given Ctx) => UnionMap -> Bool
plainIsEmpty = \umap' -> trace "plainIsEmpty" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_plainIsEmpty umap


foreign import ccall "isl_union_map_plain_is_injective" c_plainIsInjective :: UnionMap -> IO C.CBool


plainIsInjective :: (Given Ctx) => UnionMap -> Bool
plainIsInjective = \umap' -> trace "plainIsInjective" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_plainIsInjective umap


foreign import ccall "isl_union_map_sample" c_sample :: UnionMap -> IO BasicMap


sample :: (Given Ctx) => UnionMap -> BasicMap
sample = \umap' -> trace "sample" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_sample umap


foreign import ccall "isl_union_map_add_map" c_addMap :: UnionMap -> Map -> IO UnionMap


addMap :: (Given Ctx) => UnionMap -> Map -> UnionMap
addMap = \umap' map' -> trace "addMap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      map <- (return) map'

      let ctx = given :: Ctx
      c_addMap umap map


foreign import ccall "isl_union_map_align_params" c_alignParams :: UnionMap -> Space -> IO UnionMap


alignParams :: (Given Ctx) => UnionMap -> Space -> UnionMap
alignParams = \umap' model' -> trace "alignParams" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      model <- (return) model'

      let ctx = given :: Ctx
      c_alignParams umap model


foreign import ccall "isl_union_map_copy" c_copy :: UnionMap -> IO UnionMap


copy :: (Given Ctx) => UnionMap -> UnionMap
copy = \umap' -> trace "copy" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_copy umap


foreign import ccall "isl_union_map_deltas_map" c_deltasMap :: UnionMap -> IO UnionMap


deltasMap :: (Given Ctx) => UnionMap -> UnionMap
deltasMap = \umap' -> trace "deltasMap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_deltasMap umap


foreign import ccall "isl_union_map_empty" c_empty :: Space -> IO UnionMap


empty :: (Given Ctx) => Space -> UnionMap
empty = \space' -> trace "empty" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_empty space


foreign import ccall "isl_union_map_empty_ctx" c_emptyCtx :: Ctx -> IO UnionMap


emptyCtx :: (Given Ctx) => UnionMap
emptyCtx =  trace "emptyCtx" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_emptyCtx ctx


foreign import ccall "isl_union_map_empty_space" c_emptySpace :: Space -> IO UnionMap


emptySpace :: (Given Ctx) => Space -> UnionMap
emptySpace = \space' -> trace "emptySpace" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_emptySpace space


foreign import ccall "isl_union_map_fixed_power_val" c_fixedPowerVal :: UnionMap -> Val -> IO UnionMap


fixedPowerVal :: (Given Ctx) => UnionMap -> Val -> UnionMap
fixedPowerVal = \umap' exp' -> trace "fixedPowerVal" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      exp <- (return) exp'

      let ctx = given :: Ctx
      c_fixedPowerVal umap exp


foreign import ccall "isl_union_map_flat_domain_product" c_flatDomainProduct :: UnionMap -> UnionMap -> IO UnionMap


flatDomainProduct :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
flatDomainProduct = \umap1' umap2' -> trace "flatDomainProduct" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_flatDomainProduct umap1 umap2


foreign import ccall "isl_union_map_flat_range_product" c_flatRangeProduct :: UnionMap -> UnionMap -> IO UnionMap


flatRangeProduct :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
flatRangeProduct = \umap1' umap2' -> trace "flatRangeProduct" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_flatRangeProduct umap1 umap2


foreign import ccall "isl_union_map_intersect_domain" c_intersectDomain :: UnionMap -> UnionSet -> IO UnionMap


intersectDomain :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
intersectDomain = \umap' uset' -> trace "intersectDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_intersectDomain umap uset


foreign import ccall "isl_union_map_intersect_domain_space" c_intersectDomainSpace :: UnionMap -> Space -> IO UnionMap


intersectDomainSpace :: (Given Ctx) => UnionMap -> Space -> UnionMap
intersectDomainSpace = \umap' space' -> trace "intersectDomainSpace" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      space <- (return) space'

      let ctx = given :: Ctx
      c_intersectDomainSpace umap space


foreign import ccall "isl_union_map_intersect_domain_union_set" c_intersectDomainUnionSet :: UnionMap -> UnionSet -> IO UnionMap


intersectDomainUnionSet :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
intersectDomainUnionSet = \umap' uset' -> trace "intersectDomainUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_intersectDomainUnionSet umap uset


foreign import ccall "isl_union_map_intersect_domain_wrapped_domain_union_set" c_intersectDomainWrappedDomainUnionSet :: UnionMap -> UnionSet -> IO UnionMap


intersectDomainWrappedDomainUnionSet :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
intersectDomainWrappedDomainUnionSet = \umap' domain' -> trace "intersectDomainWrappedDomainUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      domain <- (return) domain'

      let ctx = given :: Ctx
      c_intersectDomainWrappedDomainUnionSet umap domain


foreign import ccall "isl_union_map_intersect_range" c_intersectRange :: UnionMap -> UnionSet -> IO UnionMap


intersectRange :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
intersectRange = \umap' uset' -> trace "intersectRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_intersectRange umap uset


foreign import ccall "isl_union_map_intersect_range_space" c_intersectRangeSpace :: UnionMap -> Space -> IO UnionMap


intersectRangeSpace :: (Given Ctx) => UnionMap -> Space -> UnionMap
intersectRangeSpace = \umap' space' -> trace "intersectRangeSpace" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      space <- (return) space'

      let ctx = given :: Ctx
      c_intersectRangeSpace umap space


foreign import ccall "isl_union_map_intersect_range_union_set" c_intersectRangeUnionSet :: UnionMap -> UnionSet -> IO UnionMap


intersectRangeUnionSet :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
intersectRangeUnionSet = \umap' uset' -> trace "intersectRangeUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_intersectRangeUnionSet umap uset


foreign import ccall "isl_union_map_intersect_range_wrapped_domain_union_set" c_intersectRangeWrappedDomainUnionSet :: UnionMap -> UnionSet -> IO UnionMap


intersectRangeWrappedDomainUnionSet :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
intersectRangeWrappedDomainUnionSet = \umap' domain' -> trace "intersectRangeWrappedDomainUnionSet" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      domain <- (return) domain'

      let ctx = given :: Ctx
      c_intersectRangeWrappedDomainUnionSet umap domain


foreign import ccall "isl_union_map_lex_ge_union_map" c_lexGeUnionMap :: UnionMap -> UnionMap -> IO UnionMap


lexGeUnionMap :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
lexGeUnionMap = \umap1' umap2' -> trace "lexGeUnionMap" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_lexGeUnionMap umap1 umap2


foreign import ccall "isl_union_map_lex_gt_union_map" c_lexGtUnionMap :: UnionMap -> UnionMap -> IO UnionMap


lexGtUnionMap :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
lexGtUnionMap = \umap1' umap2' -> trace "lexGtUnionMap" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_lexGtUnionMap umap1 umap2


foreign import ccall "isl_union_map_lex_le_union_map" c_lexLeUnionMap :: UnionMap -> UnionMap -> IO UnionMap


lexLeUnionMap :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
lexLeUnionMap = \umap1' umap2' -> trace "lexLeUnionMap" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_lexLeUnionMap umap1 umap2


foreign import ccall "isl_union_map_lex_lt_union_map" c_lexLtUnionMap :: UnionMap -> UnionMap -> IO UnionMap


lexLtUnionMap :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
lexLtUnionMap = \umap1' umap2' -> trace "lexLtUnionMap" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_lexLtUnionMap umap1 umap2


foreign import ccall "isl_union_map_project_out" c_projectOut :: UnionMap -> DimType -> C.CUInt -> C.CUInt -> IO UnionMap


projectOut :: (Given Ctx) => UnionMap -> DimType -> Int -> Int -> UnionMap
projectOut = \umap' typ' first' n' -> trace "projectOut" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_projectOut umap typ first n


foreign import ccall "isl_union_map_project_out_param_id" c_projectOutParamId :: UnionMap -> Id -> IO UnionMap


projectOutParamId :: (Given Ctx) => UnionMap -> Id -> UnionMap
projectOutParamId = \umap' id' -> trace "projectOutParamId" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      id <- (return) id'

      let ctx = given :: Ctx
      c_projectOutParamId umap id


foreign import ccall "isl_union_map_range_curry" c_rangeCurry :: UnionMap -> IO UnionMap


rangeCurry :: (Given Ctx) => UnionMap -> UnionMap
rangeCurry = \umap' -> trace "rangeCurry" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_rangeCurry umap


foreign import ccall "isl_union_map_remove_divs" c_removeDivs :: UnionMap -> IO UnionMap


removeDivs :: (Given Ctx) => UnionMap -> UnionMap
removeDivs = \bmap' -> trace "removeDivs" $ 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_removeDivs bmap


foreign import ccall "isl_union_map_remove_redundancies" c_removeRedundancies :: UnionMap -> IO UnionMap


removeRedundancies :: (Given Ctx) => UnionMap -> UnionMap
removeRedundancies = \umap' -> trace "removeRedundancies" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_removeRedundancies umap


foreign import ccall "isl_union_map_reset_user" c_resetUser :: UnionMap -> IO UnionMap


resetUser :: (Given Ctx) => UnionMap -> UnionMap
resetUser = \umap' -> trace "resetUser" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_resetUser umap


foreign import ccall "isl_union_map_simple_hull" c_simpleHull :: UnionMap -> IO UnionMap


simpleHull :: (Given Ctx) => UnionMap -> UnionMap
simpleHull = \umap' -> trace "simpleHull" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_simpleHull umap


foreign import ccall "isl_union_map_get_dim_id" c_getDimId :: UnionMap -> DimType -> C.CUInt -> IO Id


getDimId :: (Given Ctx) => UnionMap -> DimType -> Int -> Id
getDimId = \umap' typ' pos' -> trace "getDimId" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimId umap typ pos


foreign import ccall "isl_union_map_to_str" c_toStr :: UnionMap -> IO C.CString


toStr :: (Given Ctx) => UnionMap -> String
toStr = \umap' -> trace "toStr" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_toStr umap


foreign import ccall "isl_union_map_isa_map" c_isaMap :: UnionMap -> IO C.CInt


isaMap :: (Given Ctx) => UnionMap -> Int
isaMap = \umap' -> trace "isaMap" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_isaMap umap


foreign import ccall "isl_union_map_is_bijective" c_isBijective :: UnionMap -> IO C.CBool


isBijective :: (Given Ctx) => UnionMap -> Bool
isBijective = \umap' -> trace "isBijective" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_isBijective umap


foreign import ccall "isl_union_map_is_disjoint" c_isDisjoint :: UnionMap -> UnionMap -> IO C.CBool


isDisjoint :: (Given Ctx) => UnionMap -> UnionMap -> Bool
isDisjoint = \umap1' umap2' -> trace "isDisjoint" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_isDisjoint umap1 umap2


foreign import ccall "isl_union_map_is_empty" c_isEmpty :: UnionMap -> IO C.CBool


isEmpty :: (Given Ctx) => UnionMap -> Bool
isEmpty = \umap' -> trace "isEmpty" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_isEmpty umap


foreign import ccall "isl_union_map_is_equal" c_isEqual :: UnionMap -> UnionMap -> IO C.CBool


isEqual :: (Given Ctx) => UnionMap -> UnionMap -> Bool
isEqual = \umap1' umap2' -> trace "isEqual" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_isEqual umap1 umap2


foreign import ccall "isl_union_map_is_injective" c_isInjective :: UnionMap -> IO C.CBool


isInjective :: (Given Ctx) => UnionMap -> Bool
isInjective = \umap' -> trace "isInjective" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_isInjective umap


foreign import ccall "isl_union_map_is_single_valued" c_isSingleValued :: UnionMap -> IO C.CBool


isSingleValued :: (Given Ctx) => UnionMap -> Bool
isSingleValued = \umap' -> trace "isSingleValued" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_isSingleValued umap


foreign import ccall "isl_union_map_is_strict_subset" c_isStrictSubset :: UnionMap -> UnionMap -> IO C.CBool


isStrictSubset :: (Given Ctx) => UnionMap -> UnionMap -> Bool
isStrictSubset = \umap1' umap2' -> trace "isStrictSubset" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_isStrictSubset umap1 umap2


foreign import ccall "isl_union_map_is_subset" c_isSubset :: UnionMap -> UnionMap -> IO C.CBool


isSubset :: (Given Ctx) => UnionMap -> UnionMap -> Bool
isSubset = \umap1' umap2' -> trace "isSubset" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_isSubset umap1 umap2


foreign import ccall "isl_union_map_params" c_params :: UnionMap -> IO Set


params :: (Given Ctx) => UnionMap -> Set
params = \umap' -> trace "params" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_params umap


foreign import ccall "isl_union_map_get_space" c_getSpace :: UnionMap -> IO Space


getSpace :: (Given Ctx) => UnionMap -> Space
getSpace = \umap' -> trace "getSpace" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_getSpace umap


foreign import ccall "isl_union_map_as_map" c_asMap :: UnionMap -> IO Map


asMap :: (Given Ctx) => UnionMap -> Map
asMap = \umap' -> trace "asMap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_asMap umap


foreign import ccall "isl_union_map_extract_map" c_extractMap :: UnionMap -> Space -> IO Map


extractMap :: (Given Ctx) => UnionMap -> Space -> Map
extractMap = \umap' space' -> trace "extractMap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      space <- (return) space'

      let ctx = given :: Ctx
      c_extractMap umap space


foreign import ccall "isl_union_map_affine_hull" c_affineHull :: UnionMap -> IO UnionMap


affineHull :: (Given Ctx) => UnionMap -> UnionMap
affineHull = \umap' -> trace "affineHull" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_affineHull umap


foreign import ccall "isl_union_map_apply_domain" c_applyDomain :: UnionMap -> UnionMap -> IO UnionMap


applyDomain :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
applyDomain = \umap1' umap2' -> trace "applyDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_applyDomain umap1 umap2


foreign import ccall "isl_union_map_apply_range" c_applyRange :: UnionMap -> UnionMap -> IO UnionMap


applyRange :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
applyRange = \umap1' umap2' -> trace "applyRange" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_applyRange umap1 umap2


foreign import ccall "isl_union_map_coalesce" c_coalesce :: UnionMap -> IO UnionMap


coalesce :: (Given Ctx) => UnionMap -> UnionMap
coalesce = \umap' -> trace "coalesce" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_coalesce umap


foreign import ccall "isl_union_map_compute_divs" c_computeDivs :: UnionMap -> IO UnionMap


computeDivs :: (Given Ctx) => UnionMap -> UnionMap
computeDivs = \umap' -> trace "computeDivs" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_computeDivs umap


foreign import ccall "isl_union_map_curry" c_curry :: UnionMap -> IO UnionMap


curry :: (Given Ctx) => UnionMap -> UnionMap
curry = \umap' -> trace "curry" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_curry umap


foreign import ccall "isl_union_map_detect_equalities" c_detectEqualities :: UnionMap -> IO UnionMap


detectEqualities :: (Given Ctx) => UnionMap -> UnionMap
detectEqualities = \umap' -> trace "detectEqualities" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_detectEqualities umap


foreign import ccall "isl_union_map_domain_factor_domain" c_domainFactorDomain :: UnionMap -> IO UnionMap


domainFactorDomain :: (Given Ctx) => UnionMap -> UnionMap
domainFactorDomain = \umap' -> trace "domainFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_domainFactorDomain umap


foreign import ccall "isl_union_map_domain_factor_range" c_domainFactorRange :: UnionMap -> IO UnionMap


domainFactorRange :: (Given Ctx) => UnionMap -> UnionMap
domainFactorRange = \umap' -> trace "domainFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_domainFactorRange umap


foreign import ccall "isl_union_map_domain_map" c_domainMap :: UnionMap -> IO UnionMap


domainMap :: (Given Ctx) => UnionMap -> UnionMap
domainMap = \umap' -> trace "domainMap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_domainMap umap


foreign import ccall "isl_union_map_domain_product" c_domainProduct :: UnionMap -> UnionMap -> IO UnionMap


domainProduct :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
domainProduct = \umap1' umap2' -> trace "domainProduct" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_domainProduct umap1 umap2


foreign import ccall "isl_union_map_domain_reverse" c_domainReverse :: UnionMap -> IO UnionMap


domainReverse :: (Given Ctx) => UnionMap -> UnionMap
domainReverse = \umap' -> trace "domainReverse" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_domainReverse umap


foreign import ccall "isl_union_map_drop_unused_params" c_dropUnusedParams :: UnionMap -> IO UnionMap


dropUnusedParams :: (Given Ctx) => UnionMap -> UnionMap
dropUnusedParams = \umap' -> trace "dropUnusedParams" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_dropUnusedParams umap


foreign import ccall "isl_union_map_factor_domain" c_factorDomain :: UnionMap -> IO UnionMap


factorDomain :: (Given Ctx) => UnionMap -> UnionMap
factorDomain = \umap' -> trace "factorDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_factorDomain umap


foreign import ccall "isl_union_map_factor_range" c_factorRange :: UnionMap -> IO UnionMap


factorRange :: (Given Ctx) => UnionMap -> UnionMap
factorRange = \umap' -> trace "factorRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_factorRange umap


foreign import ccall "isl_union_map_from_domain" c_fromDomain :: UnionSet -> IO UnionMap


fromDomain :: (Given Ctx) => UnionSet -> UnionMap
fromDomain = \uset' -> trace "fromDomain" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_fromDomain uset


foreign import ccall "isl_union_map_from_domain_and_range" c_fromDomainAndRange :: UnionSet -> UnionSet -> IO UnionMap


fromDomainAndRange :: (Given Ctx) => UnionSet -> UnionSet -> UnionMap
fromDomainAndRange = \domain' range' -> trace "fromDomainAndRange" $ 
    unsafePerformIO $ (return) =<< do
      domain <- (return) domain'
      range <- (return) range'

      let ctx = given :: Ctx
      c_fromDomainAndRange domain range


foreign import ccall "isl_union_map_from_range" c_fromRange :: UnionSet -> IO UnionMap


fromRange :: (Given Ctx) => UnionSet -> UnionMap
fromRange = \uset' -> trace "fromRange" $ 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_fromRange uset


foreign import ccall "isl_union_map_gist" c_gist :: UnionMap -> UnionMap -> IO UnionMap


gist :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
gist = \umap' context' -> trace "gist" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gist umap context


foreign import ccall "isl_union_map_gist_domain" c_gistDomain :: UnionMap -> UnionSet -> IO UnionMap


gistDomain :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
gistDomain = \umap' uset' -> trace "gistDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_gistDomain umap uset


foreign import ccall "isl_union_map_gist_params" c_gistParams :: UnionMap -> Set -> IO UnionMap


gistParams :: (Given Ctx) => UnionMap -> Set -> UnionMap
gistParams = \umap' set' -> trace "gistParams" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      set <- (return) set'

      let ctx = given :: Ctx
      c_gistParams umap set


foreign import ccall "isl_union_map_gist_range" c_gistRange :: UnionMap -> UnionSet -> IO UnionMap


gistRange :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
gistRange = \umap' uset' -> trace "gistRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_gistRange umap uset


foreign import ccall "isl_union_map_intersect" c_intersect :: UnionMap -> UnionMap -> IO UnionMap


intersect :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
intersect = \umap1' umap2' -> trace "intersect" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_intersect umap1 umap2


foreign import ccall "isl_union_map_intersect_domain_factor_domain" c_intersectDomainFactorDomain :: UnionMap -> UnionMap -> IO UnionMap


intersectDomainFactorDomain :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
intersectDomainFactorDomain = \umap' factor' -> trace "intersectDomainFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectDomainFactorDomain umap factor


foreign import ccall "isl_union_map_intersect_domain_factor_range" c_intersectDomainFactorRange :: UnionMap -> UnionMap -> IO UnionMap


intersectDomainFactorRange :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
intersectDomainFactorRange = \umap' factor' -> trace "intersectDomainFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectDomainFactorRange umap factor


foreign import ccall "isl_union_map_intersect_params" c_intersectParams :: UnionMap -> Set -> IO UnionMap


intersectParams :: (Given Ctx) => UnionMap -> Set -> UnionMap
intersectParams = \umap' set' -> trace "intersectParams" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      set <- (return) set'

      let ctx = given :: Ctx
      c_intersectParams umap set


foreign import ccall "isl_union_map_intersect_range_factor_domain" c_intersectRangeFactorDomain :: UnionMap -> UnionMap -> IO UnionMap


intersectRangeFactorDomain :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
intersectRangeFactorDomain = \umap' factor' -> trace "intersectRangeFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectRangeFactorDomain umap factor


foreign import ccall "isl_union_map_intersect_range_factor_range" c_intersectRangeFactorRange :: UnionMap -> UnionMap -> IO UnionMap


intersectRangeFactorRange :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
intersectRangeFactorRange = \umap' factor' -> trace "intersectRangeFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectRangeFactorRange umap factor


foreign import ccall "isl_union_map_lexmax" c_lexmax :: UnionMap -> IO UnionMap


lexmax :: (Given Ctx) => UnionMap -> UnionMap
lexmax = \umap' -> trace "lexmax" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_lexmax umap


foreign import ccall "isl_union_map_lexmin" c_lexmin :: UnionMap -> IO UnionMap


lexmin :: (Given Ctx) => UnionMap -> UnionMap
lexmin = \umap' -> trace "lexmin" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_lexmin umap


foreign import ccall "isl_union_map_polyhedral_hull" c_polyhedralHull :: UnionMap -> IO UnionMap


polyhedralHull :: (Given Ctx) => UnionMap -> UnionMap
polyhedralHull = \umap' -> trace "polyhedralHull" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_polyhedralHull umap


foreign import ccall "isl_union_map_product" c_product :: UnionMap -> UnionMap -> IO UnionMap


product :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
product = \umap1' umap2' -> trace "product" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_product umap1 umap2


foreign import ccall "isl_union_map_project_out_all_params" c_projectOutAllParams :: UnionMap -> IO UnionMap


projectOutAllParams :: (Given Ctx) => UnionMap -> UnionMap
projectOutAllParams = \umap' -> trace "projectOutAllParams" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_projectOutAllParams umap


foreign import ccall "isl_union_map_range_factor_domain" c_rangeFactorDomain :: UnionMap -> IO UnionMap


rangeFactorDomain :: (Given Ctx) => UnionMap -> UnionMap
rangeFactorDomain = \umap' -> trace "rangeFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_rangeFactorDomain umap


foreign import ccall "isl_union_map_range_factor_range" c_rangeFactorRange :: UnionMap -> IO UnionMap


rangeFactorRange :: (Given Ctx) => UnionMap -> UnionMap
rangeFactorRange = \umap' -> trace "rangeFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_rangeFactorRange umap


foreign import ccall "isl_union_map_range_map" c_rangeMap :: UnionMap -> IO UnionMap


rangeMap :: (Given Ctx) => UnionMap -> UnionMap
rangeMap = \umap' -> trace "rangeMap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_rangeMap umap


foreign import ccall "isl_union_map_range_product" c_rangeProduct :: UnionMap -> UnionMap -> IO UnionMap


rangeProduct :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
rangeProduct = \umap1' umap2' -> trace "rangeProduct" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_rangeProduct umap1 umap2


foreign import ccall "isl_union_map_range_reverse" c_rangeReverse :: UnionMap -> IO UnionMap


rangeReverse :: (Given Ctx) => UnionMap -> UnionMap
rangeReverse = \umap' -> trace "rangeReverse" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_rangeReverse umap


foreign import ccall "isl_union_map_reverse" c_reverse :: UnionMap -> IO UnionMap


reverse :: (Given Ctx) => UnionMap -> UnionMap
reverse = \umap' -> trace "reverse" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_reverse umap


foreign import ccall "isl_union_map_subtract" c_subtract :: UnionMap -> UnionMap -> IO UnionMap


subtract :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
subtract = \umap1' umap2' -> trace "subtract" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_subtract umap1 umap2


foreign import ccall "isl_union_map_subtract_domain" c_subtractDomain :: UnionMap -> UnionSet -> IO UnionMap


subtractDomain :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
subtractDomain = \umap' dom' -> trace "subtractDomain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      dom <- (return) dom'

      let ctx = given :: Ctx
      c_subtractDomain umap dom


foreign import ccall "isl_union_map_subtract_range" c_subtractRange :: UnionMap -> UnionSet -> IO UnionMap


subtractRange :: (Given Ctx) => UnionMap -> UnionSet -> UnionMap
subtractRange = \umap' dom' -> trace "subtractRange" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'
      dom <- (return) dom'

      let ctx = given :: Ctx
      c_subtractRange umap dom


foreign import ccall "isl_union_map_uncurry" c_uncurry :: UnionMap -> IO UnionMap


uncurry :: (Given Ctx) => UnionMap -> UnionMap
uncurry = \umap' -> trace "uncurry" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_uncurry umap


foreign import ccall "isl_union_map_union" c_union :: UnionMap -> UnionMap -> IO UnionMap


union :: (Given Ctx) => UnionMap -> UnionMap -> UnionMap
union = \umap1' umap2' -> trace "union" $ 
    unsafePerformIO $ (return) =<< do
      umap1 <- (return) umap1'
      umap2 <- (return) umap2'

      let ctx = given :: Ctx
      c_union umap1 umap2


foreign import ccall "isl_union_map_universe" c_universe :: UnionMap -> IO UnionMap


universe :: (Given Ctx) => UnionMap -> UnionMap
universe = \umap' -> trace "universe" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_universe umap


foreign import ccall "isl_union_map_zip" c_zip :: UnionMap -> IO UnionMap


zip :: (Given Ctx) => UnionMap -> UnionMap
zip = \umap' -> trace "zip" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_zip umap


foreign import ccall "isl_union_map_deltas" c_deltas :: UnionMap -> IO UnionSet


deltas :: (Given Ctx) => UnionMap -> UnionSet
deltas = \umap' -> trace "deltas" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_deltas umap


foreign import ccall "isl_union_map_domain" c_domain :: UnionMap -> IO UnionSet


domain :: (Given Ctx) => UnionMap -> UnionSet
domain = \umap' -> trace "domain" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_domain umap


foreign import ccall "isl_union_map_range" c_range :: UnionMap -> IO UnionSet


range :: (Given Ctx) => UnionMap -> UnionSet
range = \umap' -> trace "range" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_range umap


foreign import ccall "isl_union_map_wrap" c_wrap :: UnionMap -> IO UnionSet


wrap :: (Given Ctx) => UnionMap -> UnionSet
wrap = \umap' -> trace "wrap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_wrap umap


foreign import ccall "isl_union_map_from_basic_map" c_fromBasicMap :: BasicMap -> IO UnionMap


fromBasicMap :: (Given Ctx) => BasicMap -> UnionMap
fromBasicMap = \bmap' -> trace "fromBasicMap" $ 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_fromBasicMap bmap


foreign import ccall "isl_union_map_from_map" c_fromMap :: Map -> IO UnionMap


fromMap :: (Given Ctx) => Map -> UnionMap
fromMap = \map' -> trace "fromMap" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_fromMap map


foreign import ccall "isl_union_map_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO UnionMap


readFromStr :: (Given Ctx) => String -> UnionMap
readFromStr = \str' -> trace "readFromStr" $ 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


