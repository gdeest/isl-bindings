{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.UnionMap.Generated where

import Isl.Types
import Isl.Monad
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_union_map_contains" c_contains :: UnionMapRef -> SpaceRef -> IO C.CInt

contains :: UnionMapRef -> SpaceRef -> Int
contains umap space =
    unsafePerformIO $ fromIntegral <$> c_contains umap space


foreign import ccall "isl_union_map_dim" c_dim :: UnionMapRef -> DimType -> IO C.CInt

dim :: UnionMapRef -> DimType -> Int
dim umap typ =
    unsafePerformIO $ fromIntegral <$> c_dim umap typ


foreign import ccall "isl_union_map_find_dim_by_name" c_findDimByName :: UnionMapRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: UnionMapRef -> DimType -> String -> Int
findDimByName umap typ name =
    unsafePerformIO $ do
      name_c <- C.newCString name
      fromIntegral <$> c_findDimByName umap typ name_c


foreign import ccall "isl_union_map_involves_dims" c_involvesDims :: UnionMapRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: UnionMapRef -> DimType -> Int -> Int -> Int
involvesDims umap typ first n =
    unsafePerformIO $ fromIntegral <$> c_involvesDims umap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_union_map_n_map" c_nMap :: UnionMapRef -> IO C.CInt

nMap :: UnionMapRef -> Int
nMap umap =
    unsafePerformIO $ fromIntegral <$> c_nMap umap


foreign import ccall "isl_union_map_dump" c_dump :: UnionMapRef -> IO ()

dump :: UnionMapRef -> ()
dump umap =
    unsafePerformIO $ c_dump umap


foreign import ccall "isl_union_map_is_identity" c_isIdentity :: UnionMapRef -> IO C.CBool

isIdentity :: UnionMapRef -> Bool
isIdentity umap =
    unsafePerformIO $ M.toBool <$> c_isIdentity umap


foreign import ccall "isl_union_map_plain_is_empty" c_plainIsEmpty :: UnionMapRef -> IO C.CBool

plainIsEmpty :: UnionMapRef -> Bool
plainIsEmpty umap =
    unsafePerformIO $ M.toBool <$> c_plainIsEmpty umap


foreign import ccall "isl_union_map_plain_is_injective" c_plainIsInjective :: UnionMapRef -> IO C.CBool

plainIsInjective :: UnionMapRef -> Bool
plainIsInjective umap =
    unsafePerformIO $ M.toBool <$> c_plainIsInjective umap


foreign import ccall "isl_union_map_sample" c_sample :: UnionMap -> IO BasicMap

sample :: forall m. MonadIO m => UnionMap %1 -> IslT m BasicMap
sample = unsafeCoerce go where
  go :: UnionMap -> IslT m BasicMap
  go umap =
    unsafeIslFromIO $ \_ -> c_sample umap


foreign import ccall "isl_union_map_add_map" c_addMap :: UnionMap -> Map -> IO UnionMap

addMap :: forall m. MonadIO m => UnionMap %1 -> Map %1 -> IslT m UnionMap
addMap = unsafeCoerce go where
  go :: UnionMap -> Map -> IslT m UnionMap
  go umap map =
    unsafeIslFromIO $ \_ -> c_addMap umap map


foreign import ccall "isl_union_map_align_params" c_alignParams :: UnionMap -> Space -> IO UnionMap

alignParams :: forall m. MonadIO m => UnionMap %1 -> Space %1 -> IslT m UnionMap
alignParams = unsafeCoerce go where
  go :: UnionMap -> Space -> IslT m UnionMap
  go umap model =
    unsafeIslFromIO $ \_ -> c_alignParams umap model


foreign import ccall "isl_union_map_deltas_map" c_deltasMap :: UnionMap -> IO UnionMap

deltasMap :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
deltasMap = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_deltasMap umap


foreign import ccall "isl_union_map_empty" c_empty :: Space -> IO UnionMap

empty :: forall m. MonadIO m => Space %1 -> IslT m UnionMap
empty = unsafeCoerce go where
  go :: Space -> IslT m UnionMap
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_union_map_empty_ctx" c_emptyCtx :: Ctx -> IO UnionMap

emptyCtx :: MonadIO m => IslT m UnionMap
emptyCtx =
    unsafeIslFromIO $ \ctx -> c_emptyCtx ctx


foreign import ccall "isl_union_map_empty_space" c_emptySpace :: Space -> IO UnionMap

emptySpace :: forall m. MonadIO m => Space %1 -> IslT m UnionMap
emptySpace = unsafeCoerce go where
  go :: Space -> IslT m UnionMap
  go space =
    unsafeIslFromIO $ \_ -> c_emptySpace space


foreign import ccall "isl_union_map_fixed_power_val" c_fixedPowerVal :: UnionMap -> Val -> IO UnionMap

fixedPowerVal :: forall m. MonadIO m => UnionMap %1 -> Val %1 -> IslT m UnionMap
fixedPowerVal = unsafeCoerce go where
  go :: UnionMap -> Val -> IslT m UnionMap
  go umap exp =
    unsafeIslFromIO $ \_ -> c_fixedPowerVal umap exp


foreign import ccall "isl_union_map_flat_domain_product" c_flatDomainProduct :: UnionMap -> UnionMap -> IO UnionMap

flatDomainProduct :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
flatDomainProduct = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_flatDomainProduct umap1 umap2


foreign import ccall "isl_union_map_flat_range_product" c_flatRangeProduct :: UnionMap -> UnionMap -> IO UnionMap

flatRangeProduct :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
flatRangeProduct = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_flatRangeProduct umap1 umap2


foreign import ccall "isl_union_map_intersect_domain" c_intersectDomain :: UnionMap -> UnionSet -> IO UnionMap

intersectDomain :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectDomain = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap uset =
    unsafeIslFromIO $ \_ -> c_intersectDomain umap uset


foreign import ccall "isl_union_map_intersect_domain_space" c_intersectDomainSpace :: UnionMap -> Space -> IO UnionMap

intersectDomainSpace :: forall m. MonadIO m => UnionMap %1 -> Space %1 -> IslT m UnionMap
intersectDomainSpace = unsafeCoerce go where
  go :: UnionMap -> Space -> IslT m UnionMap
  go umap space =
    unsafeIslFromIO $ \_ -> c_intersectDomainSpace umap space


foreign import ccall "isl_union_map_intersect_domain_union_set" c_intersectDomainUnionSet :: UnionMap -> UnionSet -> IO UnionMap

intersectDomainUnionSet :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectDomainUnionSet = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap uset =
    unsafeIslFromIO $ \_ -> c_intersectDomainUnionSet umap uset


foreign import ccall "isl_union_map_intersect_domain_wrapped_domain_union_set" c_intersectDomainWrappedDomainUnionSet :: UnionMap -> UnionSet -> IO UnionMap

intersectDomainWrappedDomainUnionSet :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectDomainWrappedDomainUnionSet = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap domain =
    unsafeIslFromIO $ \_ -> c_intersectDomainWrappedDomainUnionSet umap domain


foreign import ccall "isl_union_map_intersect_range" c_intersectRange :: UnionMap -> UnionSet -> IO UnionMap

intersectRange :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectRange = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap uset =
    unsafeIslFromIO $ \_ -> c_intersectRange umap uset


foreign import ccall "isl_union_map_intersect_range_space" c_intersectRangeSpace :: UnionMap -> Space -> IO UnionMap

intersectRangeSpace :: forall m. MonadIO m => UnionMap %1 -> Space %1 -> IslT m UnionMap
intersectRangeSpace = unsafeCoerce go where
  go :: UnionMap -> Space -> IslT m UnionMap
  go umap space =
    unsafeIslFromIO $ \_ -> c_intersectRangeSpace umap space


foreign import ccall "isl_union_map_intersect_range_union_set" c_intersectRangeUnionSet :: UnionMap -> UnionSet -> IO UnionMap

intersectRangeUnionSet :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectRangeUnionSet = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap uset =
    unsafeIslFromIO $ \_ -> c_intersectRangeUnionSet umap uset


foreign import ccall "isl_union_map_intersect_range_wrapped_domain_union_set" c_intersectRangeWrappedDomainUnionSet :: UnionMap -> UnionSet -> IO UnionMap

intersectRangeWrappedDomainUnionSet :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectRangeWrappedDomainUnionSet = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap domain =
    unsafeIslFromIO $ \_ -> c_intersectRangeWrappedDomainUnionSet umap domain


foreign import ccall "isl_union_map_lex_ge_union_map" c_lexGeUnionMap :: UnionMap -> UnionMap -> IO UnionMap

lexGeUnionMap :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
lexGeUnionMap = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_lexGeUnionMap umap1 umap2


foreign import ccall "isl_union_map_lex_gt_union_map" c_lexGtUnionMap :: UnionMap -> UnionMap -> IO UnionMap

lexGtUnionMap :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
lexGtUnionMap = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_lexGtUnionMap umap1 umap2


foreign import ccall "isl_union_map_lex_le_union_map" c_lexLeUnionMap :: UnionMap -> UnionMap -> IO UnionMap

lexLeUnionMap :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
lexLeUnionMap = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_lexLeUnionMap umap1 umap2


foreign import ccall "isl_union_map_lex_lt_union_map" c_lexLtUnionMap :: UnionMap -> UnionMap -> IO UnionMap

lexLtUnionMap :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
lexLtUnionMap = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_lexLtUnionMap umap1 umap2


foreign import ccall "isl_union_map_project_out" c_projectOut :: UnionMap -> DimType -> C.CUInt -> C.CUInt -> IO UnionMap

projectOut :: forall m. MonadIO m => UnionMap %1 -> DimType -> Int -> Int -> IslT m UnionMap
projectOut = unsafeCoerce go where
  go :: UnionMap -> DimType -> Int -> Int -> IslT m UnionMap
  go umap typ first n =
    unsafeIslFromIO $ \_ -> c_projectOut umap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_union_map_project_out_param_id" c_projectOutParamId :: UnionMap -> Id -> IO UnionMap

projectOutParamId :: forall m. MonadIO m => UnionMap %1 -> Id %1 -> IslT m UnionMap
projectOutParamId = unsafeCoerce go where
  go :: UnionMap -> Id -> IslT m UnionMap
  go umap id =
    unsafeIslFromIO $ \_ -> c_projectOutParamId umap id


foreign import ccall "isl_union_map_range_curry" c_rangeCurry :: UnionMap -> IO UnionMap

rangeCurry :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
rangeCurry = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_rangeCurry umap


foreign import ccall "isl_union_map_remove_divs" c_removeDivs :: UnionMap -> IO UnionMap

removeDivs :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
removeDivs = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_removeDivs bmap


foreign import ccall "isl_union_map_remove_redundancies" c_removeRedundancies :: UnionMap -> IO UnionMap

removeRedundancies :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
removeRedundancies = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_removeRedundancies umap


foreign import ccall "isl_union_map_reset_user" c_resetUser :: UnionMap -> IO UnionMap

resetUser :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
resetUser = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_resetUser umap


foreign import ccall "isl_union_map_simple_hull" c_simpleHull :: UnionMap -> IO UnionMap

simpleHull :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
simpleHull = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_simpleHull umap


foreign import ccall "isl_union_map_get_dim_id" c_getDimId :: UnionMapRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => UnionMapRef -> DimType -> Int -> IslT m Id
getDimId umap typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId umap typ (fromIntegral pos)


foreign import ccall "isl_union_map_to_str" c_toStr :: UnionMapRef -> IO C.CString

toStr :: UnionMapRef -> String
toStr umap =
    unsafePerformIO $ C.peekCString =<< c_toStr umap


foreign import ccall "isl_union_map_isa_map" c_isaMap :: UnionMapRef -> IO C.CInt

isaMap :: UnionMapRef -> Int
isaMap umap =
    unsafePerformIO $ fromIntegral <$> c_isaMap umap


foreign import ccall "isl_union_map_is_bijective" c_isBijective :: UnionMapRef -> IO C.CBool

isBijective :: UnionMapRef -> Bool
isBijective umap =
    unsafePerformIO $ M.toBool <$> c_isBijective umap


foreign import ccall "isl_union_map_is_disjoint" c_isDisjoint :: UnionMapRef -> UnionMapRef -> IO C.CBool

isDisjoint :: UnionMapRef -> UnionMapRef -> Bool
isDisjoint umap1 umap2 =
    unsafePerformIO $ M.toBool <$> c_isDisjoint umap1 umap2


foreign import ccall "isl_union_map_is_empty" c_isEmpty :: UnionMapRef -> IO C.CBool

isEmpty :: UnionMapRef -> Bool
isEmpty umap =
    unsafePerformIO $ M.toBool <$> c_isEmpty umap


foreign import ccall "isl_union_map_is_equal" c_isEqual :: UnionMapRef -> UnionMapRef -> IO C.CBool

isEqual :: UnionMapRef -> UnionMapRef -> Bool
isEqual umap1 umap2 =
    unsafePerformIO $ M.toBool <$> c_isEqual umap1 umap2


foreign import ccall "isl_union_map_is_injective" c_isInjective :: UnionMapRef -> IO C.CBool

isInjective :: UnionMapRef -> Bool
isInjective umap =
    unsafePerformIO $ M.toBool <$> c_isInjective umap


foreign import ccall "isl_union_map_is_single_valued" c_isSingleValued :: UnionMapRef -> IO C.CBool

isSingleValued :: UnionMapRef -> Bool
isSingleValued umap =
    unsafePerformIO $ M.toBool <$> c_isSingleValued umap


foreign import ccall "isl_union_map_is_strict_subset" c_isStrictSubset :: UnionMapRef -> UnionMapRef -> IO C.CBool

isStrictSubset :: UnionMapRef -> UnionMapRef -> Bool
isStrictSubset umap1 umap2 =
    unsafePerformIO $ M.toBool <$> c_isStrictSubset umap1 umap2


foreign import ccall "isl_union_map_is_subset" c_isSubset :: UnionMapRef -> UnionMapRef -> IO C.CBool

isSubset :: UnionMapRef -> UnionMapRef -> Bool
isSubset umap1 umap2 =
    unsafePerformIO $ M.toBool <$> c_isSubset umap1 umap2


foreign import ccall "isl_union_map_params" c_params :: UnionMap -> IO Set

params :: forall m. MonadIO m => UnionMap %1 -> IslT m Set
params = unsafeCoerce go where
  go :: UnionMap -> IslT m Set
  go umap =
    unsafeIslFromIO $ \_ -> c_params umap


foreign import ccall "isl_union_map_get_space" c_getSpace :: UnionMapRef -> IO Space

getSpace :: MonadIO m => UnionMapRef -> IslT m Space
getSpace umap =
    unsafeIslFromIO $ \_ -> c_getSpace umap


foreign import ccall "isl_union_map_as_map" c_asMap :: UnionMap -> IO Map

asMap :: forall m. MonadIO m => UnionMap %1 -> IslT m Map
asMap = unsafeCoerce go where
  go :: UnionMap -> IslT m Map
  go umap =
    unsafeIslFromIO $ \_ -> c_asMap umap


foreign import ccall "isl_union_map_extract_map" c_extractMap :: UnionMapRef -> Space -> IO Map

extractMap :: forall m. MonadIO m => UnionMapRef -> Space %1 -> IslT m Map
extractMap = unsafeCoerce go where
  go :: UnionMapRef -> Space -> IslT m Map
  go umap space =
    unsafeIslFromIO $ \_ -> c_extractMap umap space


foreign import ccall "isl_union_map_affine_hull" c_affineHull :: UnionMap -> IO UnionMap

affineHull :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
affineHull = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_affineHull umap


foreign import ccall "isl_union_map_apply_domain" c_applyDomain :: UnionMap -> UnionMap -> IO UnionMap

applyDomain :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
applyDomain = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_applyDomain umap1 umap2


foreign import ccall "isl_union_map_apply_range" c_applyRange :: UnionMap -> UnionMap -> IO UnionMap

applyRange :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
applyRange = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_applyRange umap1 umap2


foreign import ccall "isl_union_map_coalesce" c_coalesce :: UnionMap -> IO UnionMap

coalesce :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
coalesce = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_coalesce umap


foreign import ccall "isl_union_map_compute_divs" c_computeDivs :: UnionMap -> IO UnionMap

computeDivs :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
computeDivs = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_computeDivs umap


foreign import ccall "isl_union_map_curry" c_curry :: UnionMap -> IO UnionMap

curry :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
curry = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_curry umap


foreign import ccall "isl_union_map_detect_equalities" c_detectEqualities :: UnionMap -> IO UnionMap

detectEqualities :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
detectEqualities = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_detectEqualities umap


foreign import ccall "isl_union_map_domain_factor_domain" c_domainFactorDomain :: UnionMap -> IO UnionMap

domainFactorDomain :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
domainFactorDomain = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_domainFactorDomain umap


foreign import ccall "isl_union_map_domain_factor_range" c_domainFactorRange :: UnionMap -> IO UnionMap

domainFactorRange :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
domainFactorRange = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_domainFactorRange umap


foreign import ccall "isl_union_map_domain_map" c_domainMap :: UnionMap -> IO UnionMap

domainMap :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
domainMap = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_domainMap umap


foreign import ccall "isl_union_map_domain_product" c_domainProduct :: UnionMap -> UnionMap -> IO UnionMap

domainProduct :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
domainProduct = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_domainProduct umap1 umap2


foreign import ccall "isl_union_map_domain_reverse" c_domainReverse :: UnionMap -> IO UnionMap

domainReverse :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
domainReverse = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_domainReverse umap


foreign import ccall "isl_union_map_drop_unused_params" c_dropUnusedParams :: UnionMap -> IO UnionMap

dropUnusedParams :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
dropUnusedParams = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams umap


foreign import ccall "isl_union_map_factor_domain" c_factorDomain :: UnionMap -> IO UnionMap

factorDomain :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
factorDomain = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_factorDomain umap


foreign import ccall "isl_union_map_factor_range" c_factorRange :: UnionMap -> IO UnionMap

factorRange :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
factorRange = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_factorRange umap


foreign import ccall "isl_union_map_from_domain" c_fromDomain :: UnionSet -> IO UnionMap

fromDomain :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionMap
fromDomain = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionMap
  go uset =
    unsafeIslFromIO $ \_ -> c_fromDomain uset


foreign import ccall "isl_union_map_from_domain_and_range" c_fromDomainAndRange :: UnionSet -> UnionSet -> IO UnionMap

fromDomainAndRange :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionMap
fromDomainAndRange = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionMap
  go domain range =
    unsafeIslFromIO $ \_ -> c_fromDomainAndRange domain range


foreign import ccall "isl_union_map_from_range" c_fromRange :: UnionSet -> IO UnionMap

fromRange :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionMap
fromRange = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionMap
  go uset =
    unsafeIslFromIO $ \_ -> c_fromRange uset


foreign import ccall "isl_union_map_gist" c_gist :: UnionMap -> UnionMap -> IO UnionMap

gist :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
gist = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap context =
    unsafeIslFromIO $ \_ -> c_gist umap context


foreign import ccall "isl_union_map_gist_domain" c_gistDomain :: UnionMap -> UnionSet -> IO UnionMap

gistDomain :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
gistDomain = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap uset =
    unsafeIslFromIO $ \_ -> c_gistDomain umap uset


foreign import ccall "isl_union_map_gist_params" c_gistParams :: UnionMap -> Set -> IO UnionMap

gistParams :: forall m. MonadIO m => UnionMap %1 -> Set %1 -> IslT m UnionMap
gistParams = unsafeCoerce go where
  go :: UnionMap -> Set -> IslT m UnionMap
  go umap set =
    unsafeIslFromIO $ \_ -> c_gistParams umap set


foreign import ccall "isl_union_map_gist_range" c_gistRange :: UnionMap -> UnionSet -> IO UnionMap

gistRange :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
gistRange = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap uset =
    unsafeIslFromIO $ \_ -> c_gistRange umap uset


foreign import ccall "isl_union_map_intersect" c_intersect :: UnionMap -> UnionMap -> IO UnionMap

intersect :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
intersect = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_intersect umap1 umap2


foreign import ccall "isl_union_map_intersect_domain_factor_domain" c_intersectDomainFactorDomain :: UnionMap -> UnionMap -> IO UnionMap

intersectDomainFactorDomain :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
intersectDomainFactorDomain = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap factor =
    unsafeIslFromIO $ \_ -> c_intersectDomainFactorDomain umap factor


foreign import ccall "isl_union_map_intersect_domain_factor_range" c_intersectDomainFactorRange :: UnionMap -> UnionMap -> IO UnionMap

intersectDomainFactorRange :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
intersectDomainFactorRange = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap factor =
    unsafeIslFromIO $ \_ -> c_intersectDomainFactorRange umap factor


foreign import ccall "isl_union_map_intersect_params" c_intersectParams :: UnionMap -> Set -> IO UnionMap

intersectParams :: forall m. MonadIO m => UnionMap %1 -> Set %1 -> IslT m UnionMap
intersectParams = unsafeCoerce go where
  go :: UnionMap -> Set -> IslT m UnionMap
  go umap set =
    unsafeIslFromIO $ \_ -> c_intersectParams umap set


foreign import ccall "isl_union_map_intersect_range_factor_domain" c_intersectRangeFactorDomain :: UnionMap -> UnionMap -> IO UnionMap

intersectRangeFactorDomain :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
intersectRangeFactorDomain = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap factor =
    unsafeIslFromIO $ \_ -> c_intersectRangeFactorDomain umap factor


foreign import ccall "isl_union_map_intersect_range_factor_range" c_intersectRangeFactorRange :: UnionMap -> UnionMap -> IO UnionMap

intersectRangeFactorRange :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
intersectRangeFactorRange = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap factor =
    unsafeIslFromIO $ \_ -> c_intersectRangeFactorRange umap factor


foreign import ccall "isl_union_map_lexmax" c_lexmax :: UnionMap -> IO UnionMap

lexmax :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
lexmax = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_lexmax umap


foreign import ccall "isl_union_map_lexmin" c_lexmin :: UnionMap -> IO UnionMap

lexmin :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
lexmin = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_lexmin umap


foreign import ccall "isl_union_map_polyhedral_hull" c_polyhedralHull :: UnionMap -> IO UnionMap

polyhedralHull :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
polyhedralHull = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_polyhedralHull umap


foreign import ccall "isl_union_map_product" c_product :: UnionMap -> UnionMap -> IO UnionMap

product :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
product = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_product umap1 umap2


foreign import ccall "isl_union_map_project_out_all_params" c_projectOutAllParams :: UnionMap -> IO UnionMap

projectOutAllParams :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
projectOutAllParams = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_projectOutAllParams umap


foreign import ccall "isl_union_map_range_factor_domain" c_rangeFactorDomain :: UnionMap -> IO UnionMap

rangeFactorDomain :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
rangeFactorDomain = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_rangeFactorDomain umap


foreign import ccall "isl_union_map_range_factor_range" c_rangeFactorRange :: UnionMap -> IO UnionMap

rangeFactorRange :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
rangeFactorRange = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_rangeFactorRange umap


foreign import ccall "isl_union_map_range_map" c_rangeMap :: UnionMap -> IO UnionMap

rangeMap :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
rangeMap = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_rangeMap umap


foreign import ccall "isl_union_map_range_product" c_rangeProduct :: UnionMap -> UnionMap -> IO UnionMap

rangeProduct :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
rangeProduct = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_rangeProduct umap1 umap2


foreign import ccall "isl_union_map_range_reverse" c_rangeReverse :: UnionMap -> IO UnionMap

rangeReverse :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
rangeReverse = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_rangeReverse umap


foreign import ccall "isl_union_map_reverse" c_reverse :: UnionMap -> IO UnionMap

reverse :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
reverse = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_reverse umap


foreign import ccall "isl_union_map_subtract" c_subtract :: UnionMap -> UnionMap -> IO UnionMap

subtract :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
subtract = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_subtract umap1 umap2


foreign import ccall "isl_union_map_subtract_domain" c_subtractDomain :: UnionMap -> UnionSet -> IO UnionMap

subtractDomain :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
subtractDomain = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap dom =
    unsafeIslFromIO $ \_ -> c_subtractDomain umap dom


foreign import ccall "isl_union_map_subtract_range" c_subtractRange :: UnionMap -> UnionSet -> IO UnionMap

subtractRange :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
subtractRange = unsafeCoerce go where
  go :: UnionMap -> UnionSet -> IslT m UnionMap
  go umap dom =
    unsafeIslFromIO $ \_ -> c_subtractRange umap dom


foreign import ccall "isl_union_map_uncurry" c_uncurry :: UnionMap -> IO UnionMap

uncurry :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
uncurry = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_uncurry umap


foreign import ccall "isl_union_map_union" c_union :: UnionMap -> UnionMap -> IO UnionMap

union :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
union = unsafeCoerce go where
  go :: UnionMap -> UnionMap -> IslT m UnionMap
  go umap1 umap2 =
    unsafeIslFromIO $ \_ -> c_union umap1 umap2


foreign import ccall "isl_union_map_universe" c_universe :: UnionMap -> IO UnionMap

universe :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
universe = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_universe umap


foreign import ccall "isl_union_map_zip" c_zip :: UnionMap -> IO UnionMap

zip :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
zip = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionMap
  go umap =
    unsafeIslFromIO $ \_ -> c_zip umap


foreign import ccall "isl_union_map_deltas" c_deltas :: UnionMap -> IO UnionSet

deltas :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
deltas = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionSet
  go umap =
    unsafeIslFromIO $ \_ -> c_deltas umap


foreign import ccall "isl_union_map_domain" c_domain :: UnionMap -> IO UnionSet

domain :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
domain = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionSet
  go umap =
    unsafeIslFromIO $ \_ -> c_domain umap


foreign import ccall "isl_union_map_range" c_range :: UnionMap -> IO UnionSet

range :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
range = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionSet
  go umap =
    unsafeIslFromIO $ \_ -> c_range umap


foreign import ccall "isl_union_map_wrap" c_wrap :: UnionMap -> IO UnionSet

wrap :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
wrap = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionSet
  go umap =
    unsafeIslFromIO $ \_ -> c_wrap umap


foreign import ccall "isl_union_map_from_basic_map" c_fromBasicMap :: BasicMap -> IO UnionMap

fromBasicMap :: forall m. MonadIO m => BasicMap %1 -> IslT m UnionMap
fromBasicMap = unsafeCoerce go where
  go :: BasicMap -> IslT m UnionMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_fromBasicMap bmap


foreign import ccall "isl_union_map_from_map" c_fromMap :: Map -> IO UnionMap

fromMap :: forall m. MonadIO m => Map %1 -> IslT m UnionMap
fromMap = unsafeCoerce go where
  go :: Map -> IslT m UnionMap
  go map =
    unsafeIslFromIO $ \_ -> c_fromMap map


foreign import ccall "isl_union_map_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO UnionMap

readFromStr :: MonadIO m => String -> IslT m UnionMap
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_union_map_free" c_free :: UnionMap -> IO ()

instance Consumable UnionMap where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_union_map_copy" c_copy :: UnionMap -> IO UnionMap

instance Dupable UnionMap where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow UnionMap UnionMapRef where
  borrow = unsafeCoerce $ \(UnionMap ptr) f -> let !r = f (UnionMapRef ptr) in (r, UnionMap ptr)


