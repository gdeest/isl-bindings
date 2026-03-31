{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.BasicMap.Generated where

import Isl.Types
import Isl.Monad
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_basic_map_dim" c_dim :: BasicMapRef -> DimType -> IO C.CInt

dim :: BasicMapRef -> DimType -> Int
dim bmap typ =
    unsafePerformIO $ fromIntegral <$> c_dim bmap typ


foreign import ccall "isl_basic_map_find_dim_by_name" c_findDimByName :: BasicMapRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: BasicMapRef -> DimType -> String -> Int
findDimByName bmap typ name =
    unsafePerformIO $ do
      name_c <- C.newCString name
      fromIntegral <$> c_findDimByName bmap typ name_c


foreign import ccall "isl_basic_map_involves_dims" c_involvesDims :: BasicMapRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: BasicMapRef -> DimType -> Int -> Int -> Int
involvesDims bmap typ first n =
    unsafePerformIO $ fromIntegral <$> c_involvesDims bmap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_map_n_constraint" c_nConstraint :: BasicMapRef -> IO C.CInt

nConstraint :: BasicMapRef -> Int
nConstraint bmap =
    unsafePerformIO $ fromIntegral <$> c_nConstraint bmap


foreign import ccall "isl_basic_map_dump" c_dump :: BasicMapRef -> IO ()

dump :: BasicMapRef -> ()
dump bmap =
    unsafePerformIO $ c_dump bmap


foreign import ccall "isl_basic_map_get_dim_name" c_getDimName :: BasicMapRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: BasicMapRef -> DimType -> Int -> String
getDimName bmap typ pos =
    unsafePerformIO $ C.peekCString =<< c_getDimName bmap typ (fromIntegral pos)


foreign import ccall "isl_basic_map_get_tuple_name" c_getTupleName :: BasicMapRef -> DimType -> IO C.CString

getTupleName :: BasicMapRef -> DimType -> String
getTupleName bmap typ =
    unsafePerformIO $ C.peekCString =<< c_getTupleName bmap typ


foreign import ccall "isl_basic_map_can_curry" c_canCurry :: BasicMapRef -> IO C.CBool

canCurry :: BasicMapRef -> Bool
canCurry bmap =
    unsafePerformIO $ M.toBool <$> c_canCurry bmap


foreign import ccall "isl_basic_map_can_uncurry" c_canUncurry :: BasicMapRef -> IO C.CBool

canUncurry :: BasicMapRef -> Bool
canUncurry bmap =
    unsafePerformIO $ M.toBool <$> c_canUncurry bmap


foreign import ccall "isl_basic_map_can_zip" c_canZip :: BasicMapRef -> IO C.CBool

canZip :: BasicMapRef -> Bool
canZip bmap =
    unsafePerformIO $ M.toBool <$> c_canZip bmap


foreign import ccall "isl_basic_map_has_dim_id" c_hasDimId :: BasicMapRef -> DimType -> C.CUInt -> IO C.CBool

hasDimId :: BasicMapRef -> DimType -> Int -> Bool
hasDimId bmap typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimId bmap typ (fromIntegral pos)


foreign import ccall "isl_basic_map_image_is_bounded" c_imageIsBounded :: BasicMapRef -> IO C.CBool

imageIsBounded :: BasicMapRef -> Bool
imageIsBounded bmap =
    unsafePerformIO $ M.toBool <$> c_imageIsBounded bmap


foreign import ccall "isl_basic_map_is_disjoint" c_isDisjoint :: BasicMapRef -> BasicMapRef -> IO C.CBool

isDisjoint :: BasicMapRef -> BasicMapRef -> Bool
isDisjoint bmap1 bmap2 =
    unsafePerformIO $ M.toBool <$> c_isDisjoint bmap1 bmap2


foreign import ccall "isl_basic_map_is_rational" c_isRational :: BasicMapRef -> IO C.CBool

isRational :: BasicMapRef -> Bool
isRational bmap =
    unsafePerformIO $ M.toBool <$> c_isRational bmap


foreign import ccall "isl_basic_map_is_single_valued" c_isSingleValued :: BasicMapRef -> IO C.CBool

isSingleValued :: BasicMapRef -> Bool
isSingleValued bmap =
    unsafePerformIO $ M.toBool <$> c_isSingleValued bmap


foreign import ccall "isl_basic_map_is_strict_subset" c_isStrictSubset :: BasicMapRef -> BasicMapRef -> IO C.CBool

isStrictSubset :: BasicMapRef -> BasicMapRef -> Bool
isStrictSubset bmap1 bmap2 =
    unsafePerformIO $ M.toBool <$> c_isStrictSubset bmap1 bmap2


foreign import ccall "isl_basic_map_is_universe" c_isUniverse :: BasicMapRef -> IO C.CBool

isUniverse :: BasicMapRef -> Bool
isUniverse bmap =
    unsafePerformIO $ M.toBool <$> c_isUniverse bmap


foreign import ccall "isl_basic_map_plain_is_empty" c_plainIsEmpty :: BasicMapRef -> IO C.CBool

plainIsEmpty :: BasicMapRef -> Bool
plainIsEmpty bmap =
    unsafePerformIO $ M.toBool <$> c_plainIsEmpty bmap


foreign import ccall "isl_basic_map_plain_is_universe" c_plainIsUniverse :: BasicMapRef -> IO C.CBool

plainIsUniverse :: BasicMapRef -> Bool
plainIsUniverse bmap =
    unsafePerformIO $ M.toBool <$> c_plainIsUniverse bmap


foreign import ccall "isl_basic_map_get_space" c_getSpace :: BasicMapRef -> IO Space

getSpace :: MonadIO m => BasicMapRef -> IslT m Space
getSpace bmap =
    unsafeIslFromIO $ \_ -> c_getSpace bmap


foreign import ccall "isl_basic_map_compute_divs" c_computeDivs :: BasicMap -> IO Map

computeDivs :: forall m. MonadIO m => BasicMap %1 -> IslT m Map
computeDivs = unsafeCoerce go where
  go :: BasicMap -> IslT m Map
  go bmap =
    unsafeIslFromIO $ \_ -> c_computeDivs bmap


foreign import ccall "isl_basic_map_add_constraint" c_addConstraint :: BasicMap -> Constraint -> IO BasicMap

addConstraint :: forall m. MonadIO m => BasicMap %1 -> Constraint %1 -> IslT m BasicMap
addConstraint = unsafeCoerce go where
  go :: BasicMap -> Constraint -> IslT m BasicMap
  go bmap constraint =
    unsafeIslFromIO $ \_ -> c_addConstraint bmap constraint


foreign import ccall "isl_basic_map_add_dims" c_addDims :: BasicMap -> DimType -> C.CUInt -> IO BasicMap

addDims :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> IslT m BasicMap
addDims = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> IslT m BasicMap
  go bmap typ n =
    unsafeIslFromIO $ \_ -> c_addDims bmap typ (fromIntegral n)


foreign import ccall "isl_basic_map_align_params" c_alignParams :: BasicMap -> Space -> IO BasicMap

alignParams :: forall m. MonadIO m => BasicMap %1 -> Space %1 -> IslT m BasicMap
alignParams = unsafeCoerce go where
  go :: BasicMap -> Space -> IslT m BasicMap
  go bmap model =
    unsafeIslFromIO $ \_ -> c_alignParams bmap model


foreign import ccall "isl_basic_map_curry" c_curry :: BasicMap -> IO BasicMap

curry :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
curry = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_curry bmap


foreign import ccall "isl_basic_map_deltas_map" c_deltasMap :: BasicMap -> IO BasicMap

deltasMap :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
deltasMap = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_deltasMap bmap


foreign import ccall "isl_basic_map_domain_map" c_domainMap :: BasicMap -> IO BasicMap

domainMap :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
domainMap = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_domainMap bmap


foreign import ccall "isl_basic_map_domain_product" c_domainProduct :: BasicMap -> BasicMap -> IO BasicMap

domainProduct :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
domainProduct = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_domainProduct bmap1 bmap2


foreign import ccall "isl_basic_map_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

dropConstraintsInvolvingDims :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
dropConstraintsInvolvingDims = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsInvolvingDims bmap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_map_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

dropConstraintsNotInvolvingDims :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
dropConstraintsNotInvolvingDims = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsNotInvolvingDims bmap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_map_drop_unused_params" c_dropUnusedParams :: BasicMap -> IO BasicMap

dropUnusedParams :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
dropUnusedParams = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams bmap


foreign import ccall "isl_basic_map_eliminate" c_eliminate :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

eliminate :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
eliminate = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ first n =
    unsafeIslFromIO $ \_ -> c_eliminate bmap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_map_empty" c_empty :: Space -> IO BasicMap

empty :: forall m. MonadIO m => Space %1 -> IslT m BasicMap
empty = unsafeCoerce go where
  go :: Space -> IslT m BasicMap
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_basic_map_equal" c_equal :: Space -> C.CUInt -> IO BasicMap

equal :: forall m. MonadIO m => Space %1 -> Int -> IslT m BasicMap
equal = unsafeCoerce go where
  go :: Space -> Int -> IslT m BasicMap
  go space n_equal =
    unsafeIslFromIO $ \_ -> c_equal space (fromIntegral n_equal)


foreign import ccall "isl_basic_map_equate" c_equate :: BasicMap -> DimType -> C.CInt -> DimType -> C.CInt -> IO BasicMap

equate :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> DimType -> Int -> IslT m BasicMap
equate = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> DimType -> Int -> IslT m BasicMap
  go bmap type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_equate bmap type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_basic_map_fix_si" c_fixSi :: BasicMap -> DimType -> C.CUInt -> C.CInt -> IO BasicMap

fixSi :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
fixSi = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ pos value =
    unsafeIslFromIO $ \_ -> c_fixSi bmap typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_basic_map_fix_val" c_fixVal :: BasicMap -> DimType -> C.CUInt -> Val -> IO BasicMap

fixVal :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Val %1 -> IslT m BasicMap
fixVal = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Val -> IslT m BasicMap
  go bmap typ pos v =
    unsafeIslFromIO $ \_ -> c_fixVal bmap typ (fromIntegral pos) v


foreign import ccall "isl_basic_map_flat_product" c_flatProduct :: BasicMap -> BasicMap -> IO BasicMap

flatProduct :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
flatProduct = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_flatProduct bmap1 bmap2


foreign import ccall "isl_basic_map_flat_range_product" c_flatRangeProduct :: BasicMap -> BasicMap -> IO BasicMap

flatRangeProduct :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
flatRangeProduct = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_flatRangeProduct bmap1 bmap2


foreign import ccall "isl_basic_map_from_aff" c_fromAff :: Aff -> IO BasicMap

fromAff :: forall m. MonadIO m => Aff %1 -> IslT m BasicMap
fromAff = unsafeCoerce go where
  go :: Aff -> IslT m BasicMap
  go aff =
    unsafeIslFromIO $ \_ -> c_fromAff aff


foreign import ccall "isl_basic_map_from_constraint" c_fromConstraint :: Constraint -> IO BasicMap

fromConstraint :: forall m. MonadIO m => Constraint %1 -> IslT m BasicMap
fromConstraint = unsafeCoerce go where
  go :: Constraint -> IslT m BasicMap
  go constraint =
    unsafeIslFromIO $ \_ -> c_fromConstraint constraint


foreign import ccall "isl_basic_map_from_domain" c_fromDomain :: BasicSet -> IO BasicMap

fromDomain :: forall m. MonadIO m => BasicSet %1 -> IslT m BasicMap
fromDomain = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicMap
  go bset =
    unsafeIslFromIO $ \_ -> c_fromDomain bset


foreign import ccall "isl_basic_map_from_domain_and_range" c_fromDomainAndRange :: BasicSet -> BasicSet -> IO BasicMap

fromDomainAndRange :: forall m. MonadIO m => BasicSet %1 -> BasicSet %1 -> IslT m BasicMap
fromDomainAndRange = unsafeCoerce go where
  go :: BasicSet -> BasicSet -> IslT m BasicMap
  go domain range =
    unsafeIslFromIO $ \_ -> c_fromDomainAndRange domain range


foreign import ccall "isl_basic_map_from_range" c_fromRange :: BasicSet -> IO BasicMap

fromRange :: forall m. MonadIO m => BasicSet %1 -> IslT m BasicMap
fromRange = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicMap
  go bset =
    unsafeIslFromIO $ \_ -> c_fromRange bset


foreign import ccall "isl_basic_map_gist_domain" c_gistDomain :: BasicMap -> BasicSet -> IO BasicMap

gistDomain :: forall m. MonadIO m => BasicMap %1 -> BasicSet %1 -> IslT m BasicMap
gistDomain = unsafeCoerce go where
  go :: BasicMap -> BasicSet -> IslT m BasicMap
  go bmap context =
    unsafeIslFromIO $ \_ -> c_gistDomain bmap context


foreign import ccall "isl_basic_map_identity" c_identity :: Space -> IO BasicMap

identity :: forall m. MonadIO m => Space %1 -> IslT m BasicMap
identity = unsafeCoerce go where
  go :: Space -> IslT m BasicMap
  go space =
    unsafeIslFromIO $ \_ -> c_identity space


foreign import ccall "isl_basic_map_insert_dims" c_insertDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

insertDims :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
insertDims = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ pos n =
    unsafeIslFromIO $ \_ -> c_insertDims bmap typ (fromIntegral pos) (fromIntegral n)


foreign import ccall "isl_basic_map_less_at" c_lessAt :: Space -> C.CUInt -> IO BasicMap

lessAt :: forall m. MonadIO m => Space %1 -> Int -> IslT m BasicMap
lessAt = unsafeCoerce go where
  go :: Space -> Int -> IslT m BasicMap
  go space pos =
    unsafeIslFromIO $ \_ -> c_lessAt space (fromIntegral pos)


foreign import ccall "isl_basic_map_lower_bound_si" c_lowerBoundSi :: BasicMap -> DimType -> C.CUInt -> C.CInt -> IO BasicMap

lowerBoundSi :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
lowerBoundSi = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ pos value =
    unsafeIslFromIO $ \_ -> c_lowerBoundSi bmap typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_basic_map_more_at" c_moreAt :: Space -> C.CUInt -> IO BasicMap

moreAt :: forall m. MonadIO m => Space %1 -> Int -> IslT m BasicMap
moreAt = unsafeCoerce go where
  go :: Space -> Int -> IslT m BasicMap
  go space pos =
    unsafeIslFromIO $ \_ -> c_moreAt space (fromIntegral pos)


foreign import ccall "isl_basic_map_move_dims" c_moveDims :: BasicMap -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

moveDims :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m BasicMap
moveDims = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims bmap dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_basic_map_nat_universe" c_natUniverse :: Space -> IO BasicMap

natUniverse :: forall m. MonadIO m => Space %1 -> IslT m BasicMap
natUniverse = unsafeCoerce go where
  go :: Space -> IslT m BasicMap
  go space =
    unsafeIslFromIO $ \_ -> c_natUniverse space


foreign import ccall "isl_basic_map_neg" c_neg :: BasicMap -> IO BasicMap

neg :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
neg = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_neg bmap


foreign import ccall "isl_basic_map_order_ge" c_orderGe :: BasicMap -> DimType -> C.CInt -> DimType -> C.CInt -> IO BasicMap

orderGe :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> DimType -> Int -> IslT m BasicMap
orderGe = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> DimType -> Int -> IslT m BasicMap
  go bmap type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_orderGe bmap type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_basic_map_order_gt" c_orderGt :: BasicMap -> DimType -> C.CInt -> DimType -> C.CInt -> IO BasicMap

orderGt :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> DimType -> Int -> IslT m BasicMap
orderGt = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> DimType -> Int -> IslT m BasicMap
  go bmap type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_orderGt bmap type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_basic_map_product" c_product :: BasicMap -> BasicMap -> IO BasicMap

product :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
product = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_product bmap1 bmap2


foreign import ccall "isl_basic_map_project_out" c_projectOut :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

projectOut :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
projectOut = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ first n =
    unsafeIslFromIO $ \_ -> c_projectOut bmap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_map_range_map" c_rangeMap :: BasicMap -> IO BasicMap

rangeMap :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
rangeMap = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_rangeMap bmap


foreign import ccall "isl_basic_map_range_product" c_rangeProduct :: BasicMap -> BasicMap -> IO BasicMap

rangeProduct :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
rangeProduct = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_rangeProduct bmap1 bmap2


foreign import ccall "isl_basic_map_remove_dims" c_removeDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

removeDims :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
removeDims = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ first n =
    unsafeIslFromIO $ \_ -> c_removeDims bmap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_map_remove_divs" c_removeDivs :: BasicMap -> IO BasicMap

removeDivs :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
removeDivs = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_removeDivs bmap


foreign import ccall "isl_basic_map_remove_divs_involving_dims" c_removeDivsInvolvingDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap

removeDivsInvolvingDims :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
removeDivsInvolvingDims = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ first n =
    unsafeIslFromIO $ \_ -> c_removeDivsInvolvingDims bmap typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_map_remove_redundancies" c_removeRedundancies :: BasicMap -> IO BasicMap

removeRedundancies :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
removeRedundancies = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_removeRedundancies bmap


foreign import ccall "isl_basic_map_set_dim_name" c_setDimName :: BasicMap -> DimType -> C.CUInt -> C.CString -> IO BasicMap

setDimName :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> String -> IslT m BasicMap
setDimName = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> String -> IslT m BasicMap
  go bmap typ pos s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setDimName bmap typ (fromIntegral pos) s_c


foreign import ccall "isl_basic_map_set_tuple_id" c_setTupleId :: BasicMap -> DimType -> Id -> IO BasicMap

setTupleId :: forall m. MonadIO m => BasicMap %1 -> DimType -> Id %1 -> IslT m BasicMap
setTupleId = unsafeCoerce go where
  go :: BasicMap -> DimType -> Id -> IslT m BasicMap
  go bmap typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId bmap typ id


foreign import ccall "isl_basic_map_set_tuple_name" c_setTupleName :: BasicMap -> DimType -> C.CString -> IO BasicMap

setTupleName :: forall m. MonadIO m => BasicMap %1 -> DimType -> String -> IslT m BasicMap
setTupleName = unsafeCoerce go where
  go :: BasicMap -> DimType -> String -> IslT m BasicMap
  go bmap typ s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setTupleName bmap typ s_c


foreign import ccall "isl_basic_map_sum" c_sum :: BasicMap -> BasicMap -> IO BasicMap

sum :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
sum = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_sum bmap1 bmap2


foreign import ccall "isl_basic_map_uncurry" c_uncurry :: BasicMap -> IO BasicMap

uncurry :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
uncurry = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_uncurry bmap


foreign import ccall "isl_basic_map_universe" c_universe :: Space -> IO BasicMap

universe :: forall m. MonadIO m => Space %1 -> IslT m BasicMap
universe = unsafeCoerce go where
  go :: Space -> IslT m BasicMap
  go space =
    unsafeIslFromIO $ \_ -> c_universe space


foreign import ccall "isl_basic_map_upper_bound_si" c_upperBoundSi :: BasicMap -> DimType -> C.CUInt -> C.CInt -> IO BasicMap

upperBoundSi :: forall m. MonadIO m => BasicMap %1 -> DimType -> Int -> Int -> IslT m BasicMap
upperBoundSi = unsafeCoerce go where
  go :: BasicMap -> DimType -> Int -> Int -> IslT m BasicMap
  go bmap typ pos value =
    unsafeIslFromIO $ \_ -> c_upperBoundSi bmap typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_basic_map_zip" c_zip :: BasicMap -> IO BasicMap

zip :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
zip = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_zip bmap


foreign import ccall "isl_basic_map_domain" c_domain :: BasicMap -> IO BasicSet

domain :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicSet
domain = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicSet
  go bmap =
    unsafeIslFromIO $ \_ -> c_domain bmap


foreign import ccall "isl_basic_map_range" c_range :: BasicMap -> IO BasicSet

range :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicSet
range = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicSet
  go bmap =
    unsafeIslFromIO $ \_ -> c_range bmap


foreign import ccall "isl_basic_map_wrap" c_wrap :: BasicMap -> IO BasicSet

wrap :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicSet
wrap = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicSet
  go bmap =
    unsafeIslFromIO $ \_ -> c_wrap bmap


foreign import ccall "isl_basic_map_plain_get_val_if_fixed" c_plainGetValIfFixed :: BasicMapRef -> DimType -> C.CUInt -> IO Val

plainGetValIfFixed :: MonadIO m => BasicMapRef -> DimType -> Int -> IslT m Val
plainGetValIfFixed bmap typ pos =
    unsafeIslFromIO $ \_ -> c_plainGetValIfFixed bmap typ (fromIntegral pos)


foreign import ccall "isl_basic_map_get_div" c_getDiv :: BasicMapRef -> C.CInt -> IO Aff

getDiv :: MonadIO m => BasicMapRef -> Int -> IslT m Aff
getDiv bmap pos =
    unsafeIslFromIO $ \_ -> c_getDiv bmap (fromIntegral pos)


foreign import ccall "isl_basic_map_to_str" c_toStr :: BasicMapRef -> IO C.CString

toStr :: BasicMapRef -> String
toStr bmap =
    unsafePerformIO $ C.peekCString =<< c_toStr bmap


foreign import ccall "isl_basic_map_get_local_space" c_getLocalSpace :: BasicMapRef -> IO LocalSpace

getLocalSpace :: MonadIO m => BasicMapRef -> IslT m LocalSpace
getLocalSpace bmap =
    unsafeIslFromIO $ \_ -> c_getLocalSpace bmap


foreign import ccall "isl_basic_map_is_empty" c_isEmpty :: BasicMapRef -> IO C.CBool

isEmpty :: BasicMapRef -> Bool
isEmpty bmap =
    unsafePerformIO $ M.toBool <$> c_isEmpty bmap


foreign import ccall "isl_basic_map_is_equal" c_isEqual :: BasicMapRef -> BasicMapRef -> IO C.CBool

isEqual :: BasicMapRef -> BasicMapRef -> Bool
isEqual bmap1 bmap2 =
    unsafePerformIO $ M.toBool <$> c_isEqual bmap1 bmap2


foreign import ccall "isl_basic_map_is_subset" c_isSubset :: BasicMapRef -> BasicMapRef -> IO C.CBool

isSubset :: BasicMapRef -> BasicMapRef -> Bool
isSubset bmap1 bmap2 =
    unsafePerformIO $ M.toBool <$> c_isSubset bmap1 bmap2


foreign import ccall "isl_basic_map_lexmax" c_lexmax :: BasicMap -> IO Map

lexmax :: forall m. MonadIO m => BasicMap %1 -> IslT m Map
lexmax = unsafeCoerce go where
  go :: BasicMap -> IslT m Map
  go bmap =
    unsafeIslFromIO $ \_ -> c_lexmax bmap


foreign import ccall "isl_basic_map_lexmin" c_lexmin :: BasicMap -> IO Map

lexmin :: forall m. MonadIO m => BasicMap %1 -> IslT m Map
lexmin = unsafeCoerce go where
  go :: BasicMap -> IslT m Map
  go bmap =
    unsafeIslFromIO $ \_ -> c_lexmin bmap


foreign import ccall "isl_basic_map_union" c_union :: BasicMap -> BasicMap -> IO Map

union :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m Map
union = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m Map
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_union bmap1 bmap2


foreign import ccall "isl_basic_map_affine_hull" c_affineHull :: BasicMap -> IO BasicMap

affineHull :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
affineHull = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_affineHull bmap


foreign import ccall "isl_basic_map_apply_domain" c_applyDomain :: BasicMap -> BasicMap -> IO BasicMap

applyDomain :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
applyDomain = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_applyDomain bmap1 bmap2


foreign import ccall "isl_basic_map_apply_range" c_applyRange :: BasicMap -> BasicMap -> IO BasicMap

applyRange :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
applyRange = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_applyRange bmap1 bmap2


foreign import ccall "isl_basic_map_detect_equalities" c_detectEqualities :: BasicMap -> IO BasicMap

detectEqualities :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
detectEqualities = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_detectEqualities bmap


foreign import ccall "isl_basic_map_flatten" c_flatten :: BasicMap -> IO BasicMap

flatten :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
flatten = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_flatten bmap


foreign import ccall "isl_basic_map_flatten_domain" c_flattenDomain :: BasicMap -> IO BasicMap

flattenDomain :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
flattenDomain = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_flattenDomain bmap


foreign import ccall "isl_basic_map_flatten_range" c_flattenRange :: BasicMap -> IO BasicMap

flattenRange :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
flattenRange = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_flattenRange bmap


foreign import ccall "isl_basic_map_gist" c_gist :: BasicMap -> BasicMap -> IO BasicMap

gist :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
gist = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap context =
    unsafeIslFromIO $ \_ -> c_gist bmap context


foreign import ccall "isl_basic_map_intersect" c_intersect :: BasicMap -> BasicMap -> IO BasicMap

intersect :: forall m. MonadIO m => BasicMap %1 -> BasicMap %1 -> IslT m BasicMap
intersect = unsafeCoerce go where
  go :: BasicMap -> BasicMap -> IslT m BasicMap
  go bmap1 bmap2 =
    unsafeIslFromIO $ \_ -> c_intersect bmap1 bmap2


foreign import ccall "isl_basic_map_intersect_domain" c_intersectDomain :: BasicMap -> BasicSet -> IO BasicMap

intersectDomain :: forall m. MonadIO m => BasicMap %1 -> BasicSet %1 -> IslT m BasicMap
intersectDomain = unsafeCoerce go where
  go :: BasicMap -> BasicSet -> IslT m BasicMap
  go bmap bset =
    unsafeIslFromIO $ \_ -> c_intersectDomain bmap bset


foreign import ccall "isl_basic_map_intersect_params" c_intersectParams :: BasicMap -> BasicSet -> IO BasicMap

intersectParams :: forall m. MonadIO m => BasicMap %1 -> BasicSet %1 -> IslT m BasicMap
intersectParams = unsafeCoerce go where
  go :: BasicMap -> BasicSet -> IslT m BasicMap
  go bmap bset =
    unsafeIslFromIO $ \_ -> c_intersectParams bmap bset


foreign import ccall "isl_basic_map_intersect_range" c_intersectRange :: BasicMap -> BasicSet -> IO BasicMap

intersectRange :: forall m. MonadIO m => BasicMap %1 -> BasicSet %1 -> IslT m BasicMap
intersectRange = unsafeCoerce go where
  go :: BasicMap -> BasicSet -> IslT m BasicMap
  go bmap bset =
    unsafeIslFromIO $ \_ -> c_intersectRange bmap bset


foreign import ccall "isl_basic_map_reverse" c_reverse :: BasicMap -> IO BasicMap

reverse :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
reverse = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_reverse bmap


foreign import ccall "isl_basic_map_sample" c_sample :: BasicMap -> IO BasicMap

sample :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicMap
sample = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicMap
  go bmap =
    unsafeIslFromIO $ \_ -> c_sample bmap


foreign import ccall "isl_basic_map_deltas" c_deltas :: BasicMap -> IO BasicSet

deltas :: forall m. MonadIO m => BasicMap %1 -> IslT m BasicSet
deltas = unsafeCoerce go where
  go :: BasicMap -> IslT m BasicSet
  go bmap =
    unsafeIslFromIO $ \_ -> c_deltas bmap


foreign import ccall "isl_basic_map_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO BasicMap

readFromStr :: MonadIO m => String -> IslT m BasicMap
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_basic_map_free" c_free :: BasicMap -> IO ()

instance Consumable BasicMap where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_basic_map_copy" c_copy :: BasicMap -> IO BasicMap

instance Dupable BasicMap where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow BasicMap BasicMapRef where
  borrow = unsafeCoerce $ \(BasicMap ptr) f -> let !r = f (BasicMapRef ptr) in (r, BasicMap ptr)


