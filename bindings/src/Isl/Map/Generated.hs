{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.Map.Generated where

import Isl.Types
import Isl.Monad
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_map_dim" c_dim :: MapRef -> DimType -> IO C.CInt

dim :: MapRef -> DimType -> Int
dim map typ =
    unsafePerformIO $ fromIntegral <$> c_dim map typ


foreign import ccall "isl_map_find_dim_by_id" c_findDimById :: MapRef -> DimType -> IdRef -> IO C.CInt

findDimById :: MapRef -> DimType -> IdRef -> Int
findDimById map typ id =
    unsafePerformIO $ fromIntegral <$> c_findDimById map typ id


foreign import ccall "isl_map_find_dim_by_name" c_findDimByName :: MapRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: MapRef -> DimType -> String -> Int
findDimByName map typ name =
    unsafePerformIO $ do
      name_c <- C.newCString name
      fromIntegral <$> c_findDimByName map typ name_c


foreign import ccall "isl_map_involves_dims" c_involvesDims :: MapRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: MapRef -> DimType -> Int -> Int -> Int
involvesDims map typ first n =
    unsafePerformIO $ fromIntegral <$> c_involvesDims map typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_dump" c_dump :: MapRef -> IO ()

dump :: MapRef -> ()
dump map =
    unsafePerformIO $ c_dump map


foreign import ccall "isl_map_get_dim_name" c_getDimName :: MapRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: MapRef -> DimType -> Int -> String
getDimName map typ pos =
    unsafePerformIO $ C.peekCString =<< c_getDimName map typ (fromIntegral pos)


foreign import ccall "isl_map_get_tuple_name" c_getTupleName :: MapRef -> DimType -> IO C.CString

getTupleName :: MapRef -> DimType -> String
getTupleName map typ =
    unsafePerformIO $ C.peekCString =<< c_getTupleName map typ


foreign import ccall "isl_map_can_curry" c_canCurry :: MapRef -> IO C.CBool

canCurry :: MapRef -> Bool
canCurry map =
    unsafePerformIO $ M.toBool <$> c_canCurry map


foreign import ccall "isl_map_can_range_curry" c_canRangeCurry :: MapRef -> IO C.CBool

canRangeCurry :: MapRef -> Bool
canRangeCurry map =
    unsafePerformIO $ M.toBool <$> c_canRangeCurry map


foreign import ccall "isl_map_can_uncurry" c_canUncurry :: MapRef -> IO C.CBool

canUncurry :: MapRef -> Bool
canUncurry map =
    unsafePerformIO $ M.toBool <$> c_canUncurry map


foreign import ccall "isl_map_can_zip" c_canZip :: MapRef -> IO C.CBool

canZip :: MapRef -> Bool
canZip map =
    unsafePerformIO $ M.toBool <$> c_canZip map


foreign import ccall "isl_map_domain_is_wrapping" c_domainIsWrapping :: MapRef -> IO C.CBool

domainIsWrapping :: MapRef -> Bool
domainIsWrapping map =
    unsafePerformIO $ M.toBool <$> c_domainIsWrapping map


foreign import ccall "isl_map_has_dim_id" c_hasDimId :: MapRef -> DimType -> C.CUInt -> IO C.CBool

hasDimId :: MapRef -> DimType -> Int -> Bool
hasDimId map typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimId map typ (fromIntegral pos)


foreign import ccall "isl_map_has_dim_name" c_hasDimName :: MapRef -> DimType -> C.CUInt -> IO C.CBool

hasDimName :: MapRef -> DimType -> Int -> Bool
hasDimName map typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimName map typ (fromIntegral pos)


foreign import ccall "isl_map_has_equal_space" c_hasEqualSpace :: MapRef -> MapRef -> IO C.CBool

hasEqualSpace :: MapRef -> MapRef -> Bool
hasEqualSpace map1 map2 =
    unsafePerformIO $ M.toBool <$> c_hasEqualSpace map1 map2


foreign import ccall "isl_map_has_tuple_id" c_hasTupleId :: MapRef -> DimType -> IO C.CBool

hasTupleId :: MapRef -> DimType -> Bool
hasTupleId map typ =
    unsafePerformIO $ M.toBool <$> c_hasTupleId map typ


foreign import ccall "isl_map_has_tuple_name" c_hasTupleName :: MapRef -> DimType -> IO C.CBool

hasTupleName :: MapRef -> DimType -> Bool
hasTupleName map typ =
    unsafePerformIO $ M.toBool <$> c_hasTupleName map typ


foreign import ccall "isl_map_is_identity" c_isIdentity :: MapRef -> IO C.CBool

isIdentity :: MapRef -> Bool
isIdentity map =
    unsafePerformIO $ M.toBool <$> c_isIdentity map


foreign import ccall "isl_map_is_product" c_isProduct :: MapRef -> IO C.CBool

isProduct :: MapRef -> Bool
isProduct map =
    unsafePerformIO $ M.toBool <$> c_isProduct map


foreign import ccall "isl_map_is_translation" c_isTranslation :: MapRef -> IO C.CBool

isTranslation :: MapRef -> Bool
isTranslation map =
    unsafePerformIO $ M.toBool <$> c_isTranslation map


foreign import ccall "isl_map_plain_is_empty" c_plainIsEmpty :: MapRef -> IO C.CBool

plainIsEmpty :: MapRef -> Bool
plainIsEmpty map =
    unsafePerformIO $ M.toBool <$> c_plainIsEmpty map


foreign import ccall "isl_map_plain_is_equal" c_plainIsEqual :: MapRef -> MapRef -> IO C.CBool

plainIsEqual :: MapRef -> MapRef -> Bool
plainIsEqual map1 map2 =
    unsafePerformIO $ M.toBool <$> c_plainIsEqual map1 map2


foreign import ccall "isl_map_plain_is_injective" c_plainIsInjective :: MapRef -> IO C.CBool

plainIsInjective :: MapRef -> Bool
plainIsInjective map =
    unsafePerformIO $ M.toBool <$> c_plainIsInjective map


foreign import ccall "isl_map_plain_is_single_valued" c_plainIsSingleValued :: MapRef -> IO C.CBool

plainIsSingleValued :: MapRef -> Bool
plainIsSingleValued map =
    unsafePerformIO $ M.toBool <$> c_plainIsSingleValued map


foreign import ccall "isl_map_plain_is_universe" c_plainIsUniverse :: MapRef -> IO C.CBool

plainIsUniverse :: MapRef -> Bool
plainIsUniverse map =
    unsafePerformIO $ M.toBool <$> c_plainIsUniverse map


foreign import ccall "isl_map_range_is_wrapping" c_rangeIsWrapping :: MapRef -> IO C.CBool

rangeIsWrapping :: MapRef -> Bool
rangeIsWrapping map =
    unsafePerformIO $ M.toBool <$> c_rangeIsWrapping map


foreign import ccall "isl_map_add_constraint" c_addConstraint :: Map -> Constraint -> IO Map

addConstraint :: forall m. MonadIO m => Map %1 -> Constraint %1 -> IslT m Map
addConstraint = unsafeCoerce go where
  go :: Map -> Constraint -> IslT m Map
  go map constraint =
    unsafeIslFromIO $ \_ -> c_addConstraint map constraint


foreign import ccall "isl_map_add_dims" c_addDims :: Map -> DimType -> C.CUInt -> IO Map

addDims :: forall m. MonadIO m => Map %1 -> DimType -> Int -> IslT m Map
addDims = unsafeCoerce go where
  go :: Map -> DimType -> Int -> IslT m Map
  go map typ n =
    unsafeIslFromIO $ \_ -> c_addDims map typ (fromIntegral n)


foreign import ccall "isl_map_align_params" c_alignParams :: Map -> Space -> IO Map

alignParams :: forall m. MonadIO m => Map %1 -> Space %1 -> IslT m Map
alignParams = unsafeCoerce go where
  go :: Map -> Space -> IslT m Map
  go map model =
    unsafeIslFromIO $ \_ -> c_alignParams map model


foreign import ccall "isl_map_compute_divs" c_computeDivs :: Map -> IO Map

computeDivs :: forall m. MonadIO m => Map %1 -> IslT m Map
computeDivs = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_computeDivs map


foreign import ccall "isl_map_deltas_map" c_deltasMap :: Map -> IO Map

deltasMap :: forall m. MonadIO m => Map %1 -> IslT m Map
deltasMap = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_deltasMap map


foreign import ccall "isl_map_domain_map" c_domainMap :: Map -> IO Map

domainMap :: forall m. MonadIO m => Map %1 -> IslT m Map
domainMap = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_domainMap map


foreign import ccall "isl_map_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map

dropConstraintsInvolvingDims :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
dropConstraintsInvolvingDims = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsInvolvingDims map typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map

dropConstraintsNotInvolvingDims :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
dropConstraintsNotInvolvingDims = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsNotInvolvingDims map typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_eliminate" c_eliminate :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map

eliminate :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
eliminate = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ first n =
    unsafeIslFromIO $ \_ -> c_eliminate map typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_equate" c_equate :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map

equate :: forall m. MonadIO m => Map %1 -> DimType -> Int -> DimType -> Int -> IslT m Map
equate = unsafeCoerce go where
  go :: Map -> DimType -> Int -> DimType -> Int -> IslT m Map
  go map type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_equate map type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_map_fix_input_si" c_fixInputSi :: Map -> C.CUInt -> C.CInt -> IO Map

fixInputSi :: forall m. MonadIO m => Map %1 -> Int -> Int -> IslT m Map
fixInputSi = unsafeCoerce go where
  go :: Map -> Int -> Int -> IslT m Map
  go map input value =
    unsafeIslFromIO $ \_ -> c_fixInputSi map (fromIntegral input) (fromIntegral value)


foreign import ccall "isl_map_fix_si" c_fixSi :: Map -> DimType -> C.CUInt -> C.CInt -> IO Map

fixSi :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
fixSi = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ pos value =
    unsafeIslFromIO $ \_ -> c_fixSi map typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_map_fix_val" c_fixVal :: Map -> DimType -> C.CUInt -> Val -> IO Map

fixVal :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Val %1 -> IslT m Map
fixVal = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Val -> IslT m Map
  go map typ pos v =
    unsafeIslFromIO $ \_ -> c_fixVal map typ (fromIntegral pos) v


foreign import ccall "isl_map_fixed_power_val" c_fixedPowerVal :: Map -> Val -> IO Map

fixedPowerVal :: forall m. MonadIO m => Map %1 -> Val %1 -> IslT m Map
fixedPowerVal = unsafeCoerce go where
  go :: Map -> Val -> IslT m Map
  go map exp =
    unsafeIslFromIO $ \_ -> c_fixedPowerVal map exp


foreign import ccall "isl_map_flat_domain_product" c_flatDomainProduct :: Map -> Map -> IO Map

flatDomainProduct :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
flatDomainProduct = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_flatDomainProduct map1 map2


foreign import ccall "isl_map_flat_product" c_flatProduct :: Map -> Map -> IO Map

flatProduct :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
flatProduct = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_flatProduct map1 map2


foreign import ccall "isl_map_flat_range_product" c_flatRangeProduct :: Map -> Map -> IO Map

flatRangeProduct :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
flatRangeProduct = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_flatRangeProduct map1 map2


foreign import ccall "isl_map_floordiv_val" c_floordivVal :: Map -> Val -> IO Map

floordivVal :: forall m. MonadIO m => Map %1 -> Val %1 -> IslT m Map
floordivVal = unsafeCoerce go where
  go :: Map -> Val -> IslT m Map
  go map d =
    unsafeIslFromIO $ \_ -> c_floordivVal map d


foreign import ccall "isl_map_from_aff" c_fromAff :: Aff -> IO Map

fromAff :: forall m. MonadIO m => Aff %1 -> IslT m Map
fromAff = unsafeCoerce go where
  go :: Aff -> IslT m Map
  go aff =
    unsafeIslFromIO $ \_ -> c_fromAff aff


foreign import ccall "isl_map_from_domain" c_fromDomain :: Set -> IO Map

fromDomain :: forall m. MonadIO m => Set %1 -> IslT m Map
fromDomain = unsafeCoerce go where
  go :: Set -> IslT m Map
  go set =
    unsafeIslFromIO $ \_ -> c_fromDomain set


foreign import ccall "isl_map_from_domain_and_range" c_fromDomainAndRange :: Set -> Set -> IO Map

fromDomainAndRange :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Map
fromDomainAndRange = unsafeCoerce go where
  go :: Set -> Set -> IslT m Map
  go domain range =
    unsafeIslFromIO $ \_ -> c_fromDomainAndRange domain range


foreign import ccall "isl_map_from_multi_aff" c_fromMultiAff :: MultiAff -> IO Map

fromMultiAff :: forall m. MonadIO m => MultiAff %1 -> IslT m Map
fromMultiAff = unsafeCoerce go where
  go :: MultiAff -> IslT m Map
  go maff =
    unsafeIslFromIO $ \_ -> c_fromMultiAff maff


foreign import ccall "isl_map_from_pw_aff" c_fromPwAff :: PwAff -> IO Map

fromPwAff :: forall m. MonadIO m => PwAff %1 -> IslT m Map
fromPwAff = unsafeCoerce go where
  go :: PwAff -> IslT m Map
  go pwaff =
    unsafeIslFromIO $ \_ -> c_fromPwAff pwaff


foreign import ccall "isl_map_from_pw_multi_aff" c_fromPwMultiAff :: PwMultiAff -> IO Map

fromPwMultiAff :: forall m. MonadIO m => PwMultiAff %1 -> IslT m Map
fromPwMultiAff = unsafeCoerce go where
  go :: PwMultiAff -> IslT m Map
  go pma =
    unsafeIslFromIO $ \_ -> c_fromPwMultiAff pma


foreign import ccall "isl_map_from_range" c_fromRange :: Set -> IO Map

fromRange :: forall m. MonadIO m => Set %1 -> IslT m Map
fromRange = unsafeCoerce go where
  go :: Set -> IslT m Map
  go set =
    unsafeIslFromIO $ \_ -> c_fromRange set


foreign import ccall "isl_map_from_union_map" c_fromUnionMap :: UnionMap -> IO Map

fromUnionMap :: forall m. MonadIO m => UnionMap %1 -> IslT m Map
fromUnionMap = unsafeCoerce go where
  go :: UnionMap -> IslT m Map
  go umap =
    unsafeIslFromIO $ \_ -> c_fromUnionMap umap


foreign import ccall "isl_map_gist_basic_map" c_gistBasicMap :: Map -> BasicMap -> IO Map

gistBasicMap :: forall m. MonadIO m => Map %1 -> BasicMap %1 -> IslT m Map
gistBasicMap = unsafeCoerce go where
  go :: Map -> BasicMap -> IslT m Map
  go map context =
    unsafeIslFromIO $ \_ -> c_gistBasicMap map context


foreign import ccall "isl_map_gist_range" c_gistRange :: Map -> Set -> IO Map

gistRange :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
gistRange = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map context =
    unsafeIslFromIO $ \_ -> c_gistRange map context


foreign import ccall "isl_map_identity" c_identity :: Space -> IO Map

identity :: forall m. MonadIO m => Space %1 -> IslT m Map
identity = unsafeCoerce go where
  go :: Space -> IslT m Map
  go space =
    unsafeIslFromIO $ \_ -> c_identity space


foreign import ccall "isl_map_insert_dims" c_insertDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map

insertDims :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
insertDims = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ pos n =
    unsafeIslFromIO $ \_ -> c_insertDims map typ (fromIntegral pos) (fromIntegral n)


foreign import ccall "isl_map_lex_ge" c_lexGe :: Space -> IO Map

lexGe :: forall m. MonadIO m => Space %1 -> IslT m Map
lexGe = unsafeCoerce go where
  go :: Space -> IslT m Map
  go set_space =
    unsafeIslFromIO $ \_ -> c_lexGe set_space


foreign import ccall "isl_map_lex_ge_first" c_lexGeFirst :: Space -> C.CUInt -> IO Map

lexGeFirst :: forall m. MonadIO m => Space %1 -> Int -> IslT m Map
lexGeFirst = unsafeCoerce go where
  go :: Space -> Int -> IslT m Map
  go space n =
    unsafeIslFromIO $ \_ -> c_lexGeFirst space (fromIntegral n)


foreign import ccall "isl_map_lex_ge_map" c_lexGeMap :: Map -> Map -> IO Map

lexGeMap :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
lexGeMap = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_lexGeMap map1 map2


foreign import ccall "isl_map_lex_gt" c_lexGt :: Space -> IO Map

lexGt :: forall m. MonadIO m => Space %1 -> IslT m Map
lexGt = unsafeCoerce go where
  go :: Space -> IslT m Map
  go set_space =
    unsafeIslFromIO $ \_ -> c_lexGt set_space


foreign import ccall "isl_map_lex_gt_first" c_lexGtFirst :: Space -> C.CUInt -> IO Map

lexGtFirst :: forall m. MonadIO m => Space %1 -> Int -> IslT m Map
lexGtFirst = unsafeCoerce go where
  go :: Space -> Int -> IslT m Map
  go space n =
    unsafeIslFromIO $ \_ -> c_lexGtFirst space (fromIntegral n)


foreign import ccall "isl_map_lex_gt_map" c_lexGtMap :: Map -> Map -> IO Map

lexGtMap :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
lexGtMap = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_lexGtMap map1 map2


foreign import ccall "isl_map_lex_le" c_lexLe :: Space -> IO Map

lexLe :: forall m. MonadIO m => Space %1 -> IslT m Map
lexLe = unsafeCoerce go where
  go :: Space -> IslT m Map
  go set_space =
    unsafeIslFromIO $ \_ -> c_lexLe set_space


foreign import ccall "isl_map_lex_le_first" c_lexLeFirst :: Space -> C.CUInt -> IO Map

lexLeFirst :: forall m. MonadIO m => Space %1 -> Int -> IslT m Map
lexLeFirst = unsafeCoerce go where
  go :: Space -> Int -> IslT m Map
  go space n =
    unsafeIslFromIO $ \_ -> c_lexLeFirst space (fromIntegral n)


foreign import ccall "isl_map_lex_le_map" c_lexLeMap :: Map -> Map -> IO Map

lexLeMap :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
lexLeMap = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_lexLeMap map1 map2


foreign import ccall "isl_map_lex_lt" c_lexLt :: Space -> IO Map

lexLt :: forall m. MonadIO m => Space %1 -> IslT m Map
lexLt = unsafeCoerce go where
  go :: Space -> IslT m Map
  go set_space =
    unsafeIslFromIO $ \_ -> c_lexLt set_space


foreign import ccall "isl_map_lex_lt_first" c_lexLtFirst :: Space -> C.CUInt -> IO Map

lexLtFirst :: forall m. MonadIO m => Space %1 -> Int -> IslT m Map
lexLtFirst = unsafeCoerce go where
  go :: Space -> Int -> IslT m Map
  go space n =
    unsafeIslFromIO $ \_ -> c_lexLtFirst space (fromIntegral n)


foreign import ccall "isl_map_lex_lt_map" c_lexLtMap :: Map -> Map -> IO Map

lexLtMap :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
lexLtMap = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_lexLtMap map1 map2


foreign import ccall "isl_map_lower_bound_si" c_lowerBoundSi :: Map -> DimType -> C.CUInt -> C.CInt -> IO Map

lowerBoundSi :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
lowerBoundSi = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ pos value =
    unsafeIslFromIO $ \_ -> c_lowerBoundSi map typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_map_lower_bound_val" c_lowerBoundVal :: Map -> DimType -> C.CUInt -> Val -> IO Map

lowerBoundVal :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Val %1 -> IslT m Map
lowerBoundVal = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Val -> IslT m Map
  go map typ pos value =
    unsafeIslFromIO $ \_ -> c_lowerBoundVal map typ (fromIntegral pos) value


foreign import ccall "isl_map_make_disjoint" c_makeDisjoint :: Map -> IO Map

makeDisjoint :: forall m. MonadIO m => Map %1 -> IslT m Map
makeDisjoint = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_makeDisjoint map


foreign import ccall "isl_map_move_dims" c_moveDims :: Map -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Map

moveDims :: forall m. MonadIO m => Map %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m Map
moveDims = unsafeCoerce go where
  go :: Map -> DimType -> Int -> DimType -> Int -> Int -> IslT m Map
  go map dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims map dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_map_nat_universe" c_natUniverse :: Space -> IO Map

natUniverse :: forall m. MonadIO m => Space %1 -> IslT m Map
natUniverse = unsafeCoerce go where
  go :: Space -> IslT m Map
  go space =
    unsafeIslFromIO $ \_ -> c_natUniverse space


foreign import ccall "isl_map_neg" c_neg :: Map -> IO Map

neg :: forall m. MonadIO m => Map %1 -> IslT m Map
neg = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_neg map


foreign import ccall "isl_map_oppose" c_oppose :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map

oppose :: forall m. MonadIO m => Map %1 -> DimType -> Int -> DimType -> Int -> IslT m Map
oppose = unsafeCoerce go where
  go :: Map -> DimType -> Int -> DimType -> Int -> IslT m Map
  go map type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_oppose map type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_map_order_ge" c_orderGe :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map

orderGe :: forall m. MonadIO m => Map %1 -> DimType -> Int -> DimType -> Int -> IslT m Map
orderGe = unsafeCoerce go where
  go :: Map -> DimType -> Int -> DimType -> Int -> IslT m Map
  go map type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_orderGe map type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_map_order_gt" c_orderGt :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map

orderGt :: forall m. MonadIO m => Map %1 -> DimType -> Int -> DimType -> Int -> IslT m Map
orderGt = unsafeCoerce go where
  go :: Map -> DimType -> Int -> DimType -> Int -> IslT m Map
  go map type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_orderGt map type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_map_order_le" c_orderLe :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map

orderLe :: forall m. MonadIO m => Map %1 -> DimType -> Int -> DimType -> Int -> IslT m Map
orderLe = unsafeCoerce go where
  go :: Map -> DimType -> Int -> DimType -> Int -> IslT m Map
  go map type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_orderLe map type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_map_order_lt" c_orderLt :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map

orderLt :: forall m. MonadIO m => Map %1 -> DimType -> Int -> DimType -> Int -> IslT m Map
orderLt = unsafeCoerce go where
  go :: Map -> DimType -> Int -> DimType -> Int -> IslT m Map
  go map type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_orderLt map type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_map_preimage_domain_multi_aff" c_preimageDomainMultiAff :: Map -> MultiAff -> IO Map

preimageDomainMultiAff :: forall m. MonadIO m => Map %1 -> MultiAff %1 -> IslT m Map
preimageDomainMultiAff = unsafeCoerce go where
  go :: Map -> MultiAff -> IslT m Map
  go map ma =
    unsafeIslFromIO $ \_ -> c_preimageDomainMultiAff map ma


foreign import ccall "isl_map_preimage_domain_pw_multi_aff" c_preimageDomainPwMultiAff :: Map -> PwMultiAff -> IO Map

preimageDomainPwMultiAff :: forall m. MonadIO m => Map %1 -> PwMultiAff %1 -> IslT m Map
preimageDomainPwMultiAff = unsafeCoerce go where
  go :: Map -> PwMultiAff -> IslT m Map
  go map pma =
    unsafeIslFromIO $ \_ -> c_preimageDomainPwMultiAff map pma


foreign import ccall "isl_map_preimage_range_multi_aff" c_preimageRangeMultiAff :: Map -> MultiAff -> IO Map

preimageRangeMultiAff :: forall m. MonadIO m => Map %1 -> MultiAff %1 -> IslT m Map
preimageRangeMultiAff = unsafeCoerce go where
  go :: Map -> MultiAff -> IslT m Map
  go map ma =
    unsafeIslFromIO $ \_ -> c_preimageRangeMultiAff map ma


foreign import ccall "isl_map_preimage_range_pw_multi_aff" c_preimageRangePwMultiAff :: Map -> PwMultiAff -> IO Map

preimageRangePwMultiAff :: forall m. MonadIO m => Map %1 -> PwMultiAff %1 -> IslT m Map
preimageRangePwMultiAff = unsafeCoerce go where
  go :: Map -> PwMultiAff -> IslT m Map
  go map pma =
    unsafeIslFromIO $ \_ -> c_preimageRangePwMultiAff map pma


foreign import ccall "isl_map_project_out" c_projectOut :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map

projectOut :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
projectOut = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ first n =
    unsafeIslFromIO $ \_ -> c_projectOut map typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_project_out_param_id" c_projectOutParamId :: Map -> Id -> IO Map

projectOutParamId :: forall m. MonadIO m => Map %1 -> Id %1 -> IslT m Map
projectOutParamId = unsafeCoerce go where
  go :: Map -> Id -> IslT m Map
  go map id =
    unsafeIslFromIO $ \_ -> c_projectOutParamId map id


foreign import ccall "isl_map_range_curry" c_rangeCurry :: Map -> IO Map

rangeCurry :: forall m. MonadIO m => Map %1 -> IslT m Map
rangeCurry = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_rangeCurry map


foreign import ccall "isl_map_range_map" c_rangeMap :: Map -> IO Map

rangeMap :: forall m. MonadIO m => Map %1 -> IslT m Map
rangeMap = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_rangeMap map


foreign import ccall "isl_map_remove_dims" c_removeDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map

removeDims :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
removeDims = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ first n =
    unsafeIslFromIO $ \_ -> c_removeDims map typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_remove_divs" c_removeDivs :: Map -> IO Map

removeDivs :: forall m. MonadIO m => Map %1 -> IslT m Map
removeDivs = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_removeDivs map


foreign import ccall "isl_map_remove_divs_involving_dims" c_removeDivsInvolvingDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map

removeDivsInvolvingDims :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
removeDivsInvolvingDims = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ first n =
    unsafeIslFromIO $ \_ -> c_removeDivsInvolvingDims map typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_remove_inputs" c_removeInputs :: Map -> C.CUInt -> C.CUInt -> IO Map

removeInputs :: forall m. MonadIO m => Map %1 -> Int -> Int -> IslT m Map
removeInputs = unsafeCoerce go where
  go :: Map -> Int -> Int -> IslT m Map
  go map first n =
    unsafeIslFromIO $ \_ -> c_removeInputs map (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_map_remove_redundancies" c_removeRedundancies :: Map -> IO Map

removeRedundancies :: forall m. MonadIO m => Map %1 -> IslT m Map
removeRedundancies = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_removeRedundancies map


foreign import ccall "isl_map_remove_unknown_divs" c_removeUnknownDivs :: Map -> IO Map

removeUnknownDivs :: forall m. MonadIO m => Map %1 -> IslT m Map
removeUnknownDivs = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_removeUnknownDivs map


foreign import ccall "isl_map_reset_tuple_id" c_resetTupleId :: Map -> DimType -> IO Map

resetTupleId :: forall m. MonadIO m => Map %1 -> DimType -> IslT m Map
resetTupleId = unsafeCoerce go where
  go :: Map -> DimType -> IslT m Map
  go map typ =
    unsafeIslFromIO $ \_ -> c_resetTupleId map typ


foreign import ccall "isl_map_reset_user" c_resetUser :: Map -> IO Map

resetUser :: forall m. MonadIO m => Map %1 -> IslT m Map
resetUser = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_resetUser map


foreign import ccall "isl_map_set_dim_id" c_setDimId :: Map -> DimType -> C.CUInt -> Id -> IO Map

setDimId :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Id %1 -> IslT m Map
setDimId = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Id -> IslT m Map
  go map typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId map typ (fromIntegral pos) id


foreign import ccall "isl_map_set_dim_name" c_setDimName :: Map -> DimType -> C.CUInt -> C.CString -> IO Map

setDimName :: forall m. MonadIO m => Map %1 -> DimType -> Int -> String -> IslT m Map
setDimName = unsafeCoerce go where
  go :: Map -> DimType -> Int -> String -> IslT m Map
  go map typ pos s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setDimName map typ (fromIntegral pos) s_c


foreign import ccall "isl_map_set_domain_tuple_id" c_setDomainTupleId :: Map -> Id -> IO Map

setDomainTupleId :: forall m. MonadIO m => Map %1 -> Id %1 -> IslT m Map
setDomainTupleId = unsafeCoerce go where
  go :: Map -> Id -> IslT m Map
  go map id =
    unsafeIslFromIO $ \_ -> c_setDomainTupleId map id


foreign import ccall "isl_map_set_range_tuple_id" c_setRangeTupleId :: Map -> Id -> IO Map

setRangeTupleId :: forall m. MonadIO m => Map %1 -> Id %1 -> IslT m Map
setRangeTupleId = unsafeCoerce go where
  go :: Map -> Id -> IslT m Map
  go map id =
    unsafeIslFromIO $ \_ -> c_setRangeTupleId map id


foreign import ccall "isl_map_set_tuple_id" c_setTupleId :: Map -> DimType -> Id -> IO Map

setTupleId :: forall m. MonadIO m => Map %1 -> DimType -> Id %1 -> IslT m Map
setTupleId = unsafeCoerce go where
  go :: Map -> DimType -> Id -> IslT m Map
  go map typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId map typ id


foreign import ccall "isl_map_set_tuple_name" c_setTupleName :: Map -> DimType -> C.CString -> IO Map

setTupleName :: forall m. MonadIO m => Map %1 -> DimType -> String -> IslT m Map
setTupleName = unsafeCoerce go where
  go :: Map -> DimType -> String -> IslT m Map
  go map typ s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setTupleName map typ s_c


foreign import ccall "isl_map_subtract_domain" c_subtractDomain :: Map -> Set -> IO Map

subtractDomain :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
subtractDomain = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map dom =
    unsafeIslFromIO $ \_ -> c_subtractDomain map dom


foreign import ccall "isl_map_subtract_range" c_subtractRange :: Map -> Set -> IO Map

subtractRange :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
subtractRange = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map dom =
    unsafeIslFromIO $ \_ -> c_subtractRange map dom


foreign import ccall "isl_map_sum" c_sum :: Map -> Map -> IO Map

sum :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
sum = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_sum map1 map2


foreign import ccall "isl_map_union_disjoint" c_unionDisjoint :: Map -> Map -> IO Map

unionDisjoint :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
unionDisjoint = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_unionDisjoint map1 map2


foreign import ccall "isl_map_upper_bound_si" c_upperBoundSi :: Map -> DimType -> C.CUInt -> C.CInt -> IO Map

upperBoundSi :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Int -> IslT m Map
upperBoundSi = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Int -> IslT m Map
  go map typ pos value =
    unsafeIslFromIO $ \_ -> c_upperBoundSi map typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_map_upper_bound_val" c_upperBoundVal :: Map -> DimType -> C.CUInt -> Val -> IO Map

upperBoundVal :: forall m. MonadIO m => Map %1 -> DimType -> Int -> Val %1 -> IslT m Map
upperBoundVal = unsafeCoerce go where
  go :: Map -> DimType -> Int -> Val -> IslT m Map
  go map typ pos value =
    unsafeIslFromIO $ \_ -> c_upperBoundVal map typ (fromIntegral pos) value


foreign import ccall "isl_map_convex_hull" c_convexHull :: Map -> IO BasicMap

convexHull :: forall m. MonadIO m => Map %1 -> IslT m BasicMap
convexHull = unsafeCoerce go where
  go :: Map -> IslT m BasicMap
  go map =
    unsafeIslFromIO $ \_ -> c_convexHull map


foreign import ccall "isl_map_plain_unshifted_simple_hull" c_plainUnshiftedSimpleHull :: Map -> IO BasicMap

plainUnshiftedSimpleHull :: forall m. MonadIO m => Map %1 -> IslT m BasicMap
plainUnshiftedSimpleHull = unsafeCoerce go where
  go :: Map -> IslT m BasicMap
  go map =
    unsafeIslFromIO $ \_ -> c_plainUnshiftedSimpleHull map


foreign import ccall "isl_map_simple_hull" c_simpleHull :: Map -> IO BasicMap

simpleHull :: forall m. MonadIO m => Map %1 -> IslT m BasicMap
simpleHull = unsafeCoerce go where
  go :: Map -> IslT m BasicMap
  go map =
    unsafeIslFromIO $ \_ -> c_simpleHull map


foreign import ccall "isl_map_plain_get_val_if_fixed" c_plainGetValIfFixed :: MapRef -> DimType -> C.CUInt -> IO Val

plainGetValIfFixed :: MonadIO m => MapRef -> DimType -> Int -> IslT m Val
plainGetValIfFixed map typ pos =
    unsafeIslFromIO $ \_ -> c_plainGetValIfFixed map typ (fromIntegral pos)


foreign import ccall "isl_map_dim_max" c_dimMax :: Map -> C.CInt -> IO PwAff

dimMax :: forall m. MonadIO m => Map %1 -> Int -> IslT m PwAff
dimMax = unsafeCoerce go where
  go :: Map -> Int -> IslT m PwAff
  go map pos =
    unsafeIslFromIO $ \_ -> c_dimMax map (fromIntegral pos)


foreign import ccall "isl_map_dim_min" c_dimMin :: Map -> C.CInt -> IO PwAff

dimMin :: forall m. MonadIO m => Map %1 -> Int -> IslT m PwAff
dimMin = unsafeCoerce go where
  go :: Map -> Int -> IslT m PwAff
  go map pos =
    unsafeIslFromIO $ \_ -> c_dimMin map (fromIntegral pos)


foreign import ccall "isl_map_get_dim_id" c_getDimId :: MapRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => MapRef -> DimType -> Int -> IslT m Id
getDimId map typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId map typ (fromIntegral pos)


foreign import ccall "isl_map_get_tuple_id" c_getTupleId :: MapRef -> DimType -> IO Id

getTupleId :: MonadIO m => MapRef -> DimType -> IslT m Id
getTupleId map typ =
    unsafeIslFromIO $ \_ -> c_getTupleId map typ


foreign import ccall "isl_map_to_str" c_toStr :: MapRef -> IO C.CString

toStr :: MapRef -> String
toStr map =
    unsafePerformIO $ C.peekCString =<< c_toStr map


foreign import ccall "isl_map_domain_tuple_dim" c_domainTupleDim :: MapRef -> IO C.CInt

domainTupleDim :: MapRef -> Int
domainTupleDim map =
    unsafePerformIO $ fromIntegral <$> c_domainTupleDim map


foreign import ccall "isl_map_n_basic_map" c_nBasicMap :: MapRef -> IO C.CInt

nBasicMap :: MapRef -> Int
nBasicMap map =
    unsafePerformIO $ fromIntegral <$> c_nBasicMap map


foreign import ccall "isl_map_range_tuple_dim" c_rangeTupleDim :: MapRef -> IO C.CInt

rangeTupleDim :: MapRef -> Int
rangeTupleDim map =
    unsafePerformIO $ fromIntegral <$> c_rangeTupleDim map


foreign import ccall "isl_map_has_domain_tuple_id" c_hasDomainTupleId :: MapRef -> IO C.CBool

hasDomainTupleId :: MapRef -> Bool
hasDomainTupleId map =
    unsafePerformIO $ M.toBool <$> c_hasDomainTupleId map


foreign import ccall "isl_map_has_range_tuple_id" c_hasRangeTupleId :: MapRef -> IO C.CBool

hasRangeTupleId :: MapRef -> Bool
hasRangeTupleId map =
    unsafePerformIO $ M.toBool <$> c_hasRangeTupleId map


foreign import ccall "isl_map_is_bijective" c_isBijective :: MapRef -> IO C.CBool

isBijective :: MapRef -> Bool
isBijective map =
    unsafePerformIO $ M.toBool <$> c_isBijective map


foreign import ccall "isl_map_is_disjoint" c_isDisjoint :: MapRef -> MapRef -> IO C.CBool

isDisjoint :: MapRef -> MapRef -> Bool
isDisjoint map1 map2 =
    unsafePerformIO $ M.toBool <$> c_isDisjoint map1 map2


foreign import ccall "isl_map_is_empty" c_isEmpty :: MapRef -> IO C.CBool

isEmpty :: MapRef -> Bool
isEmpty map =
    unsafePerformIO $ M.toBool <$> c_isEmpty map


foreign import ccall "isl_map_is_equal" c_isEqual :: MapRef -> MapRef -> IO C.CBool

isEqual :: MapRef -> MapRef -> Bool
isEqual map1 map2 =
    unsafePerformIO $ M.toBool <$> c_isEqual map1 map2


foreign import ccall "isl_map_is_injective" c_isInjective :: MapRef -> IO C.CBool

isInjective :: MapRef -> Bool
isInjective map =
    unsafePerformIO $ M.toBool <$> c_isInjective map


foreign import ccall "isl_map_is_single_valued" c_isSingleValued :: MapRef -> IO C.CBool

isSingleValued :: MapRef -> Bool
isSingleValued map =
    unsafePerformIO $ M.toBool <$> c_isSingleValued map


foreign import ccall "isl_map_is_strict_subset" c_isStrictSubset :: MapRef -> MapRef -> IO C.CBool

isStrictSubset :: MapRef -> MapRef -> Bool
isStrictSubset map1 map2 =
    unsafePerformIO $ M.toBool <$> c_isStrictSubset map1 map2


foreign import ccall "isl_map_is_subset" c_isSubset :: MapRef -> MapRef -> IO C.CBool

isSubset :: MapRef -> MapRef -> Bool
isSubset map1 map2 =
    unsafePerformIO $ M.toBool <$> c_isSubset map1 map2


foreign import ccall "isl_map_deltas" c_deltas :: Map -> IO Set

deltas :: forall m. MonadIO m => Map %1 -> IslT m Set
deltas = unsafeCoerce go where
  go :: Map -> IslT m Set
  go map =
    unsafeIslFromIO $ \_ -> c_deltas map


foreign import ccall "isl_map_domain" c_domain :: Map -> IO Set

domain :: forall m. MonadIO m => Map %1 -> IslT m Set
domain = unsafeCoerce go where
  go :: Map -> IslT m Set
  go bmap =
    unsafeIslFromIO $ \_ -> c_domain bmap


foreign import ccall "isl_map_params" c_params :: Map -> IO Set

params :: forall m. MonadIO m => Map %1 -> IslT m Set
params = unsafeCoerce go where
  go :: Map -> IslT m Set
  go map =
    unsafeIslFromIO $ \_ -> c_params map


foreign import ccall "isl_map_range" c_range :: Map -> IO Set

range :: forall m. MonadIO m => Map %1 -> IslT m Set
range = unsafeCoerce go where
  go :: Map -> IslT m Set
  go map =
    unsafeIslFromIO $ \_ -> c_range map


foreign import ccall "isl_map_wrap" c_wrap :: Map -> IO Set

wrap :: forall m. MonadIO m => Map %1 -> IslT m Set
wrap = unsafeCoerce go where
  go :: Map -> IslT m Set
  go map =
    unsafeIslFromIO $ \_ -> c_wrap map


foreign import ccall "isl_map_get_space" c_getSpace :: MapRef -> IO Space

getSpace :: MonadIO m => MapRef -> IslT m Space
getSpace map =
    unsafeIslFromIO $ \_ -> c_getSpace map


foreign import ccall "isl_map_apply_domain" c_applyDomain :: Map -> Map -> IO Map

applyDomain :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
applyDomain = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_applyDomain map1 map2


foreign import ccall "isl_map_apply_range" c_applyRange :: Map -> Map -> IO Map

applyRange :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
applyRange = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_applyRange map1 map2


foreign import ccall "isl_map_coalesce" c_coalesce :: Map -> IO Map

coalesce :: forall m. MonadIO m => Map %1 -> IslT m Map
coalesce = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_coalesce map


foreign import ccall "isl_map_complement" c_complement :: Map -> IO Map

complement :: forall m. MonadIO m => Map %1 -> IslT m Map
complement = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_complement map


foreign import ccall "isl_map_curry" c_curry :: Map -> IO Map

curry :: forall m. MonadIO m => Map %1 -> IslT m Map
curry = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_curry map


foreign import ccall "isl_map_detect_equalities" c_detectEqualities :: Map -> IO Map

detectEqualities :: forall m. MonadIO m => Map %1 -> IslT m Map
detectEqualities = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_detectEqualities map


foreign import ccall "isl_map_domain_factor_domain" c_domainFactorDomain :: Map -> IO Map

domainFactorDomain :: forall m. MonadIO m => Map %1 -> IslT m Map
domainFactorDomain = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_domainFactorDomain map


foreign import ccall "isl_map_domain_factor_range" c_domainFactorRange :: Map -> IO Map

domainFactorRange :: forall m. MonadIO m => Map %1 -> IslT m Map
domainFactorRange = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_domainFactorRange map


foreign import ccall "isl_map_domain_product" c_domainProduct :: Map -> Map -> IO Map

domainProduct :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
domainProduct = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_domainProduct map1 map2


foreign import ccall "isl_map_domain_reverse" c_domainReverse :: Map -> IO Map

domainReverse :: forall m. MonadIO m => Map %1 -> IslT m Map
domainReverse = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_domainReverse map


foreign import ccall "isl_map_drop_unused_params" c_dropUnusedParams :: Map -> IO Map

dropUnusedParams :: forall m. MonadIO m => Map %1 -> IslT m Map
dropUnusedParams = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams map


foreign import ccall "isl_map_empty" c_empty :: Space -> IO Map

empty :: forall m. MonadIO m => Space %1 -> IslT m Map
empty = unsafeCoerce go where
  go :: Space -> IslT m Map
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_map_factor_domain" c_factorDomain :: Map -> IO Map

factorDomain :: forall m. MonadIO m => Map %1 -> IslT m Map
factorDomain = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_factorDomain map


foreign import ccall "isl_map_factor_range" c_factorRange :: Map -> IO Map

factorRange :: forall m. MonadIO m => Map %1 -> IslT m Map
factorRange = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_factorRange map


foreign import ccall "isl_map_flatten" c_flatten :: Map -> IO Map

flatten :: forall m. MonadIO m => Map %1 -> IslT m Map
flatten = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_flatten map


foreign import ccall "isl_map_flatten_domain" c_flattenDomain :: Map -> IO Map

flattenDomain :: forall m. MonadIO m => Map %1 -> IslT m Map
flattenDomain = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_flattenDomain map


foreign import ccall "isl_map_flatten_range" c_flattenRange :: Map -> IO Map

flattenRange :: forall m. MonadIO m => Map %1 -> IslT m Map
flattenRange = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_flattenRange map


foreign import ccall "isl_map_gist" c_gist :: Map -> Map -> IO Map

gist :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
gist = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map context =
    unsafeIslFromIO $ \_ -> c_gist map context


foreign import ccall "isl_map_gist_domain" c_gistDomain :: Map -> Set -> IO Map

gistDomain :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
gistDomain = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map context =
    unsafeIslFromIO $ \_ -> c_gistDomain map context


foreign import ccall "isl_map_gist_params" c_gistParams :: Map -> Set -> IO Map

gistParams :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
gistParams = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map context =
    unsafeIslFromIO $ \_ -> c_gistParams map context


foreign import ccall "isl_map_intersect" c_intersect :: Map -> Map -> IO Map

intersect :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
intersect = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_intersect map1 map2


foreign import ccall "isl_map_intersect_domain" c_intersectDomain :: Map -> Set -> IO Map

intersectDomain :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
intersectDomain = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map set =
    unsafeIslFromIO $ \_ -> c_intersectDomain map set


foreign import ccall "isl_map_intersect_domain_factor_domain" c_intersectDomainFactorDomain :: Map -> Map -> IO Map

intersectDomainFactorDomain :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
intersectDomainFactorDomain = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map factor =
    unsafeIslFromIO $ \_ -> c_intersectDomainFactorDomain map factor


foreign import ccall "isl_map_intersect_domain_factor_range" c_intersectDomainFactorRange :: Map -> Map -> IO Map

intersectDomainFactorRange :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
intersectDomainFactorRange = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map factor =
    unsafeIslFromIO $ \_ -> c_intersectDomainFactorRange map factor


foreign import ccall "isl_map_intersect_domain_wrapped_domain" c_intersectDomainWrappedDomain :: Map -> Set -> IO Map

intersectDomainWrappedDomain :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
intersectDomainWrappedDomain = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map domain =
    unsafeIslFromIO $ \_ -> c_intersectDomainWrappedDomain map domain


foreign import ccall "isl_map_intersect_params" c_intersectParams :: Map -> Set -> IO Map

intersectParams :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
intersectParams = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map params =
    unsafeIslFromIO $ \_ -> c_intersectParams map params


foreign import ccall "isl_map_intersect_range" c_intersectRange :: Map -> Set -> IO Map

intersectRange :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
intersectRange = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map set =
    unsafeIslFromIO $ \_ -> c_intersectRange map set


foreign import ccall "isl_map_intersect_range_factor_domain" c_intersectRangeFactorDomain :: Map -> Map -> IO Map

intersectRangeFactorDomain :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
intersectRangeFactorDomain = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map factor =
    unsafeIslFromIO $ \_ -> c_intersectRangeFactorDomain map factor


foreign import ccall "isl_map_intersect_range_factor_range" c_intersectRangeFactorRange :: Map -> Map -> IO Map

intersectRangeFactorRange :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
intersectRangeFactorRange = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map factor =
    unsafeIslFromIO $ \_ -> c_intersectRangeFactorRange map factor


foreign import ccall "isl_map_intersect_range_wrapped_domain" c_intersectRangeWrappedDomain :: Map -> Set -> IO Map

intersectRangeWrappedDomain :: forall m. MonadIO m => Map %1 -> Set %1 -> IslT m Map
intersectRangeWrappedDomain = unsafeCoerce go where
  go :: Map -> Set -> IslT m Map
  go map domain =
    unsafeIslFromIO $ \_ -> c_intersectRangeWrappedDomain map domain


foreign import ccall "isl_map_lexmax" c_lexmax :: Map -> IO Map

lexmax :: forall m. MonadIO m => Map %1 -> IslT m Map
lexmax = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_lexmax map


foreign import ccall "isl_map_lexmin" c_lexmin :: Map -> IO Map

lexmin :: forall m. MonadIO m => Map %1 -> IslT m Map
lexmin = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_lexmin map


foreign import ccall "isl_map_product" c_product :: Map -> Map -> IO Map

product :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
product = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_product map1 map2


foreign import ccall "isl_map_project_out_all_params" c_projectOutAllParams :: Map -> IO Map

projectOutAllParams :: forall m. MonadIO m => Map %1 -> IslT m Map
projectOutAllParams = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_projectOutAllParams map


foreign import ccall "isl_map_range_factor_domain" c_rangeFactorDomain :: Map -> IO Map

rangeFactorDomain :: forall m. MonadIO m => Map %1 -> IslT m Map
rangeFactorDomain = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_rangeFactorDomain map


foreign import ccall "isl_map_range_factor_range" c_rangeFactorRange :: Map -> IO Map

rangeFactorRange :: forall m. MonadIO m => Map %1 -> IslT m Map
rangeFactorRange = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_rangeFactorRange map


foreign import ccall "isl_map_range_product" c_rangeProduct :: Map -> Map -> IO Map

rangeProduct :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
rangeProduct = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_rangeProduct map1 map2


foreign import ccall "isl_map_range_reverse" c_rangeReverse :: Map -> IO Map

rangeReverse :: forall m. MonadIO m => Map %1 -> IslT m Map
rangeReverse = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_rangeReverse map


foreign import ccall "isl_map_reverse" c_reverse :: Map -> IO Map

reverse :: forall m. MonadIO m => Map %1 -> IslT m Map
reverse = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_reverse map


foreign import ccall "isl_map_subtract" c_subtract :: Map -> Map -> IO Map

subtract :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
subtract = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_subtract map1 map2


foreign import ccall "isl_map_uncurry" c_uncurry :: Map -> IO Map

uncurry :: forall m. MonadIO m => Map %1 -> IslT m Map
uncurry = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_uncurry map


foreign import ccall "isl_map_union" c_union :: Map -> Map -> IO Map

union :: forall m. MonadIO m => Map %1 -> Map %1 -> IslT m Map
union = unsafeCoerce go where
  go :: Map -> Map -> IslT m Map
  go map1 map2 =
    unsafeIslFromIO $ \_ -> c_union map1 map2


foreign import ccall "isl_map_universe" c_universe :: Space -> IO Map

universe :: forall m. MonadIO m => Space %1 -> IslT m Map
universe = unsafeCoerce go where
  go :: Space -> IslT m Map
  go space =
    unsafeIslFromIO $ \_ -> c_universe space


foreign import ccall "isl_map_zip" c_zip :: Map -> IO Map

zip :: forall m. MonadIO m => Map %1 -> IslT m Map
zip = unsafeCoerce go where
  go :: Map -> IslT m Map
  go map =
    unsafeIslFromIO $ \_ -> c_zip map


foreign import ccall "isl_map_affine_hull" c_affineHull :: Map -> IO BasicMap

affineHull :: forall m. MonadIO m => Map %1 -> IslT m BasicMap
affineHull = unsafeCoerce go where
  go :: Map -> IslT m BasicMap
  go map =
    unsafeIslFromIO $ \_ -> c_affineHull map


foreign import ccall "isl_map_polyhedral_hull" c_polyhedralHull :: Map -> IO BasicMap

polyhedralHull :: forall m. MonadIO m => Map %1 -> IslT m BasicMap
polyhedralHull = unsafeCoerce go where
  go :: Map -> IslT m BasicMap
  go map =
    unsafeIslFromIO $ \_ -> c_polyhedralHull map


foreign import ccall "isl_map_sample" c_sample :: Map -> IO BasicMap

sample :: forall m. MonadIO m => Map %1 -> IslT m BasicMap
sample = unsafeCoerce go where
  go :: Map -> IslT m BasicMap
  go map =
    unsafeIslFromIO $ \_ -> c_sample map


foreign import ccall "isl_map_unshifted_simple_hull" c_unshiftedSimpleHull :: Map -> IO BasicMap

unshiftedSimpleHull :: forall m. MonadIO m => Map %1 -> IslT m BasicMap
unshiftedSimpleHull = unsafeCoerce go where
  go :: Map -> IslT m BasicMap
  go map =
    unsafeIslFromIO $ \_ -> c_unshiftedSimpleHull map


foreign import ccall "isl_map_to_union_map" c_toUnionMap :: Map -> IO UnionMap

toUnionMap :: forall m. MonadIO m => Map %1 -> IslT m UnionMap
toUnionMap = unsafeCoerce go where
  go :: Map -> IslT m UnionMap
  go map =
    unsafeIslFromIO $ \_ -> c_toUnionMap map


foreign import ccall "isl_map_as_pw_multi_aff" c_asPwMultiAff :: Map -> IO PwMultiAff

asPwMultiAff :: forall m. MonadIO m => Map %1 -> IslT m PwMultiAff
asPwMultiAff = unsafeCoerce go where
  go :: Map -> IslT m PwMultiAff
  go map =
    unsafeIslFromIO $ \_ -> c_asPwMultiAff map


foreign import ccall "isl_map_lexmax_pw_multi_aff" c_lexmaxPwMultiAff :: Map -> IO PwMultiAff

lexmaxPwMultiAff :: forall m. MonadIO m => Map %1 -> IslT m PwMultiAff
lexmaxPwMultiAff = unsafeCoerce go where
  go :: Map -> IslT m PwMultiAff
  go map =
    unsafeIslFromIO $ \_ -> c_lexmaxPwMultiAff map


foreign import ccall "isl_map_lexmin_pw_multi_aff" c_lexminPwMultiAff :: Map -> IO PwMultiAff

lexminPwMultiAff :: forall m. MonadIO m => Map %1 -> IslT m PwMultiAff
lexminPwMultiAff = unsafeCoerce go where
  go :: Map -> IslT m PwMultiAff
  go map =
    unsafeIslFromIO $ \_ -> c_lexminPwMultiAff map


foreign import ccall "isl_map_get_domain_tuple_id" c_getDomainTupleId :: MapRef -> IO Id

getDomainTupleId :: MonadIO m => MapRef -> IslT m Id
getDomainTupleId map =
    unsafeIslFromIO $ \_ -> c_getDomainTupleId map


foreign import ccall "isl_map_get_range_tuple_id" c_getRangeTupleId :: MapRef -> IO Id

getRangeTupleId :: MonadIO m => MapRef -> IslT m Id
getRangeTupleId map =
    unsafeIslFromIO $ \_ -> c_getRangeTupleId map


foreign import ccall "isl_map_from_basic_map" c_fromBasicMap :: BasicMap -> IO Map

fromBasicMap :: forall m. MonadIO m => BasicMap %1 -> IslT m Map
fromBasicMap = unsafeCoerce go where
  go :: BasicMap -> IslT m Map
  go bmap =
    unsafeIslFromIO $ \_ -> c_fromBasicMap bmap


foreign import ccall "isl_map_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Map

readFromStr :: MonadIO m => String -> IslT m Map
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_map_free" c_free :: Map -> IO ()

instance Consumable Map where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_map_copy" c_copy :: Map -> IO Map

instance Dupable Map where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow Map MapRef where
  borrow = unsafeCoerce $ \(Map ptr) f -> let !r = f (MapRef ptr) in (r, Map ptr)


