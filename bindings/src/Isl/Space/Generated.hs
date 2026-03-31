{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.Space.Generated where

import Isl.Types
import Isl.Monad
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_space_dim" c_dim :: SpaceRef -> DimType -> IO C.CInt

dim :: SpaceRef -> DimType -> Int
dim space typ =
    unsafePerformIO $ fromIntegral <$> c_dim space typ


foreign import ccall "isl_space_find_dim_by_id" c_findDimById :: SpaceRef -> DimType -> IdRef -> IO C.CInt

findDimById :: SpaceRef -> DimType -> IdRef -> Int
findDimById space typ id =
    unsafePerformIO $ fromIntegral <$> c_findDimById space typ id


foreign import ccall "isl_space_find_dim_by_name" c_findDimByName :: SpaceRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: SpaceRef -> DimType -> String -> Int
findDimByName space typ name =
    unsafePerformIO $ do
      name_c <- C.newCString name
      fromIntegral <$> c_findDimByName space typ name_c


foreign import ccall "isl_space_dump" c_dump :: SpaceRef -> IO ()

dump :: SpaceRef -> ()
dump space =
    unsafePerformIO $ c_dump space


foreign import ccall "isl_space_can_curry" c_canCurry :: SpaceRef -> IO C.CBool

canCurry :: SpaceRef -> Bool
canCurry space =
    unsafePerformIO $ M.toBool <$> c_canCurry space


foreign import ccall "isl_space_can_range_curry" c_canRangeCurry :: SpaceRef -> IO C.CBool

canRangeCurry :: SpaceRef -> Bool
canRangeCurry space =
    unsafePerformIO $ M.toBool <$> c_canRangeCurry space


foreign import ccall "isl_space_can_uncurry" c_canUncurry :: SpaceRef -> IO C.CBool

canUncurry :: SpaceRef -> Bool
canUncurry space =
    unsafePerformIO $ M.toBool <$> c_canUncurry space


foreign import ccall "isl_space_can_zip" c_canZip :: SpaceRef -> IO C.CBool

canZip :: SpaceRef -> Bool
canZip space =
    unsafePerformIO $ M.toBool <$> c_canZip space


foreign import ccall "isl_space_domain_is_wrapping" c_domainIsWrapping :: SpaceRef -> IO C.CBool

domainIsWrapping :: SpaceRef -> Bool
domainIsWrapping space =
    unsafePerformIO $ M.toBool <$> c_domainIsWrapping space


foreign import ccall "isl_space_has_dim_id" c_hasDimId :: SpaceRef -> DimType -> C.CUInt -> IO C.CBool

hasDimId :: SpaceRef -> DimType -> Int -> Bool
hasDimId space typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimId space typ (fromIntegral pos)


foreign import ccall "isl_space_has_dim_name" c_hasDimName :: SpaceRef -> DimType -> C.CUInt -> IO C.CBool

hasDimName :: SpaceRef -> DimType -> Int -> Bool
hasDimName space typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimName space typ (fromIntegral pos)


foreign import ccall "isl_space_has_equal_params" c_hasEqualParams :: SpaceRef -> SpaceRef -> IO C.CBool

hasEqualParams :: SpaceRef -> SpaceRef -> Bool
hasEqualParams space1 space2 =
    unsafePerformIO $ M.toBool <$> c_hasEqualParams space1 space2


foreign import ccall "isl_space_has_equal_tuples" c_hasEqualTuples :: SpaceRef -> SpaceRef -> IO C.CBool

hasEqualTuples :: SpaceRef -> SpaceRef -> Bool
hasEqualTuples space1 space2 =
    unsafePerformIO $ M.toBool <$> c_hasEqualTuples space1 space2


foreign import ccall "isl_space_has_tuple_id" c_hasTupleId :: SpaceRef -> DimType -> IO C.CBool

hasTupleId :: SpaceRef -> DimType -> Bool
hasTupleId space typ =
    unsafePerformIO $ M.toBool <$> c_hasTupleId space typ


foreign import ccall "isl_space_has_tuple_name" c_hasTupleName :: SpaceRef -> DimType -> IO C.CBool

hasTupleName :: SpaceRef -> DimType -> Bool
hasTupleName space typ =
    unsafePerformIO $ M.toBool <$> c_hasTupleName space typ


foreign import ccall "isl_space_is_domain" c_isDomain :: SpaceRef -> SpaceRef -> IO C.CBool

isDomain :: SpaceRef -> SpaceRef -> Bool
isDomain space1 space2 =
    unsafePerformIO $ M.toBool <$> c_isDomain space1 space2


foreign import ccall "isl_space_is_map" c_isMap :: SpaceRef -> IO C.CBool

isMap :: SpaceRef -> Bool
isMap space =
    unsafePerformIO $ M.toBool <$> c_isMap space


foreign import ccall "isl_space_is_params" c_isParams :: SpaceRef -> IO C.CBool

isParams :: SpaceRef -> Bool
isParams space =
    unsafePerformIO $ M.toBool <$> c_isParams space


foreign import ccall "isl_space_is_product" c_isProduct :: SpaceRef -> IO C.CBool

isProduct :: SpaceRef -> Bool
isProduct space =
    unsafePerformIO $ M.toBool <$> c_isProduct space


foreign import ccall "isl_space_is_range" c_isRange :: SpaceRef -> SpaceRef -> IO C.CBool

isRange :: SpaceRef -> SpaceRef -> Bool
isRange space1 space2 =
    unsafePerformIO $ M.toBool <$> c_isRange space1 space2


foreign import ccall "isl_space_is_set" c_isSet :: SpaceRef -> IO C.CBool

isSet :: SpaceRef -> Bool
isSet space =
    unsafePerformIO $ M.toBool <$> c_isSet space


foreign import ccall "isl_space_range_is_wrapping" c_rangeIsWrapping :: SpaceRef -> IO C.CBool

rangeIsWrapping :: SpaceRef -> Bool
rangeIsWrapping space =
    unsafePerformIO $ M.toBool <$> c_rangeIsWrapping space


foreign import ccall "isl_space_tuple_is_equal" c_tupleIsEqual :: SpaceRef -> DimType -> SpaceRef -> DimType -> IO C.CBool

tupleIsEqual :: SpaceRef -> DimType -> SpaceRef -> DimType -> Bool
tupleIsEqual space1 type1 space2 type2 =
    unsafePerformIO $ M.toBool <$> c_tupleIsEqual space1 type1 space2 type2


foreign import ccall "isl_space_add_dims" c_addDims :: Space -> DimType -> C.CUInt -> IO Space

addDims :: forall m. MonadIO m => Space %1 -> DimType -> Int -> IslT m Space
addDims = unsafeCoerce go where
  go :: Space -> DimType -> Int -> IslT m Space
  go space typ n =
    unsafeIslFromIO $ \_ -> c_addDims space typ (fromIntegral n)


foreign import ccall "isl_space_add_named_tuple_id_ui" c_addNamedTupleIdUi :: Space -> Id -> C.CUInt -> IO Space

addNamedTupleIdUi :: forall m. MonadIO m => Space %1 -> Id %1 -> Int -> IslT m Space
addNamedTupleIdUi = unsafeCoerce go where
  go :: Space -> Id -> Int -> IslT m Space
  go space tuple_id dim =
    unsafeIslFromIO $ \_ -> c_addNamedTupleIdUi space tuple_id (fromIntegral dim)


foreign import ccall "isl_space_add_param_id" c_addParamId :: Space -> Id -> IO Space

addParamId :: forall m. MonadIO m => Space %1 -> Id %1 -> IslT m Space
addParamId = unsafeCoerce go where
  go :: Space -> Id -> IslT m Space
  go space id =
    unsafeIslFromIO $ \_ -> c_addParamId space id


foreign import ccall "isl_space_add_unnamed_tuple_ui" c_addUnnamedTupleUi :: Space -> C.CUInt -> IO Space

addUnnamedTupleUi :: forall m. MonadIO m => Space %1 -> Int -> IslT m Space
addUnnamedTupleUi = unsafeCoerce go where
  go :: Space -> Int -> IslT m Space
  go space dim =
    unsafeIslFromIO $ \_ -> c_addUnnamedTupleUi space (fromIntegral dim)


foreign import ccall "isl_space_align_params" c_alignParams :: Space -> Space -> IO Space

alignParams :: forall m. MonadIO m => Space %1 -> Space %1 -> IslT m Space
alignParams = unsafeCoerce go where
  go :: Space -> Space -> IslT m Space
  go space1 space2 =
    unsafeIslFromIO $ \_ -> c_alignParams space1 space2


foreign import ccall "isl_space_alloc" c_alloc :: Ctx -> C.CUInt -> C.CUInt -> C.CUInt -> IO Space

alloc :: MonadIO m => Int -> Int -> Int -> IslT m Space
alloc nparam n_in n_out =
    unsafeIslFromIO $ \ctx -> c_alloc ctx (fromIntegral nparam) (fromIntegral n_in) (fromIntegral n_out)


foreign import ccall "isl_space_domain_factor_domain" c_domainFactorDomain :: Space -> IO Space

domainFactorDomain :: forall m. MonadIO m => Space %1 -> IslT m Space
domainFactorDomain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_domainFactorDomain space


foreign import ccall "isl_space_domain_factor_range" c_domainFactorRange :: Space -> IO Space

domainFactorRange :: forall m. MonadIO m => Space %1 -> IslT m Space
domainFactorRange = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_domainFactorRange space


foreign import ccall "isl_space_domain_map" c_domainMap :: Space -> IO Space

domainMap :: forall m. MonadIO m => Space %1 -> IslT m Space
domainMap = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_domainMap space


foreign import ccall "isl_space_domain_product" c_domainProduct :: Space -> Space -> IO Space

domainProduct :: forall m. MonadIO m => Space %1 -> Space %1 -> IslT m Space
domainProduct = unsafeCoerce go where
  go :: Space -> Space -> IslT m Space
  go left right =
    unsafeIslFromIO $ \_ -> c_domainProduct left right


foreign import ccall "isl_space_domain_wrapped_domain" c_domainWrappedDomain :: Space -> IO Space

domainWrappedDomain :: forall m. MonadIO m => Space %1 -> IslT m Space
domainWrappedDomain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_domainWrappedDomain space


foreign import ccall "isl_space_domain_wrapped_range" c_domainWrappedRange :: Space -> IO Space

domainWrappedRange :: forall m. MonadIO m => Space %1 -> IslT m Space
domainWrappedRange = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_domainWrappedRange space


foreign import ccall "isl_space_drop_dims" c_dropDims :: Space -> DimType -> C.CUInt -> C.CUInt -> IO Space

dropDims :: forall m. MonadIO m => Space %1 -> DimType -> Int -> Int -> IslT m Space
dropDims = unsafeCoerce go where
  go :: Space -> DimType -> Int -> Int -> IslT m Space
  go space typ first num =
    unsafeIslFromIO $ \_ -> c_dropDims space typ (fromIntegral first) (fromIntegral num)


foreign import ccall "isl_space_factor_domain" c_factorDomain :: Space -> IO Space

factorDomain :: forall m. MonadIO m => Space %1 -> IslT m Space
factorDomain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_factorDomain space


foreign import ccall "isl_space_factor_range" c_factorRange :: Space -> IO Space

factorRange :: forall m. MonadIO m => Space %1 -> IslT m Space
factorRange = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_factorRange space


foreign import ccall "isl_space_from_domain" c_fromDomain :: Space -> IO Space

fromDomain :: forall m. MonadIO m => Space %1 -> IslT m Space
fromDomain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_fromDomain space


foreign import ccall "isl_space_from_range" c_fromRange :: Space -> IO Space

fromRange :: forall m. MonadIO m => Space %1 -> IslT m Space
fromRange = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_fromRange space


foreign import ccall "isl_space_insert_dims" c_insertDims :: Space -> DimType -> C.CUInt -> C.CUInt -> IO Space

insertDims :: forall m. MonadIO m => Space %1 -> DimType -> Int -> Int -> IslT m Space
insertDims = unsafeCoerce go where
  go :: Space -> DimType -> Int -> Int -> IslT m Space
  go space typ pos n =
    unsafeIslFromIO $ \_ -> c_insertDims space typ (fromIntegral pos) (fromIntegral n)


foreign import ccall "isl_space_join" c_join :: Space -> Space -> IO Space

join :: forall m. MonadIO m => Space %1 -> Space %1 -> IslT m Space
join = unsafeCoerce go where
  go :: Space -> Space -> IslT m Space
  go left right =
    unsafeIslFromIO $ \_ -> c_join left right


foreign import ccall "isl_space_map_from_domain_and_range" c_mapFromDomainAndRange :: Space -> Space -> IO Space

mapFromDomainAndRange :: forall m. MonadIO m => Space %1 -> Space %1 -> IslT m Space
mapFromDomainAndRange = unsafeCoerce go where
  go :: Space -> Space -> IslT m Space
  go domain range =
    unsafeIslFromIO $ \_ -> c_mapFromDomainAndRange domain range


foreign import ccall "isl_space_move_dims" c_moveDims :: Space -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Space

moveDims :: forall m. MonadIO m => Space %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m Space
moveDims = unsafeCoerce go where
  go :: Space -> DimType -> Int -> DimType -> Int -> Int -> IslT m Space
  go space dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims space dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_space_params_alloc" c_paramsAlloc :: Ctx -> C.CUInt -> IO Space

paramsAlloc :: MonadIO m => Int -> IslT m Space
paramsAlloc nparam =
    unsafeIslFromIO $ \ctx -> c_paramsAlloc ctx (fromIntegral nparam)


foreign import ccall "isl_space_range_curry" c_rangeCurry :: Space -> IO Space

rangeCurry :: forall m. MonadIO m => Space %1 -> IslT m Space
rangeCurry = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_rangeCurry space


foreign import ccall "isl_space_range_factor_domain" c_rangeFactorDomain :: Space -> IO Space

rangeFactorDomain :: forall m. MonadIO m => Space %1 -> IslT m Space
rangeFactorDomain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_rangeFactorDomain space


foreign import ccall "isl_space_range_factor_range" c_rangeFactorRange :: Space -> IO Space

rangeFactorRange :: forall m. MonadIO m => Space %1 -> IslT m Space
rangeFactorRange = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_rangeFactorRange space


foreign import ccall "isl_space_range_map" c_rangeMap :: Space -> IO Space

rangeMap :: forall m. MonadIO m => Space %1 -> IslT m Space
rangeMap = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_rangeMap space


foreign import ccall "isl_space_range_product" c_rangeProduct :: Space -> Space -> IO Space

rangeProduct :: forall m. MonadIO m => Space %1 -> Space %1 -> IslT m Space
rangeProduct = unsafeCoerce go where
  go :: Space -> Space -> IslT m Space
  go left right =
    unsafeIslFromIO $ \_ -> c_rangeProduct left right


foreign import ccall "isl_space_range_wrapped_domain" c_rangeWrappedDomain :: Space -> IO Space

rangeWrappedDomain :: forall m. MonadIO m => Space %1 -> IslT m Space
rangeWrappedDomain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_rangeWrappedDomain space


foreign import ccall "isl_space_range_wrapped_range" c_rangeWrappedRange :: Space -> IO Space

rangeWrappedRange :: forall m. MonadIO m => Space %1 -> IslT m Space
rangeWrappedRange = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_rangeWrappedRange space


foreign import ccall "isl_space_reset_tuple_id" c_resetTupleId :: Space -> DimType -> IO Space

resetTupleId :: forall m. MonadIO m => Space %1 -> DimType -> IslT m Space
resetTupleId = unsafeCoerce go where
  go :: Space -> DimType -> IslT m Space
  go space typ =
    unsafeIslFromIO $ \_ -> c_resetTupleId space typ


foreign import ccall "isl_space_reset_user" c_resetUser :: Space -> IO Space

resetUser :: forall m. MonadIO m => Space %1 -> IslT m Space
resetUser = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_resetUser space


foreign import ccall "isl_space_set_alloc" c_setAlloc :: Ctx -> C.CUInt -> C.CUInt -> IO Space

setAlloc :: MonadIO m => Int -> Int -> IslT m Space
setAlloc nparam dim =
    unsafeIslFromIO $ \ctx -> c_setAlloc ctx (fromIntegral nparam) (fromIntegral dim)


foreign import ccall "isl_space_set_dim_id" c_setDimId :: Space -> DimType -> C.CUInt -> Id -> IO Space

setDimId :: forall m. MonadIO m => Space %1 -> DimType -> Int -> Id %1 -> IslT m Space
setDimId = unsafeCoerce go where
  go :: Space -> DimType -> Int -> Id -> IslT m Space
  go space typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId space typ (fromIntegral pos) id


foreign import ccall "isl_space_set_dim_name" c_setDimName :: Space -> DimType -> C.CUInt -> C.CString -> IO Space

setDimName :: forall m. MonadIO m => Space %1 -> DimType -> Int -> String -> IslT m Space
setDimName = unsafeCoerce go where
  go :: Space -> DimType -> Int -> String -> IslT m Space
  go space typ pos name =
    unsafeIslFromIO $ \_ -> do
      name_c <- C.newCString name
      c_setDimName space typ (fromIntegral pos) name_c


foreign import ccall "isl_space_set_domain_tuple_id" c_setDomainTupleId :: Space -> Id -> IO Space

setDomainTupleId :: forall m. MonadIO m => Space %1 -> Id %1 -> IslT m Space
setDomainTupleId = unsafeCoerce go where
  go :: Space -> Id -> IslT m Space
  go space id =
    unsafeIslFromIO $ \_ -> c_setDomainTupleId space id


foreign import ccall "isl_space_set_from_params" c_setFromParams :: Space -> IO Space

setFromParams :: forall m. MonadIO m => Space %1 -> IslT m Space
setFromParams = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_setFromParams space


foreign import ccall "isl_space_set_range_tuple_id" c_setRangeTupleId :: Space -> Id -> IO Space

setRangeTupleId :: forall m. MonadIO m => Space %1 -> Id %1 -> IslT m Space
setRangeTupleId = unsafeCoerce go where
  go :: Space -> Id -> IslT m Space
  go space id =
    unsafeIslFromIO $ \_ -> c_setRangeTupleId space id


foreign import ccall "isl_space_set_tuple_id" c_setTupleId :: Space -> DimType -> Id -> IO Space

setTupleId :: forall m. MonadIO m => Space %1 -> DimType -> Id %1 -> IslT m Space
setTupleId = unsafeCoerce go where
  go :: Space -> DimType -> Id -> IslT m Space
  go space typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId space typ id


foreign import ccall "isl_space_set_tuple_name" c_setTupleName :: Space -> DimType -> C.CString -> IO Space

setTupleName :: forall m. MonadIO m => Space %1 -> DimType -> String -> IslT m Space
setTupleName = unsafeCoerce go where
  go :: Space -> DimType -> String -> IslT m Space
  go space typ s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setTupleName space typ s_c


foreign import ccall "isl_space_zip" c_zip :: Space -> IO Space

zip :: forall m. MonadIO m => Space %1 -> IslT m Space
zip = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_zip space


foreign import ccall "isl_space_param_aff_on_domain_id" c_paramAffOnDomainId :: Space -> Id -> IO Aff

paramAffOnDomainId :: forall m. MonadIO m => Space %1 -> Id %1 -> IslT m Aff
paramAffOnDomainId = unsafeCoerce go where
  go :: Space -> Id -> IslT m Aff
  go space id =
    unsafeIslFromIO $ \_ -> c_paramAffOnDomainId space id


foreign import ccall "isl_space_get_dim_id" c_getDimId :: SpaceRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => SpaceRef -> DimType -> Int -> IslT m Id
getDimId space typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId space typ (fromIntegral pos)


foreign import ccall "isl_space_get_tuple_id" c_getTupleId :: SpaceRef -> DimType -> IO Id

getTupleId :: MonadIO m => SpaceRef -> DimType -> IslT m Id
getTupleId space typ =
    unsafeIslFromIO $ \_ -> c_getTupleId space typ


foreign import ccall "isl_space_to_str" c_toStr :: SpaceRef -> IO C.CString

toStr :: SpaceRef -> String
toStr space =
    unsafePerformIO $ C.peekCString =<< c_toStr space


foreign import ccall "isl_space_get_dim_name" c_getDimName :: SpaceRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: SpaceRef -> DimType -> Int -> String
getDimName space typ pos =
    unsafePerformIO $ C.peekCString =<< c_getDimName space typ (fromIntegral pos)


foreign import ccall "isl_space_get_tuple_name" c_getTupleName :: SpaceRef -> DimType -> IO C.CString

getTupleName :: SpaceRef -> DimType -> String
getTupleName space typ =
    unsafePerformIO $ C.peekCString =<< c_getTupleName space typ


foreign import ccall "isl_space_has_domain_tuple_id" c_hasDomainTupleId :: SpaceRef -> IO C.CBool

hasDomainTupleId :: SpaceRef -> Bool
hasDomainTupleId space =
    unsafePerformIO $ M.toBool <$> c_hasDomainTupleId space


foreign import ccall "isl_space_has_range_tuple_id" c_hasRangeTupleId :: SpaceRef -> IO C.CBool

hasRangeTupleId :: SpaceRef -> Bool
hasRangeTupleId space =
    unsafePerformIO $ M.toBool <$> c_hasRangeTupleId space


foreign import ccall "isl_space_is_equal" c_isEqual :: SpaceRef -> SpaceRef -> IO C.CBool

isEqual :: SpaceRef -> SpaceRef -> Bool
isEqual space1 space2 =
    unsafePerformIO $ M.toBool <$> c_isEqual space1 space2


foreign import ccall "isl_space_is_wrapping" c_isWrapping :: SpaceRef -> IO C.CBool

isWrapping :: SpaceRef -> Bool
isWrapping space =
    unsafePerformIO $ M.toBool <$> c_isWrapping space


foreign import ccall "isl_space_universe_set" c_universeSet :: Space -> IO Set

universeSet :: forall m. MonadIO m => Space %1 -> IslT m Set
universeSet = unsafeCoerce go where
  go :: Space -> IslT m Set
  go space =
    unsafeIslFromIO $ \_ -> c_universeSet space


foreign import ccall "isl_space_curry" c_curry :: Space -> IO Space

curry :: forall m. MonadIO m => Space %1 -> IslT m Space
curry = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_curry space


foreign import ccall "isl_space_domain" c_domain :: Space -> IO Space

domain :: forall m. MonadIO m => Space %1 -> IslT m Space
domain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_domain space


foreign import ccall "isl_space_domain_reverse" c_domainReverse :: Space -> IO Space

domainReverse :: forall m. MonadIO m => Space %1 -> IslT m Space
domainReverse = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_domainReverse space


foreign import ccall "isl_space_drop_all_params" c_dropAllParams :: Space -> IO Space

dropAllParams :: forall m. MonadIO m => Space %1 -> IslT m Space
dropAllParams = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_dropAllParams space


foreign import ccall "isl_space_flatten_domain" c_flattenDomain :: Space -> IO Space

flattenDomain :: forall m. MonadIO m => Space %1 -> IslT m Space
flattenDomain = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_flattenDomain space


foreign import ccall "isl_space_flatten_range" c_flattenRange :: Space -> IO Space

flattenRange :: forall m. MonadIO m => Space %1 -> IslT m Space
flattenRange = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_flattenRange space


foreign import ccall "isl_space_map_from_set" c_mapFromSet :: Space -> IO Space

mapFromSet :: forall m. MonadIO m => Space %1 -> IslT m Space
mapFromSet = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_mapFromSet space


foreign import ccall "isl_space_params" c_params :: Space -> IO Space

params :: forall m. MonadIO m => Space %1 -> IslT m Space
params = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_params space


foreign import ccall "isl_space_product" c_product :: Space -> Space -> IO Space

product :: forall m. MonadIO m => Space %1 -> Space %1 -> IslT m Space
product = unsafeCoerce go where
  go :: Space -> Space -> IslT m Space
  go left right =
    unsafeIslFromIO $ \_ -> c_product left right


foreign import ccall "isl_space_range" c_range :: Space -> IO Space

range :: forall m. MonadIO m => Space %1 -> IslT m Space
range = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_range space


foreign import ccall "isl_space_range_reverse" c_rangeReverse :: Space -> IO Space

rangeReverse :: forall m. MonadIO m => Space %1 -> IslT m Space
rangeReverse = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_rangeReverse space


foreign import ccall "isl_space_reverse" c_reverse :: Space -> IO Space

reverse :: forall m. MonadIO m => Space %1 -> IslT m Space
reverse = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_reverse space


foreign import ccall "isl_space_uncurry" c_uncurry :: Space -> IO Space

uncurry :: forall m. MonadIO m => Space %1 -> IslT m Space
uncurry = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_uncurry space


foreign import ccall "isl_space_unit" c_unit :: Ctx -> IO Space

unit :: MonadIO m => IslT m Space
unit =
    unsafeIslFromIO $ \ctx -> c_unit ctx


foreign import ccall "isl_space_unwrap" c_unwrap :: Space -> IO Space

unwrap :: forall m. MonadIO m => Space %1 -> IslT m Space
unwrap = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_unwrap space


foreign import ccall "isl_space_wrap" c_wrap :: Space -> IO Space

wrap :: forall m. MonadIO m => Space %1 -> IslT m Space
wrap = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_wrap space


foreign import ccall "isl_space_wrapped_reverse" c_wrappedReverse :: Space -> IO Space

wrappedReverse :: forall m. MonadIO m => Space %1 -> IslT m Space
wrappedReverse = unsafeCoerce go where
  go :: Space -> IslT m Space
  go space =
    unsafeIslFromIO $ \_ -> c_wrappedReverse space


foreign import ccall "isl_space_universe_map" c_universeMap :: Space -> IO Map

universeMap :: forall m. MonadIO m => Space %1 -> IslT m Map
universeMap = unsafeCoerce go where
  go :: Space -> IslT m Map
  go space =
    unsafeIslFromIO $ \_ -> c_universeMap space


foreign import ccall "isl_space_zero_aff_on_domain" c_zeroAffOnDomain :: Space -> IO Aff

zeroAffOnDomain :: forall m. MonadIO m => Space %1 -> IslT m Aff
zeroAffOnDomain = unsafeCoerce go where
  go :: Space -> IslT m Aff
  go space =
    unsafeIslFromIO $ \_ -> c_zeroAffOnDomain space


foreign import ccall "isl_space_domain_map_multi_aff" c_domainMapMultiAff :: Space -> IO MultiAff

domainMapMultiAff :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
domainMapMultiAff = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_domainMapMultiAff space


foreign import ccall "isl_space_identity_multi_aff_on_domain" c_identityMultiAffOnDomain :: Space -> IO MultiAff

identityMultiAffOnDomain :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
identityMultiAffOnDomain = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_identityMultiAffOnDomain space


foreign import ccall "isl_space_multi_aff" c_multiAff :: Space -> AffList -> IO MultiAff

multiAff :: forall m. MonadIO m => Space %1 -> AffList %1 -> IslT m MultiAff
multiAff = unsafeCoerce go where
  go :: Space -> AffList -> IslT m MultiAff
  go space list =
    unsafeIslFromIO $ \_ -> c_multiAff space list


foreign import ccall "isl_space_range_map_multi_aff" c_rangeMapMultiAff :: Space -> IO MultiAff

rangeMapMultiAff :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
rangeMapMultiAff = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_rangeMapMultiAff space


foreign import ccall "isl_space_zero_multi_aff" c_zeroMultiAff :: Space -> IO MultiAff

zeroMultiAff :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
zeroMultiAff = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_zeroMultiAff space


foreign import ccall "isl_space_domain_map_pw_multi_aff" c_domainMapPwMultiAff :: Space -> IO PwMultiAff

domainMapPwMultiAff :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
domainMapPwMultiAff = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_domainMapPwMultiAff space


foreign import ccall "isl_space_identity_pw_multi_aff_on_domain" c_identityPwMultiAffOnDomain :: Space -> IO PwMultiAff

identityPwMultiAffOnDomain :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
identityPwMultiAffOnDomain = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_identityPwMultiAffOnDomain space


foreign import ccall "isl_space_range_map_pw_multi_aff" c_rangeMapPwMultiAff :: Space -> IO PwMultiAff

rangeMapPwMultiAff :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
rangeMapPwMultiAff = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_rangeMapPwMultiAff space


foreign import ccall "isl_space_get_domain_tuple_id" c_getDomainTupleId :: SpaceRef -> IO Id

getDomainTupleId :: MonadIO m => SpaceRef -> IslT m Id
getDomainTupleId space =
    unsafeIslFromIO $ \_ -> c_getDomainTupleId space


foreign import ccall "isl_space_get_range_tuple_id" c_getRangeTupleId :: SpaceRef -> IO Id

getRangeTupleId :: MonadIO m => SpaceRef -> IslT m Id
getRangeTupleId space =
    unsafeIslFromIO $ \_ -> c_getRangeTupleId space


foreign import ccall "isl_space_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Space

readFromStr :: MonadIO m => String -> IslT m Space
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_space_free" c_free :: Space -> IO ()

instance Consumable Space where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_space_copy" c_copy :: Space -> IO Space

instance Dupable Space where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow Space SpaceRef where
  borrow = unsafeCoerce $ \(Space ptr) f -> let !r = f (SpaceRef ptr) in (r, Space ptr)


