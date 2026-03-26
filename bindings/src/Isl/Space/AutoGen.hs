{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.Space.AutoGen where

import Control.Monad
import Data.Reflection
import Isl.Types

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall "isl_space_find_dim_by_id" c_findDimById :: Space -> DimType -> Id -> IO C.CInt


findDimById :: (Given Ctx) => Space -> DimType -> Id -> Int
findDimById = \space' typ' id' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      space <- (return) space'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_findDimById space typ id


foreign import ccall "isl_space_find_dim_by_name" c_findDimByName :: Space -> DimType -> C.CString -> IO C.CInt


findDimByName :: (Given Ctx) => Space -> DimType -> String -> Int
findDimByName = \space' typ' name' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      space <- (return) space'
      typ <- (return) typ'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_findDimByName space typ name


foreign import ccall "isl_space_get_ctx" c_getCtx :: Space -> IO Ctx


getCtx :: (Given Ctx) => Space -> Ctx
getCtx = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_getCtx space


foreign import ccall "isl_space_dump" c_dump :: Space -> IO ()


dump :: (Given Ctx) => Space -> ()
dump = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_dump space


foreign import ccall "isl_space_can_curry" c_canCurry :: Space -> IO C.CBool


canCurry :: (Given Ctx) => Space -> Bool
canCurry = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_canCurry space


foreign import ccall "isl_space_can_range_curry" c_canRangeCurry :: Space -> IO C.CBool


canRangeCurry :: (Given Ctx) => Space -> Bool
canRangeCurry = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_canRangeCurry space


foreign import ccall "isl_space_can_uncurry" c_canUncurry :: Space -> IO C.CBool


canUncurry :: (Given Ctx) => Space -> Bool
canUncurry = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_canUncurry space


foreign import ccall "isl_space_can_zip" c_canZip :: Space -> IO C.CBool


canZip :: (Given Ctx) => Space -> Bool
canZip = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_canZip space


foreign import ccall "isl_space_domain_is_wrapping" c_domainIsWrapping :: Space -> IO C.CBool


domainIsWrapping :: (Given Ctx) => Space -> Bool
domainIsWrapping = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domainIsWrapping space


foreign import ccall "isl_space_has_dim_id" c_hasDimId :: Space -> DimType -> C.CUInt -> IO C.CBool


hasDimId :: (Given Ctx) => Space -> DimType -> Int -> Bool
hasDimId = \space' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimId space typ pos


foreign import ccall "isl_space_has_dim_name" c_hasDimName :: Space -> DimType -> C.CUInt -> IO C.CBool


hasDimName :: (Given Ctx) => Space -> DimType -> Int -> Bool
hasDimName = \space' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimName space typ pos


foreign import ccall "isl_space_has_equal_params" c_hasEqualParams :: Space -> Space -> IO C.CBool


hasEqualParams :: (Given Ctx) => Space -> Space -> Bool
hasEqualParams = \space1' space2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space1 <- (return) space1'
      space2 <- (return) space2'

      let ctx = given :: Ctx
      c_hasEqualParams space1 space2


foreign import ccall "isl_space_has_equal_tuples" c_hasEqualTuples :: Space -> Space -> IO C.CBool


hasEqualTuples :: (Given Ctx) => Space -> Space -> Bool
hasEqualTuples = \space1' space2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space1 <- (return) space1'
      space2 <- (return) space2'

      let ctx = given :: Ctx
      c_hasEqualTuples space1 space2


foreign import ccall "isl_space_has_tuple_id" c_hasTupleId :: Space -> DimType -> IO C.CBool


hasTupleId :: (Given Ctx) => Space -> DimType -> Bool
hasTupleId = \space' typ' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_hasTupleId space typ


foreign import ccall "isl_space_has_tuple_name" c_hasTupleName :: Space -> DimType -> IO C.CBool


hasTupleName :: (Given Ctx) => Space -> DimType -> Bool
hasTupleName = \space' typ' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_hasTupleName space typ


foreign import ccall "isl_space_is_domain" c_isDomain :: Space -> Space -> IO C.CBool


isDomain :: (Given Ctx) => Space -> Space -> Bool
isDomain = \space1' space2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space1 <- (return) space1'
      space2 <- (return) space2'

      let ctx = given :: Ctx
      c_isDomain space1 space2


foreign import ccall "isl_space_is_map" c_isMap :: Space -> IO C.CBool


isMap :: (Given Ctx) => Space -> Bool
isMap = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_isMap space


foreign import ccall "isl_space_is_params" c_isParams :: Space -> IO C.CBool


isParams :: (Given Ctx) => Space -> Bool
isParams = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_isParams space


foreign import ccall "isl_space_is_product" c_isProduct :: Space -> IO C.CBool


isProduct :: (Given Ctx) => Space -> Bool
isProduct = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_isProduct space


foreign import ccall "isl_space_is_range" c_isRange :: Space -> Space -> IO C.CBool


isRange :: (Given Ctx) => Space -> Space -> Bool
isRange = \space1' space2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space1 <- (return) space1'
      space2 <- (return) space2'

      let ctx = given :: Ctx
      c_isRange space1 space2


foreign import ccall "isl_space_is_set" c_isSet :: Space -> IO C.CBool


isSet :: (Given Ctx) => Space -> Bool
isSet = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_isSet space


foreign import ccall "isl_space_range_is_wrapping" c_rangeIsWrapping :: Space -> IO C.CBool


rangeIsWrapping :: (Given Ctx) => Space -> Bool
rangeIsWrapping = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeIsWrapping space


foreign import ccall "isl_space_tuple_is_equal" c_tupleIsEqual :: Space -> DimType -> Space -> DimType -> IO C.CBool


tupleIsEqual :: (Given Ctx) => Space -> DimType -> Space -> DimType -> Bool
tupleIsEqual = \space1' type1' space2' type2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space1 <- (return) space1'
      type1 <- (return) type1'
      space2 <- (return) space2'
      type2 <- (return) type2'

      let ctx = given :: Ctx
      c_tupleIsEqual space1 type1 space2 type2


foreign import ccall "isl_space_add_dims" c_addDims :: Space -> DimType -> C.CUInt -> IO Space


addDims :: (Given Ctx) => Space -> DimType -> Int -> Space
addDims = \space' typ' n' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_addDims space typ n


foreign import ccall "isl_space_add_named_tuple_id_ui" c_addNamedTupleIdUi :: Space -> Id -> C.CUInt -> IO Space


addNamedTupleIdUi :: (Given Ctx) => Space -> Id -> Int -> Space
addNamedTupleIdUi = \space' tuple_id' dim' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      tuple_id <- (return) tuple_id'
      dim <- (return . fromIntegral) dim'

      let ctx = given :: Ctx
      c_addNamedTupleIdUi space tuple_id dim


foreign import ccall "isl_space_add_param_id" c_addParamId :: Space -> Id -> IO Space


addParamId :: (Given Ctx) => Space -> Id -> Space
addParamId = \space' id' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      id <- (return) id'

      let ctx = given :: Ctx
      c_addParamId space id


foreign import ccall "isl_space_add_unnamed_tuple_ui" c_addUnnamedTupleUi :: Space -> C.CUInt -> IO Space


addUnnamedTupleUi :: (Given Ctx) => Space -> Int -> Space
addUnnamedTupleUi = \space' dim' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      dim <- (return . fromIntegral) dim'

      let ctx = given :: Ctx
      c_addUnnamedTupleUi space dim


foreign import ccall "isl_space_align_params" c_alignParams :: Space -> Space -> IO Space


alignParams :: (Given Ctx) => Space -> Space -> Space
alignParams = \space1' space2' -> 
    unsafePerformIO $ (return) =<< do
      space1 <- (return) space1'
      space2 <- (return) space2'

      let ctx = given :: Ctx
      c_alignParams space1 space2


foreign import ccall "isl_space_alloc" c_alloc :: Ctx -> C.CUInt -> C.CUInt -> C.CUInt -> IO Space


alloc :: (Given Ctx) => Int -> Int -> Int -> Space
alloc = \nparam' n_in' n_out' -> 
    unsafePerformIO $ (return) =<< do
      nparam <- (return . fromIntegral) nparam'
      n_in <- (return . fromIntegral) n_in'
      n_out <- (return . fromIntegral) n_out'

      let ctx = given :: Ctx
      c_alloc ctx nparam n_in n_out


foreign import ccall "isl_space_copy" c_copy :: Space -> IO Space


copy :: (Given Ctx) => Space -> Space
copy = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_copy space


foreign import ccall "isl_space_domain_factor_domain" c_domainFactorDomain :: Space -> IO Space


domainFactorDomain :: (Given Ctx) => Space -> Space
domainFactorDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domainFactorDomain space


foreign import ccall "isl_space_domain_factor_range" c_domainFactorRange :: Space -> IO Space


domainFactorRange :: (Given Ctx) => Space -> Space
domainFactorRange = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domainFactorRange space


foreign import ccall "isl_space_domain_map" c_domainMap :: Space -> IO Space


domainMap :: (Given Ctx) => Space -> Space
domainMap = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domainMap space


foreign import ccall "isl_space_domain_product" c_domainProduct :: Space -> Space -> IO Space


domainProduct :: (Given Ctx) => Space -> Space -> Space
domainProduct = \left' right' -> 
    unsafePerformIO $ (return) =<< do
      left <- (return) left'
      right <- (return) right'

      let ctx = given :: Ctx
      c_domainProduct left right


foreign import ccall "isl_space_domain_wrapped_domain" c_domainWrappedDomain :: Space -> IO Space


domainWrappedDomain :: (Given Ctx) => Space -> Space
domainWrappedDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domainWrappedDomain space


foreign import ccall "isl_space_domain_wrapped_range" c_domainWrappedRange :: Space -> IO Space


domainWrappedRange :: (Given Ctx) => Space -> Space
domainWrappedRange = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domainWrappedRange space


foreign import ccall "isl_space_drop_dims" c_dropDims :: Space -> DimType -> C.CUInt -> C.CUInt -> IO Space


dropDims :: (Given Ctx) => Space -> DimType -> Int -> Int -> Space
dropDims = \space' typ' first' num' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      num <- (return . fromIntegral) num'

      let ctx = given :: Ctx
      c_dropDims space typ first num


foreign import ccall "isl_space_factor_domain" c_factorDomain :: Space -> IO Space


factorDomain :: (Given Ctx) => Space -> Space
factorDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_factorDomain space


foreign import ccall "isl_space_factor_range" c_factorRange :: Space -> IO Space


factorRange :: (Given Ctx) => Space -> Space
factorRange = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_factorRange space


foreign import ccall "isl_space_from_domain" c_fromDomain :: Space -> IO Space


fromDomain :: (Given Ctx) => Space -> Space
fromDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_fromDomain space


foreign import ccall "isl_space_from_range" c_fromRange :: Space -> IO Space


fromRange :: (Given Ctx) => Space -> Space
fromRange = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_fromRange space


foreign import ccall "isl_space_insert_dims" c_insertDims :: Space -> DimType -> C.CUInt -> C.CUInt -> IO Space


insertDims :: (Given Ctx) => Space -> DimType -> Int -> Int -> Space
insertDims = \space' typ' pos' n' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_insertDims space typ pos n


foreign import ccall "isl_space_join" c_join :: Space -> Space -> IO Space


join :: (Given Ctx) => Space -> Space -> Space
join = \left' right' -> 
    unsafePerformIO $ (return) =<< do
      left <- (return) left'
      right <- (return) right'

      let ctx = given :: Ctx
      c_join left right


foreign import ccall "isl_space_map_from_domain_and_range" c_mapFromDomainAndRange :: Space -> Space -> IO Space


mapFromDomainAndRange :: (Given Ctx) => Space -> Space -> Space
mapFromDomainAndRange = \domain' range' -> 
    unsafePerformIO $ (return) =<< do
      domain <- (return) domain'
      range <- (return) range'

      let ctx = given :: Ctx
      c_mapFromDomainAndRange domain range


foreign import ccall "isl_space_move_dims" c_moveDims :: Space -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Space


moveDims :: (Given Ctx) => Space -> DimType -> Int -> DimType -> Int -> Int -> Space
moveDims = \space' dst_type' dst_pos' src_type' src_pos' n' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      dst_type <- (return) dst_type'
      dst_pos <- (return . fromIntegral) dst_pos'
      src_type <- (return) src_type'
      src_pos <- (return . fromIntegral) src_pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_moveDims space dst_type dst_pos src_type src_pos n


foreign import ccall "isl_space_params_alloc" c_paramsAlloc :: Ctx -> C.CUInt -> IO Space


paramsAlloc :: (Given Ctx) => Int -> Space
paramsAlloc = \nparam' -> 
    unsafePerformIO $ (return) =<< do
      nparam <- (return . fromIntegral) nparam'

      let ctx = given :: Ctx
      c_paramsAlloc ctx nparam


foreign import ccall "isl_space_range_curry" c_rangeCurry :: Space -> IO Space


rangeCurry :: (Given Ctx) => Space -> Space
rangeCurry = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeCurry space


foreign import ccall "isl_space_range_factor_domain" c_rangeFactorDomain :: Space -> IO Space


rangeFactorDomain :: (Given Ctx) => Space -> Space
rangeFactorDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeFactorDomain space


foreign import ccall "isl_space_range_factor_range" c_rangeFactorRange :: Space -> IO Space


rangeFactorRange :: (Given Ctx) => Space -> Space
rangeFactorRange = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeFactorRange space


foreign import ccall "isl_space_range_map" c_rangeMap :: Space -> IO Space


rangeMap :: (Given Ctx) => Space -> Space
rangeMap = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeMap space


foreign import ccall "isl_space_range_product" c_rangeProduct :: Space -> Space -> IO Space


rangeProduct :: (Given Ctx) => Space -> Space -> Space
rangeProduct = \left' right' -> 
    unsafePerformIO $ (return) =<< do
      left <- (return) left'
      right <- (return) right'

      let ctx = given :: Ctx
      c_rangeProduct left right


foreign import ccall "isl_space_range_wrapped_domain" c_rangeWrappedDomain :: Space -> IO Space


rangeWrappedDomain :: (Given Ctx) => Space -> Space
rangeWrappedDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeWrappedDomain space


foreign import ccall "isl_space_range_wrapped_range" c_rangeWrappedRange :: Space -> IO Space


rangeWrappedRange :: (Given Ctx) => Space -> Space
rangeWrappedRange = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeWrappedRange space


foreign import ccall "isl_space_reset_tuple_id" c_resetTupleId :: Space -> DimType -> IO Space


resetTupleId :: (Given Ctx) => Space -> DimType -> Space
resetTupleId = \space' typ' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_resetTupleId space typ


foreign import ccall "isl_space_reset_user" c_resetUser :: Space -> IO Space


resetUser :: (Given Ctx) => Space -> Space
resetUser = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_resetUser space


foreign import ccall "isl_space_set_alloc" c_setAlloc :: Ctx -> C.CUInt -> C.CUInt -> IO Space


setAlloc :: (Given Ctx) => Int -> Int -> Space
setAlloc = \nparam' dim' -> 
    unsafePerformIO $ (return) =<< do
      nparam <- (return . fromIntegral) nparam'
      dim <- (return . fromIntegral) dim'

      let ctx = given :: Ctx
      c_setAlloc ctx nparam dim


foreign import ccall "isl_space_set_dim_id" c_setDimId :: Space -> DimType -> C.CUInt -> Id -> IO Space


setDimId :: (Given Ctx) => Space -> DimType -> Int -> Id -> Space
setDimId = \space' typ' pos' id' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setDimId space typ pos id


foreign import ccall "isl_space_set_dim_name" c_setDimName :: Space -> DimType -> C.CUInt -> C.CString -> IO Space


setDimName :: (Given Ctx) => Space -> DimType -> Int -> String -> Space
setDimName = \space' typ' pos' name' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_setDimName space typ pos name


foreign import ccall "isl_space_set_domain_tuple_id" c_setDomainTupleId :: Space -> Id -> IO Space


setDomainTupleId :: (Given Ctx) => Space -> Id -> Space
setDomainTupleId = \space' id' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setDomainTupleId space id


foreign import ccall "isl_space_set_from_params" c_setFromParams :: Space -> IO Space


setFromParams :: (Given Ctx) => Space -> Space
setFromParams = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_setFromParams space


foreign import ccall "isl_space_set_range_tuple_id" c_setRangeTupleId :: Space -> Id -> IO Space


setRangeTupleId :: (Given Ctx) => Space -> Id -> Space
setRangeTupleId = \space' id' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setRangeTupleId space id


foreign import ccall "isl_space_set_tuple_id" c_setTupleId :: Space -> DimType -> Id -> IO Space


setTupleId :: (Given Ctx) => Space -> DimType -> Id -> Space
setTupleId = \space' typ' id' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setTupleId space typ id


foreign import ccall "isl_space_set_tuple_name" c_setTupleName :: Space -> DimType -> C.CString -> IO Space


setTupleName :: (Given Ctx) => Space -> DimType -> String -> Space
setTupleName = \space' typ' s' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setTupleName space typ s


foreign import ccall "isl_space_zip" c_zip :: Space -> IO Space


zip :: (Given Ctx) => Space -> Space
zip = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_zip space


foreign import ccall "isl_space_param_aff_on_domain_id" c_paramAffOnDomainId :: Space -> Id -> IO Aff


paramAffOnDomainId :: (Given Ctx) => Space -> Id -> Aff
paramAffOnDomainId = \space' id' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      id <- (return) id'

      let ctx = given :: Ctx
      c_paramAffOnDomainId space id


foreign import ccall "isl_space_get_dim_id" c_getDimId :: Space -> DimType -> C.CUInt -> IO Id


getDimId :: (Given Ctx) => Space -> DimType -> Int -> Id
getDimId = \space' typ' pos' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimId space typ pos


foreign import ccall "isl_space_get_tuple_id" c_getTupleId :: Space -> DimType -> IO Id


getTupleId :: (Given Ctx) => Space -> DimType -> Id
getTupleId = \space' typ' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_getTupleId space typ


foreign import ccall "isl_space_to_str" c_toStr :: Space -> IO C.CString


toStr :: (Given Ctx) => Space -> String
toStr = \space' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_toStr space


foreign import ccall "isl_space_get_dim_name" c_getDimName :: Space -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => Space -> DimType -> Int -> String
getDimName = \space' typ' pos' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      space <- (return) space'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName space typ pos


foreign import ccall "isl_space_get_tuple_name" c_getTupleName :: Space -> DimType -> IO C.CString


getTupleName :: (Given Ctx) => Space -> DimType -> String
getTupleName = \space' typ' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      space <- (return) space'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_getTupleName space typ


foreign import ccall "isl_space_has_domain_tuple_id" c_hasDomainTupleId :: Space -> IO C.CBool


hasDomainTupleId :: (Given Ctx) => Space -> Bool
hasDomainTupleId = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_hasDomainTupleId space


foreign import ccall "isl_space_has_range_tuple_id" c_hasRangeTupleId :: Space -> IO C.CBool


hasRangeTupleId :: (Given Ctx) => Space -> Bool
hasRangeTupleId = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_hasRangeTupleId space


foreign import ccall "isl_space_is_equal" c_isEqual :: Space -> Space -> IO C.CBool


isEqual :: (Given Ctx) => Space -> Space -> Bool
isEqual = \space1' space2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space1 <- (return) space1'
      space2 <- (return) space2'

      let ctx = given :: Ctx
      c_isEqual space1 space2


foreign import ccall "isl_space_is_wrapping" c_isWrapping :: Space -> IO C.CBool


isWrapping :: (Given Ctx) => Space -> Bool
isWrapping = \space' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_isWrapping space


foreign import ccall "isl_space_universe_set" c_universeSet :: Space -> IO Set


universeSet :: (Given Ctx) => Space -> Set
universeSet = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_universeSet space


foreign import ccall "isl_space_curry" c_curry :: Space -> IO Space


curry :: (Given Ctx) => Space -> Space
curry = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_curry space


foreign import ccall "isl_space_domain" c_domain :: Space -> IO Space


domain :: (Given Ctx) => Space -> Space
domain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domain space


foreign import ccall "isl_space_domain_reverse" c_domainReverse :: Space -> IO Space


domainReverse :: (Given Ctx) => Space -> Space
domainReverse = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_domainReverse space


foreign import ccall "isl_space_drop_all_params" c_dropAllParams :: Space -> IO Space


dropAllParams :: (Given Ctx) => Space -> Space
dropAllParams = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_dropAllParams space


foreign import ccall "isl_space_flatten_domain" c_flattenDomain :: Space -> IO Space


flattenDomain :: (Given Ctx) => Space -> Space
flattenDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_flattenDomain space


foreign import ccall "isl_space_flatten_range" c_flattenRange :: Space -> IO Space


flattenRange :: (Given Ctx) => Space -> Space
flattenRange = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_flattenRange space


foreign import ccall "isl_space_map_from_set" c_mapFromSet :: Space -> IO Space


mapFromSet :: (Given Ctx) => Space -> Space
mapFromSet = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_mapFromSet space


foreign import ccall "isl_space_params" c_params :: Space -> IO Space


params :: (Given Ctx) => Space -> Space
params = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_params space


foreign import ccall "isl_space_product" c_product :: Space -> Space -> IO Space


product :: (Given Ctx) => Space -> Space -> Space
product = \left' right' -> 
    unsafePerformIO $ (return) =<< do
      left <- (return) left'
      right <- (return) right'

      let ctx = given :: Ctx
      c_product left right


foreign import ccall "isl_space_range" c_range :: Space -> IO Space


range :: (Given Ctx) => Space -> Space
range = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_range space


foreign import ccall "isl_space_range_reverse" c_rangeReverse :: Space -> IO Space


rangeReverse :: (Given Ctx) => Space -> Space
rangeReverse = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_rangeReverse space


foreign import ccall "isl_space_reverse" c_reverse :: Space -> IO Space


reverse :: (Given Ctx) => Space -> Space
reverse = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_reverse space


foreign import ccall "isl_space_uncurry" c_uncurry :: Space -> IO Space


uncurry :: (Given Ctx) => Space -> Space
uncurry = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_uncurry space


foreign import ccall "isl_space_unit" c_unit :: Ctx -> IO Space


unit :: (Given Ctx) => Space
unit =  
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_unit ctx


foreign import ccall "isl_space_unwrap" c_unwrap :: Space -> IO Space


unwrap :: (Given Ctx) => Space -> Space
unwrap = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_unwrap space


foreign import ccall "isl_space_wrap" c_wrap :: Space -> IO Space


wrap :: (Given Ctx) => Space -> Space
wrap = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_wrap space


foreign import ccall "isl_space_wrapped_reverse" c_wrappedReverse :: Space -> IO Space


wrappedReverse :: (Given Ctx) => Space -> Space
wrappedReverse = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_wrappedReverse space


foreign import ccall "isl_space_universe_map" c_universeMap :: Space -> IO Map


universeMap :: (Given Ctx) => Space -> Map
universeMap = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_universeMap space


foreign import ccall "isl_space_zero_aff_on_domain" c_zeroAffOnDomain :: Space -> IO Aff


zeroAffOnDomain :: (Given Ctx) => Space -> Aff
zeroAffOnDomain = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_zeroAffOnDomain space


foreign import ccall "isl_space_get_domain_tuple_id" c_getDomainTupleId :: Space -> IO Id


getDomainTupleId :: (Given Ctx) => Space -> Id
getDomainTupleId = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_getDomainTupleId space


foreign import ccall "isl_space_get_range_tuple_id" c_getRangeTupleId :: Space -> IO Id


getRangeTupleId :: (Given Ctx) => Space -> Id
getRangeTupleId = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_getRangeTupleId space


foreign import ccall "isl_space_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Space


readFromStr :: (Given Ctx) => String -> Space
readFromStr = \str' -> 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


