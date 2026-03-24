{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.Map.AutoGen where

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

foreign import ccall "isl_map_find_dim_by_id" c_findDimById :: Map -> DimType -> Id -> IO C.CInt


findDimById :: (Given Ctx) => Map -> DimType -> Id -> Int
findDimById = \map' typ' id' -> trace "findDimById" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      map <- (return) map'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_findDimById map typ id


foreign import ccall "isl_map_find_dim_by_name" c_findDimByName :: Map -> DimType -> C.CString -> IO C.CInt


findDimByName :: (Given Ctx) => Map -> DimType -> String -> Int
findDimByName = \map' typ' name' -> trace "findDimByName" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      map <- (return) map'
      typ <- (return) typ'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_findDimByName map typ name


foreign import ccall "isl_map_involves_dims" c_involvesDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt


involvesDims :: (Given Ctx) => Map -> DimType -> Int -> Int -> Int
involvesDims = \map' typ' first' n' -> trace "involvesDims" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      map <- (return) map'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_involvesDims map typ first n


foreign import ccall "isl_map_get_ctx" c_getCtx :: Map -> IO Ctx


getCtx :: (Given Ctx) => Map -> Ctx
getCtx = \map' -> trace "getCtx" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_getCtx map


foreign import ccall "isl_map_dump" c_dump :: Map -> IO ()


dump :: (Given Ctx) => Map -> ()
dump = \map' -> trace "dump" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_dump map


foreign import ccall "isl_map_get_dim_name" c_getDimName :: Map -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => Map -> DimType -> Int -> String
getDimName = \map' typ' pos' -> trace "getDimName" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName map typ pos


foreign import ccall "isl_map_get_tuple_name" c_getTupleName :: Map -> DimType -> IO C.CString


getTupleName :: (Given Ctx) => Map -> DimType -> String
getTupleName = \map' typ' -> trace "getTupleName" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      map <- (return) map'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_getTupleName map typ


foreign import ccall "isl_map_can_curry" c_canCurry :: Map -> IO C.CBool


canCurry :: (Given Ctx) => Map -> Bool
canCurry = \map' -> trace "canCurry" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_canCurry map


foreign import ccall "isl_map_can_range_curry" c_canRangeCurry :: Map -> IO C.CBool


canRangeCurry :: (Given Ctx) => Map -> Bool
canRangeCurry = \map' -> trace "canRangeCurry" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_canRangeCurry map


foreign import ccall "isl_map_can_uncurry" c_canUncurry :: Map -> IO C.CBool


canUncurry :: (Given Ctx) => Map -> Bool
canUncurry = \map' -> trace "canUncurry" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_canUncurry map


foreign import ccall "isl_map_can_zip" c_canZip :: Map -> IO C.CBool


canZip :: (Given Ctx) => Map -> Bool
canZip = \map' -> trace "canZip" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_canZip map


foreign import ccall "isl_map_domain_is_wrapping" c_domainIsWrapping :: Map -> IO C.CBool


domainIsWrapping :: (Given Ctx) => Map -> Bool
domainIsWrapping = \map' -> trace "domainIsWrapping" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_domainIsWrapping map


foreign import ccall "isl_map_has_dim_id" c_hasDimId :: Map -> DimType -> C.CUInt -> IO C.CBool


hasDimId :: (Given Ctx) => Map -> DimType -> Int -> Bool
hasDimId = \map' typ' pos' -> trace "hasDimId" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimId map typ pos


foreign import ccall "isl_map_has_dim_name" c_hasDimName :: Map -> DimType -> C.CUInt -> IO C.CBool


hasDimName :: (Given Ctx) => Map -> DimType -> Int -> Bool
hasDimName = \map' typ' pos' -> trace "hasDimName" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimName map typ pos


foreign import ccall "isl_map_has_equal_space" c_hasEqualSpace :: Map -> Map -> IO C.CBool


hasEqualSpace :: (Given Ctx) => Map -> Map -> Bool
hasEqualSpace = \map1' map2' -> trace "hasEqualSpace" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_hasEqualSpace map1 map2


foreign import ccall "isl_map_has_tuple_id" c_hasTupleId :: Map -> DimType -> IO C.CBool


hasTupleId :: (Given Ctx) => Map -> DimType -> Bool
hasTupleId = \map' typ' -> trace "hasTupleId" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_hasTupleId map typ


foreign import ccall "isl_map_has_tuple_name" c_hasTupleName :: Map -> DimType -> IO C.CBool


hasTupleName :: (Given Ctx) => Map -> DimType -> Bool
hasTupleName = \map' typ' -> trace "hasTupleName" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_hasTupleName map typ


foreign import ccall "isl_map_is_identity" c_isIdentity :: Map -> IO C.CBool


isIdentity :: (Given Ctx) => Map -> Bool
isIdentity = \map' -> trace "isIdentity" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_isIdentity map


foreign import ccall "isl_map_is_product" c_isProduct :: Map -> IO C.CBool


isProduct :: (Given Ctx) => Map -> Bool
isProduct = \map' -> trace "isProduct" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_isProduct map


foreign import ccall "isl_map_is_translation" c_isTranslation :: Map -> IO C.CBool


isTranslation :: (Given Ctx) => Map -> Bool
isTranslation = \map' -> trace "isTranslation" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_isTranslation map


foreign import ccall "isl_map_plain_is_empty" c_plainIsEmpty :: Map -> IO C.CBool


plainIsEmpty :: (Given Ctx) => Map -> Bool
plainIsEmpty = \map' -> trace "plainIsEmpty" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_plainIsEmpty map


foreign import ccall "isl_map_plain_is_equal" c_plainIsEqual :: Map -> Map -> IO C.CBool


plainIsEqual :: (Given Ctx) => Map -> Map -> Bool
plainIsEqual = \map1' map2' -> trace "plainIsEqual" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_plainIsEqual map1 map2


foreign import ccall "isl_map_plain_is_injective" c_plainIsInjective :: Map -> IO C.CBool


plainIsInjective :: (Given Ctx) => Map -> Bool
plainIsInjective = \map' -> trace "plainIsInjective" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_plainIsInjective map


foreign import ccall "isl_map_plain_is_single_valued" c_plainIsSingleValued :: Map -> IO C.CBool


plainIsSingleValued :: (Given Ctx) => Map -> Bool
plainIsSingleValued = \map' -> trace "plainIsSingleValued" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_plainIsSingleValued map


foreign import ccall "isl_map_plain_is_universe" c_plainIsUniverse :: Map -> IO C.CBool


plainIsUniverse :: (Given Ctx) => Map -> Bool
plainIsUniverse = \map' -> trace "plainIsUniverse" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_plainIsUniverse map


foreign import ccall "isl_map_range_is_wrapping" c_rangeIsWrapping :: Map -> IO C.CBool


rangeIsWrapping :: (Given Ctx) => Map -> Bool
rangeIsWrapping = \map' -> trace "rangeIsWrapping" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_rangeIsWrapping map


foreign import ccall "isl_map_add_constraint" c_addConstraint :: Map -> Constraint -> IO Map


addConstraint :: (Given Ctx) => Map -> Constraint -> Map
addConstraint = \map' constraint' -> trace "addConstraint" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_addConstraint map constraint


foreign import ccall "isl_map_add_dims" c_addDims :: Map -> DimType -> C.CUInt -> IO Map


addDims :: (Given Ctx) => Map -> DimType -> Int -> Map
addDims = \map' typ' n' -> trace "addDims" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_addDims map typ n


foreign import ccall "isl_map_align_params" c_alignParams :: Map -> Space -> IO Map


alignParams :: (Given Ctx) => Map -> Space -> Map
alignParams = \map' model' -> trace "alignParams" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      model <- (return) model'

      let ctx = given :: Ctx
      c_alignParams map model


foreign import ccall "isl_map_compute_divs" c_computeDivs :: Map -> IO Map


computeDivs :: (Given Ctx) => Map -> Map
computeDivs = \map' -> trace "computeDivs" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_computeDivs map


foreign import ccall "isl_map_copy" c_copy :: Map -> IO Map


copy :: (Given Ctx) => Map -> Map
copy = \map' -> trace "copy" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_copy map


foreign import ccall "isl_map_deltas_map" c_deltasMap :: Map -> IO Map


deltasMap :: (Given Ctx) => Map -> Map
deltasMap = \map' -> trace "deltasMap" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_deltasMap map


foreign import ccall "isl_map_domain_map" c_domainMap :: Map -> IO Map


domainMap :: (Given Ctx) => Map -> Map
domainMap = \map' -> trace "domainMap" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_domainMap map


foreign import ccall "isl_map_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map


dropConstraintsInvolvingDims :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
dropConstraintsInvolvingDims = \map' typ' first' n' -> trace "dropConstraintsInvolvingDims" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsInvolvingDims map typ first n


foreign import ccall "isl_map_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map


dropConstraintsNotInvolvingDims :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
dropConstraintsNotInvolvingDims = \map' typ' first' n' -> trace "dropConstraintsNotInvolvingDims" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsNotInvolvingDims map typ first n


foreign import ccall "isl_map_eliminate" c_eliminate :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map


eliminate :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
eliminate = \map' typ' first' n' -> trace "eliminate" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_eliminate map typ first n


foreign import ccall "isl_map_equate" c_equate :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map


equate :: (Given Ctx) => Map -> DimType -> Int -> DimType -> Int -> Map
equate = \map' type1' pos1' type2' pos2' -> trace "equate" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_equate map type1 pos1 type2 pos2


foreign import ccall "isl_map_fix_input_si" c_fixInputSi :: Map -> C.CUInt -> C.CInt -> IO Map


fixInputSi :: (Given Ctx) => Map -> Int -> Int -> Map
fixInputSi = \map' input' value' -> trace "fixInputSi" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      input <- (return . fromIntegral) input'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_fixInputSi map input value


foreign import ccall "isl_map_fix_si" c_fixSi :: Map -> DimType -> C.CUInt -> C.CInt -> IO Map


fixSi :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
fixSi = \map' typ' pos' value' -> trace "fixSi" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_fixSi map typ pos value


foreign import ccall "isl_map_fix_val" c_fixVal :: Map -> DimType -> C.CUInt -> Val -> IO Map


fixVal :: (Given Ctx) => Map -> DimType -> Int -> Val -> Map
fixVal = \map' typ' pos' v' -> trace "fixVal" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return) v'

      let ctx = given :: Ctx
      c_fixVal map typ pos v


foreign import ccall "isl_map_fixed_power_val" c_fixedPowerVal :: Map -> Val -> IO Map


fixedPowerVal :: (Given Ctx) => Map -> Val -> Map
fixedPowerVal = \map' exp' -> trace "fixedPowerVal" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      exp <- (return) exp'

      let ctx = given :: Ctx
      c_fixedPowerVal map exp


foreign import ccall "isl_map_flat_domain_product" c_flatDomainProduct :: Map -> Map -> IO Map


flatDomainProduct :: (Given Ctx) => Map -> Map -> Map
flatDomainProduct = \map1' map2' -> trace "flatDomainProduct" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_flatDomainProduct map1 map2


foreign import ccall "isl_map_flat_product" c_flatProduct :: Map -> Map -> IO Map


flatProduct :: (Given Ctx) => Map -> Map -> Map
flatProduct = \map1' map2' -> trace "flatProduct" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_flatProduct map1 map2


foreign import ccall "isl_map_flat_range_product" c_flatRangeProduct :: Map -> Map -> IO Map


flatRangeProduct :: (Given Ctx) => Map -> Map -> Map
flatRangeProduct = \map1' map2' -> trace "flatRangeProduct" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_flatRangeProduct map1 map2


foreign import ccall "isl_map_floordiv_val" c_floordivVal :: Map -> Val -> IO Map


floordivVal :: (Given Ctx) => Map -> Val -> Map
floordivVal = \map' d' -> trace "floordivVal" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      d <- (return) d'

      let ctx = given :: Ctx
      c_floordivVal map d


foreign import ccall "isl_map_from_aff" c_fromAff :: Aff -> IO Map


fromAff :: (Given Ctx) => Aff -> Map
fromAff = \aff' -> trace "fromAff" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_fromAff aff


foreign import ccall "isl_map_from_domain" c_fromDomain :: Set -> IO Map


fromDomain :: (Given Ctx) => Set -> Map
fromDomain = \set' -> trace "fromDomain" $ 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_fromDomain set


foreign import ccall "isl_map_from_domain_and_range" c_fromDomainAndRange :: Set -> Set -> IO Map


fromDomainAndRange :: (Given Ctx) => Set -> Set -> Map
fromDomainAndRange = \domain' range' -> trace "fromDomainAndRange" $ 
    unsafePerformIO $ (return) =<< do
      domain <- (return) domain'
      range <- (return) range'

      let ctx = given :: Ctx
      c_fromDomainAndRange domain range


foreign import ccall "isl_map_from_range" c_fromRange :: Set -> IO Map


fromRange :: (Given Ctx) => Set -> Map
fromRange = \set' -> trace "fromRange" $ 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_fromRange set


foreign import ccall "isl_map_from_union_map" c_fromUnionMap :: UnionMap -> IO Map


fromUnionMap :: (Given Ctx) => UnionMap -> Map
fromUnionMap = \umap' -> trace "fromUnionMap" $ 
    unsafePerformIO $ (return) =<< do
      umap <- (return) umap'

      let ctx = given :: Ctx
      c_fromUnionMap umap


foreign import ccall "isl_map_gist_basic_map" c_gistBasicMap :: Map -> BasicMap -> IO Map


gistBasicMap :: (Given Ctx) => Map -> BasicMap -> Map
gistBasicMap = \map' context' -> trace "gistBasicMap" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistBasicMap map context


foreign import ccall "isl_map_gist_range" c_gistRange :: Map -> Set -> IO Map


gistRange :: (Given Ctx) => Map -> Set -> Map
gistRange = \map' context' -> trace "gistRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistRange map context


foreign import ccall "isl_map_identity" c_identity :: Space -> IO Map


identity :: (Given Ctx) => Space -> Map
identity = \space' -> trace "identity" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_identity space


foreign import ccall "isl_map_insert_dims" c_insertDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map


insertDims :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
insertDims = \map' typ' pos' n' -> trace "insertDims" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_insertDims map typ pos n


foreign import ccall "isl_map_lex_ge" c_lexGe :: Space -> IO Map


lexGe :: (Given Ctx) => Space -> Map
lexGe = \set_space' -> trace "lexGe" $ 
    unsafePerformIO $ (return) =<< do
      set_space <- (return) set_space'

      let ctx = given :: Ctx
      c_lexGe set_space


foreign import ccall "isl_map_lex_ge_first" c_lexGeFirst :: Space -> C.CUInt -> IO Map


lexGeFirst :: (Given Ctx) => Space -> Int -> Map
lexGeFirst = \space' n' -> trace "lexGeFirst" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_lexGeFirst space n


foreign import ccall "isl_map_lex_ge_map" c_lexGeMap :: Map -> Map -> IO Map


lexGeMap :: (Given Ctx) => Map -> Map -> Map
lexGeMap = \map1' map2' -> trace "lexGeMap" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_lexGeMap map1 map2


foreign import ccall "isl_map_lex_gt" c_lexGt :: Space -> IO Map


lexGt :: (Given Ctx) => Space -> Map
lexGt = \set_space' -> trace "lexGt" $ 
    unsafePerformIO $ (return) =<< do
      set_space <- (return) set_space'

      let ctx = given :: Ctx
      c_lexGt set_space


foreign import ccall "isl_map_lex_gt_first" c_lexGtFirst :: Space -> C.CUInt -> IO Map


lexGtFirst :: (Given Ctx) => Space -> Int -> Map
lexGtFirst = \space' n' -> trace "lexGtFirst" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_lexGtFirst space n


foreign import ccall "isl_map_lex_gt_map" c_lexGtMap :: Map -> Map -> IO Map


lexGtMap :: (Given Ctx) => Map -> Map -> Map
lexGtMap = \map1' map2' -> trace "lexGtMap" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_lexGtMap map1 map2


foreign import ccall "isl_map_lex_le" c_lexLe :: Space -> IO Map


lexLe :: (Given Ctx) => Space -> Map
lexLe = \set_space' -> trace "lexLe" $ 
    unsafePerformIO $ (return) =<< do
      set_space <- (return) set_space'

      let ctx = given :: Ctx
      c_lexLe set_space


foreign import ccall "isl_map_lex_le_first" c_lexLeFirst :: Space -> C.CUInt -> IO Map


lexLeFirst :: (Given Ctx) => Space -> Int -> Map
lexLeFirst = \space' n' -> trace "lexLeFirst" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_lexLeFirst space n


foreign import ccall "isl_map_lex_le_map" c_lexLeMap :: Map -> Map -> IO Map


lexLeMap :: (Given Ctx) => Map -> Map -> Map
lexLeMap = \map1' map2' -> trace "lexLeMap" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_lexLeMap map1 map2


foreign import ccall "isl_map_lex_lt" c_lexLt :: Space -> IO Map


lexLt :: (Given Ctx) => Space -> Map
lexLt = \set_space' -> trace "lexLt" $ 
    unsafePerformIO $ (return) =<< do
      set_space <- (return) set_space'

      let ctx = given :: Ctx
      c_lexLt set_space


foreign import ccall "isl_map_lex_lt_first" c_lexLtFirst :: Space -> C.CUInt -> IO Map


lexLtFirst :: (Given Ctx) => Space -> Int -> Map
lexLtFirst = \space' n' -> trace "lexLtFirst" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_lexLtFirst space n


foreign import ccall "isl_map_lex_lt_map" c_lexLtMap :: Map -> Map -> IO Map


lexLtMap :: (Given Ctx) => Map -> Map -> Map
lexLtMap = \map1' map2' -> trace "lexLtMap" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_lexLtMap map1 map2


foreign import ccall "isl_map_lower_bound_si" c_lowerBoundSi :: Map -> DimType -> C.CUInt -> C.CInt -> IO Map


lowerBoundSi :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
lowerBoundSi = \map' typ' pos' value' -> trace "lowerBoundSi" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_lowerBoundSi map typ pos value


foreign import ccall "isl_map_lower_bound_val" c_lowerBoundVal :: Map -> DimType -> C.CUInt -> Val -> IO Map


lowerBoundVal :: (Given Ctx) => Map -> DimType -> Int -> Val -> Map
lowerBoundVal = \map' typ' pos' value' -> trace "lowerBoundVal" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return) value'

      let ctx = given :: Ctx
      c_lowerBoundVal map typ pos value


foreign import ccall "isl_map_make_disjoint" c_makeDisjoint :: Map -> IO Map


makeDisjoint :: (Given Ctx) => Map -> Map
makeDisjoint = \map' -> trace "makeDisjoint" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_makeDisjoint map


foreign import ccall "isl_map_move_dims" c_moveDims :: Map -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Map


moveDims :: (Given Ctx) => Map -> DimType -> Int -> DimType -> Int -> Int -> Map
moveDims = \map' dst_type' dst_pos' src_type' src_pos' n' -> trace "moveDims" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      dst_type <- (return) dst_type'
      dst_pos <- (return . fromIntegral) dst_pos'
      src_type <- (return) src_type'
      src_pos <- (return . fromIntegral) src_pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_moveDims map dst_type dst_pos src_type src_pos n


foreign import ccall "isl_map_nat_universe" c_natUniverse :: Space -> IO Map


natUniverse :: (Given Ctx) => Space -> Map
natUniverse = \space' -> trace "natUniverse" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_natUniverse space


foreign import ccall "isl_map_neg" c_neg :: Map -> IO Map


neg :: (Given Ctx) => Map -> Map
neg = \map' -> trace "neg" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_neg map


foreign import ccall "isl_map_oppose" c_oppose :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map


oppose :: (Given Ctx) => Map -> DimType -> Int -> DimType -> Int -> Map
oppose = \map' type1' pos1' type2' pos2' -> trace "oppose" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_oppose map type1 pos1 type2 pos2


foreign import ccall "isl_map_order_ge" c_orderGe :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map


orderGe :: (Given Ctx) => Map -> DimType -> Int -> DimType -> Int -> Map
orderGe = \map' type1' pos1' type2' pos2' -> trace "orderGe" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_orderGe map type1 pos1 type2 pos2


foreign import ccall "isl_map_order_gt" c_orderGt :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map


orderGt :: (Given Ctx) => Map -> DimType -> Int -> DimType -> Int -> Map
orderGt = \map' type1' pos1' type2' pos2' -> trace "orderGt" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_orderGt map type1 pos1 type2 pos2


foreign import ccall "isl_map_order_le" c_orderLe :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map


orderLe :: (Given Ctx) => Map -> DimType -> Int -> DimType -> Int -> Map
orderLe = \map' type1' pos1' type2' pos2' -> trace "orderLe" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_orderLe map type1 pos1 type2 pos2


foreign import ccall "isl_map_order_lt" c_orderLt :: Map -> DimType -> C.CInt -> DimType -> C.CInt -> IO Map


orderLt :: (Given Ctx) => Map -> DimType -> Int -> DimType -> Int -> Map
orderLt = \map' type1' pos1' type2' pos2' -> trace "orderLt" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_orderLt map type1 pos1 type2 pos2


foreign import ccall "isl_map_project_out" c_projectOut :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map


projectOut :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
projectOut = \map' typ' first' n' -> trace "projectOut" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_projectOut map typ first n


foreign import ccall "isl_map_project_out_param_id" c_projectOutParamId :: Map -> Id -> IO Map


projectOutParamId :: (Given Ctx) => Map -> Id -> Map
projectOutParamId = \map' id' -> trace "projectOutParamId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      id <- (return) id'

      let ctx = given :: Ctx
      c_projectOutParamId map id


foreign import ccall "isl_map_range_curry" c_rangeCurry :: Map -> IO Map


rangeCurry :: (Given Ctx) => Map -> Map
rangeCurry = \map' -> trace "rangeCurry" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_rangeCurry map


foreign import ccall "isl_map_range_map" c_rangeMap :: Map -> IO Map


rangeMap :: (Given Ctx) => Map -> Map
rangeMap = \map' -> trace "rangeMap" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_rangeMap map


foreign import ccall "isl_map_remove_dims" c_removeDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map


removeDims :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
removeDims = \map' typ' first' n' -> trace "removeDims" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDims map typ first n


foreign import ccall "isl_map_remove_divs" c_removeDivs :: Map -> IO Map


removeDivs :: (Given Ctx) => Map -> Map
removeDivs = \map' -> trace "removeDivs" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_removeDivs map


foreign import ccall "isl_map_remove_divs_involving_dims" c_removeDivsInvolvingDims :: Map -> DimType -> C.CUInt -> C.CUInt -> IO Map


removeDivsInvolvingDims :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
removeDivsInvolvingDims = \map' typ' first' n' -> trace "removeDivsInvolvingDims" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDivsInvolvingDims map typ first n


foreign import ccall "isl_map_remove_inputs" c_removeInputs :: Map -> C.CUInt -> C.CUInt -> IO Map


removeInputs :: (Given Ctx) => Map -> Int -> Int -> Map
removeInputs = \map' first' n' -> trace "removeInputs" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeInputs map first n


foreign import ccall "isl_map_remove_redundancies" c_removeRedundancies :: Map -> IO Map


removeRedundancies :: (Given Ctx) => Map -> Map
removeRedundancies = \map' -> trace "removeRedundancies" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_removeRedundancies map


foreign import ccall "isl_map_remove_unknown_divs" c_removeUnknownDivs :: Map -> IO Map


removeUnknownDivs :: (Given Ctx) => Map -> Map
removeUnknownDivs = \map' -> trace "removeUnknownDivs" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_removeUnknownDivs map


foreign import ccall "isl_map_reset_tuple_id" c_resetTupleId :: Map -> DimType -> IO Map


resetTupleId :: (Given Ctx) => Map -> DimType -> Map
resetTupleId = \map' typ' -> trace "resetTupleId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_resetTupleId map typ


foreign import ccall "isl_map_reset_user" c_resetUser :: Map -> IO Map


resetUser :: (Given Ctx) => Map -> Map
resetUser = \map' -> trace "resetUser" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_resetUser map


foreign import ccall "isl_map_set_dim_id" c_setDimId :: Map -> DimType -> C.CUInt -> Id -> IO Map


setDimId :: (Given Ctx) => Map -> DimType -> Int -> Id -> Map
setDimId = \map' typ' pos' id' -> trace "setDimId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setDimId map typ pos id


foreign import ccall "isl_map_set_dim_name" c_setDimName :: Map -> DimType -> C.CUInt -> C.CString -> IO Map


setDimName :: (Given Ctx) => Map -> DimType -> Int -> String -> Map
setDimName = \map' typ' pos' s' -> trace "setDimName" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setDimName map typ pos s


foreign import ccall "isl_map_set_domain_tuple_id" c_setDomainTupleId :: Map -> Id -> IO Map


setDomainTupleId :: (Given Ctx) => Map -> Id -> Map
setDomainTupleId = \map' id' -> trace "setDomainTupleId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setDomainTupleId map id


foreign import ccall "isl_map_set_range_tuple_id" c_setRangeTupleId :: Map -> Id -> IO Map


setRangeTupleId :: (Given Ctx) => Map -> Id -> Map
setRangeTupleId = \map' id' -> trace "setRangeTupleId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setRangeTupleId map id


foreign import ccall "isl_map_set_tuple_id" c_setTupleId :: Map -> DimType -> Id -> IO Map


setTupleId :: (Given Ctx) => Map -> DimType -> Id -> Map
setTupleId = \map' typ' id' -> trace "setTupleId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setTupleId map typ id


foreign import ccall "isl_map_set_tuple_name" c_setTupleName :: Map -> DimType -> C.CString -> IO Map


setTupleName :: (Given Ctx) => Map -> DimType -> String -> Map
setTupleName = \map' typ' s' -> trace "setTupleName" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setTupleName map typ s


foreign import ccall "isl_map_subtract_domain" c_subtractDomain :: Map -> Set -> IO Map


subtractDomain :: (Given Ctx) => Map -> Set -> Map
subtractDomain = \map' dom' -> trace "subtractDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      dom <- (return) dom'

      let ctx = given :: Ctx
      c_subtractDomain map dom


foreign import ccall "isl_map_subtract_range" c_subtractRange :: Map -> Set -> IO Map


subtractRange :: (Given Ctx) => Map -> Set -> Map
subtractRange = \map' dom' -> trace "subtractRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      dom <- (return) dom'

      let ctx = given :: Ctx
      c_subtractRange map dom


foreign import ccall "isl_map_sum" c_sum :: Map -> Map -> IO Map


sum :: (Given Ctx) => Map -> Map -> Map
sum = \map1' map2' -> trace "sum" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_sum map1 map2


foreign import ccall "isl_map_union_disjoint" c_unionDisjoint :: Map -> Map -> IO Map


unionDisjoint :: (Given Ctx) => Map -> Map -> Map
unionDisjoint = \map1' map2' -> trace "unionDisjoint" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_unionDisjoint map1 map2


foreign import ccall "isl_map_upper_bound_si" c_upperBoundSi :: Map -> DimType -> C.CUInt -> C.CInt -> IO Map


upperBoundSi :: (Given Ctx) => Map -> DimType -> Int -> Int -> Map
upperBoundSi = \map' typ' pos' value' -> trace "upperBoundSi" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_upperBoundSi map typ pos value


foreign import ccall "isl_map_upper_bound_val" c_upperBoundVal :: Map -> DimType -> C.CUInt -> Val -> IO Map


upperBoundVal :: (Given Ctx) => Map -> DimType -> Int -> Val -> Map
upperBoundVal = \map' typ' pos' value' -> trace "upperBoundVal" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return) value'

      let ctx = given :: Ctx
      c_upperBoundVal map typ pos value


foreign import ccall "isl_map_convex_hull" c_convexHull :: Map -> IO BasicMap


convexHull :: (Given Ctx) => Map -> BasicMap
convexHull = \map' -> trace "convexHull" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_convexHull map


foreign import ccall "isl_map_plain_unshifted_simple_hull" c_plainUnshiftedSimpleHull :: Map -> IO BasicMap


plainUnshiftedSimpleHull :: (Given Ctx) => Map -> BasicMap
plainUnshiftedSimpleHull = \map' -> trace "plainUnshiftedSimpleHull" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_plainUnshiftedSimpleHull map


foreign import ccall "isl_map_simple_hull" c_simpleHull :: Map -> IO BasicMap


simpleHull :: (Given Ctx) => Map -> BasicMap
simpleHull = \map' -> trace "simpleHull" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_simpleHull map


foreign import ccall "isl_map_plain_get_val_if_fixed" c_plainGetValIfFixed :: Map -> DimType -> C.CUInt -> IO Val


plainGetValIfFixed :: (Given Ctx) => Map -> DimType -> Int -> Val
plainGetValIfFixed = \map' typ' pos' -> trace "plainGetValIfFixed" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_plainGetValIfFixed map typ pos


foreign import ccall "isl_map_get_dim_id" c_getDimId :: Map -> DimType -> C.CUInt -> IO Id


getDimId :: (Given Ctx) => Map -> DimType -> Int -> Id
getDimId = \map' typ' pos' -> trace "getDimId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimId map typ pos


foreign import ccall "isl_map_get_tuple_id" c_getTupleId :: Map -> DimType -> IO Id


getTupleId :: (Given Ctx) => Map -> DimType -> Id
getTupleId = \map' typ' -> trace "getTupleId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_getTupleId map typ


foreign import ccall "isl_map_to_str" c_toStr :: Map -> IO C.CString


toStr :: (Given Ctx) => Map -> String
toStr = \map' -> trace "toStr" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_toStr map


foreign import ccall "isl_map_has_domain_tuple_id" c_hasDomainTupleId :: Map -> IO C.CBool


hasDomainTupleId :: (Given Ctx) => Map -> Bool
hasDomainTupleId = \map' -> trace "hasDomainTupleId" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_hasDomainTupleId map


foreign import ccall "isl_map_has_range_tuple_id" c_hasRangeTupleId :: Map -> IO C.CBool


hasRangeTupleId :: (Given Ctx) => Map -> Bool
hasRangeTupleId = \map' -> trace "hasRangeTupleId" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_hasRangeTupleId map


foreign import ccall "isl_map_is_bijective" c_isBijective :: Map -> IO C.CBool


isBijective :: (Given Ctx) => Map -> Bool
isBijective = \map' -> trace "isBijective" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_isBijective map


foreign import ccall "isl_map_is_disjoint" c_isDisjoint :: Map -> Map -> IO C.CBool


isDisjoint :: (Given Ctx) => Map -> Map -> Bool
isDisjoint = \map1' map2' -> trace "isDisjoint" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_isDisjoint map1 map2


foreign import ccall "isl_map_is_empty" c_isEmpty :: Map -> IO C.CBool


isEmpty :: (Given Ctx) => Map -> Bool
isEmpty = \map' -> trace "isEmpty" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_isEmpty map


foreign import ccall "isl_map_is_equal" c_isEqual :: Map -> Map -> IO C.CBool


isEqual :: (Given Ctx) => Map -> Map -> Bool
isEqual = \map1' map2' -> trace "isEqual" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_isEqual map1 map2


foreign import ccall "isl_map_is_injective" c_isInjective :: Map -> IO C.CBool


isInjective :: (Given Ctx) => Map -> Bool
isInjective = \map' -> trace "isInjective" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_isInjective map


foreign import ccall "isl_map_is_single_valued" c_isSingleValued :: Map -> IO C.CBool


isSingleValued :: (Given Ctx) => Map -> Bool
isSingleValued = \map' -> trace "isSingleValued" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_isSingleValued map


foreign import ccall "isl_map_is_strict_subset" c_isStrictSubset :: Map -> Map -> IO C.CBool


isStrictSubset :: (Given Ctx) => Map -> Map -> Bool
isStrictSubset = \map1' map2' -> trace "isStrictSubset" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_isStrictSubset map1 map2


foreign import ccall "isl_map_is_subset" c_isSubset :: Map -> Map -> IO C.CBool


isSubset :: (Given Ctx) => Map -> Map -> Bool
isSubset = \map1' map2' -> trace "isSubset" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_isSubset map1 map2


foreign import ccall "isl_map_deltas" c_deltas :: Map -> IO Set


deltas :: (Given Ctx) => Map -> Set
deltas = \map' -> trace "deltas" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_deltas map


foreign import ccall "isl_map_domain" c_domain :: Map -> IO Set


domain :: (Given Ctx) => Map -> Set
domain = \bmap' -> trace "domain" $ 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_domain bmap


foreign import ccall "isl_map_params" c_params :: Map -> IO Set


params :: (Given Ctx) => Map -> Set
params = \map' -> trace "params" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_params map


foreign import ccall "isl_map_range" c_range :: Map -> IO Set


range :: (Given Ctx) => Map -> Set
range = \map' -> trace "range" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_range map


foreign import ccall "isl_map_wrap" c_wrap :: Map -> IO Set


wrap :: (Given Ctx) => Map -> Set
wrap = \map' -> trace "wrap" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_wrap map


foreign import ccall "isl_map_get_space" c_getSpace :: Map -> IO Space


getSpace :: (Given Ctx) => Map -> Space
getSpace = \map' -> trace "getSpace" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_getSpace map


foreign import ccall "isl_map_apply_domain" c_applyDomain :: Map -> Map -> IO Map


applyDomain :: (Given Ctx) => Map -> Map -> Map
applyDomain = \map1' map2' -> trace "applyDomain" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_applyDomain map1 map2


foreign import ccall "isl_map_apply_range" c_applyRange :: Map -> Map -> IO Map


applyRange :: (Given Ctx) => Map -> Map -> Map
applyRange = \map1' map2' -> trace "applyRange" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_applyRange map1 map2


foreign import ccall "isl_map_coalesce" c_coalesce :: Map -> IO Map


coalesce :: (Given Ctx) => Map -> Map
coalesce = \map' -> trace "coalesce" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_coalesce map


foreign import ccall "isl_map_complement" c_complement :: Map -> IO Map


complement :: (Given Ctx) => Map -> Map
complement = \map' -> trace "complement" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_complement map


foreign import ccall "isl_map_curry" c_curry :: Map -> IO Map


curry :: (Given Ctx) => Map -> Map
curry = \map' -> trace "curry" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_curry map


foreign import ccall "isl_map_detect_equalities" c_detectEqualities :: Map -> IO Map


detectEqualities :: (Given Ctx) => Map -> Map
detectEqualities = \map' -> trace "detectEqualities" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_detectEqualities map


foreign import ccall "isl_map_domain_factor_domain" c_domainFactorDomain :: Map -> IO Map


domainFactorDomain :: (Given Ctx) => Map -> Map
domainFactorDomain = \map' -> trace "domainFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_domainFactorDomain map


foreign import ccall "isl_map_domain_factor_range" c_domainFactorRange :: Map -> IO Map


domainFactorRange :: (Given Ctx) => Map -> Map
domainFactorRange = \map' -> trace "domainFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_domainFactorRange map


foreign import ccall "isl_map_domain_product" c_domainProduct :: Map -> Map -> IO Map


domainProduct :: (Given Ctx) => Map -> Map -> Map
domainProduct = \map1' map2' -> trace "domainProduct" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_domainProduct map1 map2


foreign import ccall "isl_map_domain_reverse" c_domainReverse :: Map -> IO Map


domainReverse :: (Given Ctx) => Map -> Map
domainReverse = \map' -> trace "domainReverse" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_domainReverse map


foreign import ccall "isl_map_drop_unused_params" c_dropUnusedParams :: Map -> IO Map


dropUnusedParams :: (Given Ctx) => Map -> Map
dropUnusedParams = \map' -> trace "dropUnusedParams" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_dropUnusedParams map


foreign import ccall "isl_map_empty" c_empty :: Space -> IO Map


empty :: (Given Ctx) => Space -> Map
empty = \space' -> trace "empty" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_empty space


foreign import ccall "isl_map_factor_domain" c_factorDomain :: Map -> IO Map


factorDomain :: (Given Ctx) => Map -> Map
factorDomain = \map' -> trace "factorDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_factorDomain map


foreign import ccall "isl_map_factor_range" c_factorRange :: Map -> IO Map


factorRange :: (Given Ctx) => Map -> Map
factorRange = \map' -> trace "factorRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_factorRange map


foreign import ccall "isl_map_flatten" c_flatten :: Map -> IO Map


flatten :: (Given Ctx) => Map -> Map
flatten = \map' -> trace "flatten" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_flatten map


foreign import ccall "isl_map_flatten_domain" c_flattenDomain :: Map -> IO Map


flattenDomain :: (Given Ctx) => Map -> Map
flattenDomain = \map' -> trace "flattenDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_flattenDomain map


foreign import ccall "isl_map_flatten_range" c_flattenRange :: Map -> IO Map


flattenRange :: (Given Ctx) => Map -> Map
flattenRange = \map' -> trace "flattenRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_flattenRange map


foreign import ccall "isl_map_gist" c_gist :: Map -> Map -> IO Map


gist :: (Given Ctx) => Map -> Map -> Map
gist = \map' context' -> trace "gist" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gist map context


foreign import ccall "isl_map_gist_domain" c_gistDomain :: Map -> Set -> IO Map


gistDomain :: (Given Ctx) => Map -> Set -> Map
gistDomain = \map' context' -> trace "gistDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistDomain map context


foreign import ccall "isl_map_gist_params" c_gistParams :: Map -> Set -> IO Map


gistParams :: (Given Ctx) => Map -> Set -> Map
gistParams = \map' context' -> trace "gistParams" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistParams map context


foreign import ccall "isl_map_intersect" c_intersect :: Map -> Map -> IO Map


intersect :: (Given Ctx) => Map -> Map -> Map
intersect = \map1' map2' -> trace "intersect" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_intersect map1 map2


foreign import ccall "isl_map_intersect_domain" c_intersectDomain :: Map -> Set -> IO Map


intersectDomain :: (Given Ctx) => Map -> Set -> Map
intersectDomain = \map' set' -> trace "intersectDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      set <- (return) set'

      let ctx = given :: Ctx
      c_intersectDomain map set


foreign import ccall "isl_map_intersect_domain_factor_domain" c_intersectDomainFactorDomain :: Map -> Map -> IO Map


intersectDomainFactorDomain :: (Given Ctx) => Map -> Map -> Map
intersectDomainFactorDomain = \map' factor' -> trace "intersectDomainFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectDomainFactorDomain map factor


foreign import ccall "isl_map_intersect_domain_factor_range" c_intersectDomainFactorRange :: Map -> Map -> IO Map


intersectDomainFactorRange :: (Given Ctx) => Map -> Map -> Map
intersectDomainFactorRange = \map' factor' -> trace "intersectDomainFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectDomainFactorRange map factor


foreign import ccall "isl_map_intersect_domain_wrapped_domain" c_intersectDomainWrappedDomain :: Map -> Set -> IO Map


intersectDomainWrappedDomain :: (Given Ctx) => Map -> Set -> Map
intersectDomainWrappedDomain = \map' domain' -> trace "intersectDomainWrappedDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      domain <- (return) domain'

      let ctx = given :: Ctx
      c_intersectDomainWrappedDomain map domain


foreign import ccall "isl_map_intersect_params" c_intersectParams :: Map -> Set -> IO Map


intersectParams :: (Given Ctx) => Map -> Set -> Map
intersectParams = \map' params' -> trace "intersectParams" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      params <- (return) params'

      let ctx = given :: Ctx
      c_intersectParams map params


foreign import ccall "isl_map_intersect_range" c_intersectRange :: Map -> Set -> IO Map


intersectRange :: (Given Ctx) => Map -> Set -> Map
intersectRange = \map' set' -> trace "intersectRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      set <- (return) set'

      let ctx = given :: Ctx
      c_intersectRange map set


foreign import ccall "isl_map_intersect_range_factor_domain" c_intersectRangeFactorDomain :: Map -> Map -> IO Map


intersectRangeFactorDomain :: (Given Ctx) => Map -> Map -> Map
intersectRangeFactorDomain = \map' factor' -> trace "intersectRangeFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectRangeFactorDomain map factor


foreign import ccall "isl_map_intersect_range_factor_range" c_intersectRangeFactorRange :: Map -> Map -> IO Map


intersectRangeFactorRange :: (Given Ctx) => Map -> Map -> Map
intersectRangeFactorRange = \map' factor' -> trace "intersectRangeFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      factor <- (return) factor'

      let ctx = given :: Ctx
      c_intersectRangeFactorRange map factor


foreign import ccall "isl_map_intersect_range_wrapped_domain" c_intersectRangeWrappedDomain :: Map -> Set -> IO Map


intersectRangeWrappedDomain :: (Given Ctx) => Map -> Set -> Map
intersectRangeWrappedDomain = \map' domain' -> trace "intersectRangeWrappedDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'
      domain <- (return) domain'

      let ctx = given :: Ctx
      c_intersectRangeWrappedDomain map domain


foreign import ccall "isl_map_lexmax" c_lexmax :: Map -> IO Map


lexmax :: (Given Ctx) => Map -> Map
lexmax = \map' -> trace "lexmax" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_lexmax map


foreign import ccall "isl_map_lexmin" c_lexmin :: Map -> IO Map


lexmin :: (Given Ctx) => Map -> Map
lexmin = \map' -> trace "lexmin" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_lexmin map


foreign import ccall "isl_map_product" c_product :: Map -> Map -> IO Map


product :: (Given Ctx) => Map -> Map -> Map
product = \map1' map2' -> trace "product" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_product map1 map2


foreign import ccall "isl_map_project_out_all_params" c_projectOutAllParams :: Map -> IO Map


projectOutAllParams :: (Given Ctx) => Map -> Map
projectOutAllParams = \map' -> trace "projectOutAllParams" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_projectOutAllParams map


foreign import ccall "isl_map_range_factor_domain" c_rangeFactorDomain :: Map -> IO Map


rangeFactorDomain :: (Given Ctx) => Map -> Map
rangeFactorDomain = \map' -> trace "rangeFactorDomain" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_rangeFactorDomain map


foreign import ccall "isl_map_range_factor_range" c_rangeFactorRange :: Map -> IO Map


rangeFactorRange :: (Given Ctx) => Map -> Map
rangeFactorRange = \map' -> trace "rangeFactorRange" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_rangeFactorRange map


foreign import ccall "isl_map_range_product" c_rangeProduct :: Map -> Map -> IO Map


rangeProduct :: (Given Ctx) => Map -> Map -> Map
rangeProduct = \map1' map2' -> trace "rangeProduct" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_rangeProduct map1 map2


foreign import ccall "isl_map_range_reverse" c_rangeReverse :: Map -> IO Map


rangeReverse :: (Given Ctx) => Map -> Map
rangeReverse = \map' -> trace "rangeReverse" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_rangeReverse map


foreign import ccall "isl_map_reverse" c_reverse :: Map -> IO Map


reverse :: (Given Ctx) => Map -> Map
reverse = \map' -> trace "reverse" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_reverse map


foreign import ccall "isl_map_subtract" c_subtract :: Map -> Map -> IO Map


subtract :: (Given Ctx) => Map -> Map -> Map
subtract = \map1' map2' -> trace "subtract" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_subtract map1 map2


foreign import ccall "isl_map_uncurry" c_uncurry :: Map -> IO Map


uncurry :: (Given Ctx) => Map -> Map
uncurry = \map' -> trace "uncurry" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_uncurry map


foreign import ccall "isl_map_union" c_union :: Map -> Map -> IO Map


union :: (Given Ctx) => Map -> Map -> Map
union = \map1' map2' -> trace "union" $ 
    unsafePerformIO $ (return) =<< do
      map1 <- (return) map1'
      map2 <- (return) map2'

      let ctx = given :: Ctx
      c_union map1 map2


foreign import ccall "isl_map_universe" c_universe :: Space -> IO Map


universe :: (Given Ctx) => Space -> Map
universe = \space' -> trace "universe" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_universe space


foreign import ccall "isl_map_zip" c_zip :: Map -> IO Map


zip :: (Given Ctx) => Map -> Map
zip = \map' -> trace "zip" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_zip map


foreign import ccall "isl_map_affine_hull" c_affineHull :: Map -> IO BasicMap


affineHull :: (Given Ctx) => Map -> BasicMap
affineHull = \map' -> trace "affineHull" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_affineHull map


foreign import ccall "isl_map_polyhedral_hull" c_polyhedralHull :: Map -> IO BasicMap


polyhedralHull :: (Given Ctx) => Map -> BasicMap
polyhedralHull = \map' -> trace "polyhedralHull" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_polyhedralHull map


foreign import ccall "isl_map_sample" c_sample :: Map -> IO BasicMap


sample :: (Given Ctx) => Map -> BasicMap
sample = \map' -> trace "sample" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_sample map


foreign import ccall "isl_map_unshifted_simple_hull" c_unshiftedSimpleHull :: Map -> IO BasicMap


unshiftedSimpleHull :: (Given Ctx) => Map -> BasicMap
unshiftedSimpleHull = \map' -> trace "unshiftedSimpleHull" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_unshiftedSimpleHull map


foreign import ccall "isl_map_to_union_map" c_toUnionMap :: Map -> IO UnionMap


toUnionMap :: (Given Ctx) => Map -> UnionMap
toUnionMap = \map' -> trace "toUnionMap" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_toUnionMap map


foreign import ccall "isl_map_get_domain_tuple_id" c_getDomainTupleId :: Map -> IO Id


getDomainTupleId :: (Given Ctx) => Map -> Id
getDomainTupleId = \map' -> trace "getDomainTupleId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_getDomainTupleId map


foreign import ccall "isl_map_get_range_tuple_id" c_getRangeTupleId :: Map -> IO Id


getRangeTupleId :: (Given Ctx) => Map -> Id
getRangeTupleId = \map' -> trace "getRangeTupleId" $ 
    unsafePerformIO $ (return) =<< do
      map <- (return) map'

      let ctx = given :: Ctx
      c_getRangeTupleId map


foreign import ccall "isl_map_from_basic_map" c_fromBasicMap :: BasicMap -> IO Map


fromBasicMap :: (Given Ctx) => BasicMap -> Map
fromBasicMap = \bmap' -> trace "fromBasicMap" $ 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_fromBasicMap bmap


foreign import ccall "isl_map_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Map


readFromStr :: (Given Ctx) => String -> Map
readFromStr = \str' -> trace "readFromStr" $ 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


