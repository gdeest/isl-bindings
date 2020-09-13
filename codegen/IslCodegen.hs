module Main where

import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix

import Control.Monad.IO.Class
import Debug.Trace

import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

import Language.C
import Language.C.System.GCC
import Language.C.System.Preprocess

import Language.C.Pretty
import Text.PrettyPrint

import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad

import Language.C.Data.Ident

import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Environment
import System.IO
import System.IO.Unsafe

import qualified Text.Casing as Casing

import Control.Monad.State

import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Set as S

data ISLType
  = INT
  | UINT
  | ISL_DIM_TYPE
  | ISL_SET_PTR
  | ISL_SPACE_PTR
  | ISL_MAP_PTR
  | ISL_BASIC_MAP_PTR
  | ISL_UNION_MAP_PTR
  | ISL_BASIC_SET_PTR
  | ISL_CTX_PTR
  | ISL_VAL_PTR
  | ISL_PW_AFF_PTR
  | ISL_PRINTER_PTR
  | ISL_AFF_PTR
  | ISL_UNION_SET_PTR
  | ISL_MULTI_AFF_PTR
  | ISL_PW_MULTI_AFF_PTR
  | VOID_PTR
  | VOID
  | ISL_PW_QPOLYNOMIAL_PTR
  | ISL_ID_PTR
  | ISL_MULTI_VAL_PTR
  | CHAR_PTR
  | ISL_QPOLYNOMIAL_PTR
  | ISL_PW_QPOLYNOMIAL_FOLD_PTR
  | ISL_MAT_PTR
  | ISL_AST_EXPR_PTR
  | ISL_LOCAL_SPACE_PTR
  | ISL_VEC_PTR
  | ISL_ID_TO_AST_EXPR_PTR
  | ISL_UNION_PW_QPOLYNOMIAL_PTR
  | ISL_UNION_PW_QPOLYNOMIAL_FOLD_PTR
  | ISL_PW_AFF_LIST_PTR
  | ISL_CONSTRAINT_PTR
  | ISL_UNION_PW_MULTI_AFF_PTR
  | ISL_AST_NODE_PTR
  | ISL_POINT_PTR
  | ISL_QPOLYNOMIAL_FOLD_PTR
  | ISL_AST_BUILD_PTR
  | ISL_BAND_LIST_PTR
  | ISL_AFF_LIST_PTR
  | ISL_AST_EXPR_LIST_PTR
  | ISL_AST_NODE_LIST_PTR
  | ISL_ID_LIST_PTR
  | ISL_VAL_LIST_PTR
  | FILE_PTR
  | ISL_BASIC_SET_LIST_PTR
  | ISL_CONSTRAINT_LIST_PTR
  | ISL_SET_LIST_PTR
  | ISL_STREAM_PTR
  | ISL_BAND_PTR
  | ISL_SCHEDULE_CONSTRAINTS_PTR
  | ISL_ID_TO_PW_AFF_PTR
  | ISL_MAP_TO_BASIC_SET_PTR
  | INT_PTR
  | ISL_AST_PRINT_OPTIONS_PTR
  | ISL_SET_PTR_PTR
  | SIZE_T
  | ISL_FOLD
  | ISL_SCHEDULE_PTR
  | ISL_ACCESS_INFO_PTR
  | ISL_TOKEN_PTR
  | ISL_TERM_PTR
  | UINT32_T
  | ISL_RESTRICTION_PTR
  | ISL_HASH_TABLE_PTR
  | ISL_VERTICES_PTR
  | ULONG
  | ISL_FLOW_PTR
  | ISL_VERTEX_PTR
  | LONG
  | ISL_ARGS_PTR
  | ISL_CELL_PTR
  | ISL_UNION_MAP_PTR_PTR
  | ISL_VAL_PTR_PTR
  | ISL_OPTIONS_PTR
  | ISL_OPTIONS_PTR_PTR
  | CHAR_PTR_PTR
  | ISL_ERROR
  | DOUBLE
  | ISL_AST_OP_TYPE
  | ISL_QPOLYNOMIAL_PTR_PTR
  | ISL_TOKEN_TYPE
  | ISL_HASH_TABLE_ENTRY_PTR
  | ISL_AST_NODE_TYPE
  | ISL_AST_EXPR_TYPE
  | ISL_MULTI_PW_AFF_PTR
  | ISL_CONSTRAINT_PTR_PTR
  | ISL_MAT_PTR_PTR
  | MPZ_T
  | ISL_LOCAL_SPACE_PTR_PTR
  | ISL_ACCESS_LEVEL_BEFORE
  | ISL_ACCESS_RESTRICT
  | BOOL
  deriving (Eq, Show, Ord)

data ISLAnnotation
  = ISL_GIVE
  | ISL_TAKE
  | ISL_KEEP
  | ISL_EXPORT
  | ISL_CONSTRUCTOR
  | ISL_NULL
  deriving (Eq, Show, Ord)

type Identifier = String

data ISLParam =
  ISLParam [ISLAnnotation]
           ISLType
           Identifier
  deriving (Eq, Show, Ord)

data ISLFunction =
  ISLFunction [ISLAnnotation]
              ISLType
              Identifier
              [ISLParam]
  deriving (Eq, Show, Ord)

data HSFunction =
  HSFunction ISLFunction
             Identifier
  deriving (Eq, Show, Ord)

data Module =
  Module String
         (S.Set ISLFunction)

type FunctionMap = M.Map String (S.Set HSFunction)

type ParseMonad = StateT FunctionMap IO

-- type ParseMonad = WriterT [Module] IO
defines :: [String]
defines =
  [ "-D__isl_give=__attribute__((isl_give))"
  , "-D__isl_keep=__attribute__((isl_keep))"
  , "-D__isl_take=__attribute__((isl_take))"
  , "-D__isl_export=__attribute__((isl_export))"
  , "-D__isl_null=__attribute__((isl_null))"
  , "-D__isl_constructor=__attribute__((isl_constructor)) __attribute__((isl_export))"
  , "-D__isl_subclass(super)=__attribute__((isl_subclass(\" #super \"))) __attribute__((isl_export))"
  ]

mkHeaderNames :: String -> [String]
mkHeaderNames basePath =
  map ((basePath ++ "/include/isl/") ++)
    [ "space.h"
    , "map.h"
    , "union_map.h"
    , "set.h"
    , "union_set.h"
    , "constraint.h"
    , "aff.h"
    , "val.h"
    , "local_space.h"
    , "id.h"
    , "ilp.h"
    ]

withParsedFile :: MonadIO m => (String -> GlobalDecls -> m a) -> String -> m a
withParsedFile f fileName = do
  let gcc = newGCC "gcc"
      args = defines -- ++ ["-I./include"]
  res <-
    liftIO $ parseCFile gcc (Just "/tmp") args (fileName)
  case res of
    Left err -> error $ "Error parsing " ++ fileName ++ " : " ++ (show err)
    Right translUnit -> do
      let res = runTrav () (analyseAST translUnit)
      case res of
        Left err ->
          error $ "Error analyzing AST of " ++ fileName ++ " : " ++ (show err)
        Right (globalDecls, _) -> f fileName globalDecls

camelize :: String -> String
camelize (x:xs) = (toUpper x) : (camelize' xs)
  where
    camelize' (x:xs)
      | x == '_' = camelize xs
    camelize' (x:xs) = x : (camelize' xs)
    camelize' "" = ""
camelize "" = ""

moduleNameFromHeader :: String -> String
moduleNameFromHeader header = camelize (reverse $ drop 2 $ reverse header)

data TypeInfo = TI
  { cType :: String -- ^ C type
  , hsType :: String -- ^ HSType type
  , cToHs :: Maybe String -- ^ C to HS conversion
  , hsToC :: Maybe String -- ^ HS to C conversion
  , copyable :: Bool
  }

hsTypes :: M.Map ISLType TypeInfo
hsTypes =
  M.fromList
    [ (VOID, TI
        "()"
        "()"
        (Just "return")
        Nothing
        False)
    , (INT, TI
        "C.CInt"
        "Int"
        (Just "return . fromIntegral")
        (Just "return . fromIntegral")
        False)
    , (ISL_DIM_TYPE, TI
        "DimType"
        "DimType"
        (Just "return")
        (Just "return")
        False)
    , (UINT, TI
        "C.CUInt"
        "Int"
        (Just "return . fromIntegral")
        (Just "return . fromIntegral")
        False)
    , (ISL_CTX_PTR, TI
        "Ctx"
        "Ctx"
        (Just "return")
        (Just "return")
        False)
    , (ISL_SET_PTR, TI
        "Set"
        "Set"
        (Just "return")
        (Just "return")
        True)
    , (ISL_BASIC_SET_PTR, TI
        "BasicSet"
        "BasicSet"
        (Just "return")
        (Just "return")
        True)
    , (ISL_UNION_SET_PTR, TI
        "UnionSet"
        "UnionSet"
        (Just "return")
        (Just "return")
        True)
    , (ISL_MAP_PTR, TI
        "Map"
        "Map"
        (Just "return")
        (Just "return")
        True)
    , (ISL_BASIC_MAP_PTR, TI
        "BasicMap"
        "BasicMap"
        (Just "return")
        (Just "return")
        True)
    , (ISL_UNION_MAP_PTR, TI
        "UnionMap"
        "UnionMap"
        (Just "return")
        (Just "return")
        True)
    , (ISL_AFF_PTR, TI
        "Aff"
        "Aff"
        (Just "return")
        (Just "return")
        True)
    , (ISL_VAL_PTR, TI
        "Val"
        "Val"
        (Just "return")
        (Just "return")
        True)
    , (ISL_CONSTRAINT_PTR, TI
        "Constraint"
        "Constraint"
        (Just "return")
        (Just "return")
        True)
    , (ISL_SPACE_PTR, TI
        "Space"
        "Space"
        (Just "return")
        (Just "return")
        True)
    , (ISL_LOCAL_SPACE_PTR, TI
        "LocalSpace"
        "LocalSpace"
        (Just "return")
        (Just "return")
        True)
    , (ISL_ID_PTR, TI
        "Id"
        "Id"
        (Just "return")
        (Just "return")
        True)
    , (CHAR_PTR, TI
         "C.CString"
         "String"
         (Just "C.peekCString")
         (Just "C.newCString")
         False)
    , (BOOL, TI
        "C.CBool"
        "Bool"
        (Just "return . M.toBool")
        (Just "return . M.fromBool")
        False)
    ]

-- mlSigTypes = M.union sigList hsTypes
--   where
--     sigList = M.fromList [(VOID, "unit"), (UINT, "int")]

-- lookupType t | trace (show t) False = undefined
lookupType t = M.lookup t hsTypes

-- lookupSig t = M.lookup t mlSigTypes

-- wrapParam :: ISLParam -> Maybe String
-- wrapParam _ = Nothing

-- wrapRet :: ISLType -> Maybe String
-- wrapRet _ = Nothing

toInDecl :: HSFunction -> Maybe String
toInDecl (HSFunction (ISLFunction annots t name params) hsName) = do
  TI cRetType hsRetType mbToHsRet _  canCopy <- lookupType t


  toHsRet <- mbToHsRet


  let createUnwrap = hsToC
        -- toC <- hsToC pi
        -- if ISL_TAKE `elem` annots
        --   then
        --     case copyable pi of
        --       False -> Nothing
        --       True -> Just $ toC ++ " >=> islCopy"
        --   else return toC

  let filteredParams =
        filter (\(ISLParam _ t _) -> not (t == ISL_CTX_PTR)) params

  -- paramsInfo <-
  --     sequence $ map (lookupType . (\(ISLParam _ t _) -> t)) params
  paramsInfo <-
      sequence $ map (lookupType . (\(ISLParam _ t _) -> t)) filteredParams
  fullParamsInfo <-
      sequence $ map (lookupType . (\(ISLParam _ t _) -> t)) params


  unwrappers <- sequence $ map createUnwrap paramsInfo

  -- wrappers <- sequence $ map cToHs paramsInfo
  wrapper <- mbToHsRet
  -- wraps <- forM params wrapParam
  -- wrapRet <- wrapReturn t

  let importType
        = concat
        . intersperse " -> "
        $ (map cType fullParamsInfo) ++ ["IO " ++ cRetType]

      paramNames =
        map (\(ISLParam _ _ id) -> id) params
      paramNames' =
        map (\(ISLParam _ _ id) -> (id, id++"'")) filteredParams
        -- map (\x -> (x, x++"'")) . filter (/= "ctx") $ paramNames

      cFunName = "c_" ++ hsName
      importCall = unlines $
        [ concat
          ["foreign import ccall \"", name, "\" ", cFunName
          , " :: "
          , importType
          ]
          , ""
        ]
      hsTypeStr = concat $ intersperse " -> " (
          map hsType paramsInfo ++ [hsRetType]
        )
      exportCall = unlines $
        [ concat [hsName, " :: (Given Ctx) => ", hsTypeStr]
        , case paramNames' of
            [] -> concat [hsName, " =  trace \"", hsName, "\" $ "]
            _ -> concat [hsName, " = \\", concat $ intersperse " " (map snd paramNames'), " -> ", "trace \"", hsName, "\" $ "]
        , concat ["    unsafePerformIO $ (", wrapper, ") =<< do"]
        , unlines $ flip map (zip paramNames' unwrappers) $
            \((p, p'), unwrap) -> "      " ++ p ++ " <- (" ++ unwrap ++ ") " ++ p'
        , "      let ctx = given :: Ctx"
        , concat ["      ", cFunName, " "
                 , concat $ intersperse " " paramNames
                 ]
        , ""
        ]

  return (importCall ++ "\n" ++ exportCall)

toDecl :: HSFunction -> Maybe String
toDecl f = Just "Outside Decl"

writeModule :: String -> String -> [HSFunction] -> IO ()
writeModule outPath name functions = do
  let (x:xs) = name
  let name' = (toLower x) : xs
  let dirName = outPath ++ "/src/Isl/" ++ name
  createDirectoryIfMissing True dirName
  withFile (dirName ++ "/AutoGen.hs") WriteMode $ \coreh -> do
    mapM_ (hPutStrLn coreh) $
      [ "{-# LANGUAGE FlexibleContexts #-}"
      , "{-# LANGUAGE ForeignFunctionInterface #-}"
      , "{-# LANGUAGE Strict #-}"
      , ""
      , "module Isl." ++ name ++ ".AutoGen where"
      , ""
      , "import Control.Monad"
      , "import Data.Reflection"
      , "import Isl.Types"
      , "import Debug.Trace"
      , ""
      , "import Foreign.C as C"
      , "import Foreign.C.String as C"
      , "import Foreign.C.Types as C"
      , "import Foreign.ForeignPtr.Unsafe"
      , "import Foreign.Marshal.Utils as M"
      , ""
      , "import System.IO.Unsafe"
      , "import Unsafe.Coerce"
      , ""
      ]
    mapM_ (writeFunction coreh) functions
  where
    writeFunction coreh f@(HSFunction (ISLFunction _ t _ p) name) = do
      let inDecl = toInDecl f
      case inDecl of
        Just d -> do
          hPutStrLn coreh d
          -- where sig = paramTypes ++ retType
          --       params = case p of
          --         (ISLParam _ ISL_CTX_PTR _):ps -> ps
          --         _ -> p
          --       paramTypes = concat $ map (\n -> n ++ " -> ") $ catMaybes (map (\(ISLParam _ t _) -> lookupSig t) params)
          --       Just retType = lookupSig t
        Nothing -> return ()

toISLAnnotation :: Attr -> Maybe ISLAnnotation
toISLAnnotation attr@(Attr (Ident name _ _) _ _) =
  case name of
    "isl_give" -> Just ISL_GIVE
    "isl_take" -> Just ISL_TAKE
    "isl_keep" -> Just ISL_KEEP
    "isl_export" -> Just ISL_EXPORT
    "isl_constructor" -> Just ISL_CONSTRUCTOR
    "isl_null" -> Just ISL_NULL
    _ -> trace ("Unhandled annotation: " ++ (render $ pretty attr)) Nothing

toISLType :: Type -> Maybe ISLType
toISLType t =
  let strType = render $ pretty t
   in case (render $ pretty t) of
        "isl_ctx *" -> Just ISL_CTX_PTR
        "isl_id_to_ast_expr *" -> Just ISL_ID_TO_AST_EXPR_PTR
        "size_t" -> Just SIZE_T
        "long" -> Just LONG
        "isl_val_list *" -> Just ISL_VAL_LIST_PTR
        "isl_val *" -> Just ISL_VAL_PTR
        "const char *" -> Just CHAR_PTR
        "char *" -> Just CHAR_PTR
        "void" -> Just VOID
        "void *" -> Just VOID_PTR
        "int" -> Just INT
        "unsigned" -> Just UINT
        "unsigned int" -> Just UINT
        "double" -> Just DOUBLE
        "isl_multi_val *" -> Just ISL_MULTI_VAL_PTR
        "isl_ast_build *" -> Just ISL_AST_BUILD_PTR
        "isl_union_set *" -> Just ISL_UNION_SET_PTR
        "struct isl_token *" -> Just ISL_TOKEN_PTR
        "enum isl_token_type" -> Just ISL_TOKEN_TYPE
        "struct isl_obj" -> Nothing
        "struct isl_stream *" -> Just ISL_STREAM_PTR
        "enum isl_fold" -> Just ISL_FOLD
        "struct isl_set *" -> Just ISL_SET_PTR
        "struct isl_basic_set *" -> Just ISL_BASIC_SET_PTR
        "struct isl_mat *" -> Just ISL_MAT_PTR
        "struct isl_vec *" -> Just ISL_VEC_PTR
        "struct isl_map *" -> Just ISL_MAP_PTR
        "struct isl_basic_map *" -> Just ISL_BASIC_MAP_PTR
        "isl_basic_map *" -> Just ISL_BASIC_MAP_PTR
        "struct uint32_t" -> Just UINT32_T
        "uint32_t" -> Just UINT32_T
        "isl_union_map *" -> Just ISL_UNION_MAP_PTR
        "struct isl_constraint *" -> Just ISL_CONSTRAINT_PTR
        "isl_constraint *" -> Just ISL_CONSTRAINT_PTR
        "isl_basic_set *" -> Just ISL_BASIC_SET_PTR
        "isl_printer *" -> Just ISL_PRINTER_PTR
        "isl_ast_node *" -> Just ISL_AST_NODE_PTR
        "FILE *" -> Just FILE_PTR
        "isl_set *" -> Just ISL_SET_PTR
        "isl_pw_qpolynomial_fold *" -> Just ISL_PW_QPOLYNOMIAL_FOLD_PTR
        "isl_union_pw_qpolynomial_fold *" ->
          Just ISL_UNION_PW_QPOLYNOMIAL_FOLD_PTR
        "isl_union_pw_qpolynomial *" -> Just ISL_UNION_PW_QPOLYNOMIAL_FOLD_PTR
        "isl_qpolynomial *" -> Just ISL_QPOLYNOMIAL_PTR
        "isl_pw_qpolynomial *" -> Just ISL_PW_QPOLYNOMIAL_PTR
        "isl_band *" -> Just ISL_BAND_PTR
        "isl_band_list *" -> Just ISL_BAND_LIST_PTR
        "struct isl_hash_table *" -> Just ISL_HASH_TABLE_PTR
        "isl_hash_table_entry *" -> Just ISL_HASH_TABLE_ENTRY_PTR
        "isl_constraint_list *" -> Just ISL_CONSTRAINT_LIST_PTR
        "isl_map_to_basic_set *" -> Just ISL_MAP_TO_BASIC_SET_PTR
        "isl_id *" -> Just ISL_ID_PTR
        "isl_local_space *" -> Just ISL_LOCAL_SPACE_PTR
        "enum isl_ast_op_type" -> Just ISL_AST_OP_TYPE
        "isl_ast_print_options *" -> Just ISL_AST_PRINT_OPTIONS_PTR
        "enum isl_ast_node_type" -> Just ISL_AST_NODE_TYPE
        "enum isl_ast_expr_type" -> Just ISL_AST_EXPR_TYPE
        "isl_bool" -> Just INT
        "isl_ast_expr *" -> Just ISL_AST_EXPR_PTR
        "isl_vec *" -> Just ISL_VEC_PTR
        "struct isl_hash_table_entry *" -> Just ISL_HASH_TABLE_ENTRY_PTR
        "isl_id_list *" -> Just ISL_ID_LIST_PTR
        "isl_schedule *" -> Just ISL_SCHEDULE_PTR
        "isl_schedule_constraints *" -> Just ISL_SCHEDULE_CONSTRAINTS_PTR
        "isl_map *" -> Just ISL_MAP_PTR
        "isl_union_pw_multi_aff *" -> Just ISL_UNION_PW_MULTI_AFF_PTR
        "isl_pw_multi_aff *" -> Just ISL_PW_MULTI_AFF_PTR
        "isl_multi_pw_aff *" -> Just ISL_MULTI_PW_AFF_PTR
        "isl_aff *" -> Just ISL_AFF_PTR
        "isl_pw_aff *" -> Just ISL_PW_AFF_PTR
        "isl_multi_aff *" -> Just ISL_MULTI_AFF_PTR
        "isl_id_to_pw_aff *" -> Just ISL_ID_TO_PW_AFF_PTR
        "isl_mat *" -> Just ISL_MAT_PTR
        "struct isl_options *" -> Just ISL_OPTIONS_PTR
        "isl_space *" -> Just ISL_SPACE_PTR
        "isl_access_info *" -> Just ISL_ACCESS_INFO_PTR
        "isl_restriction *" -> Just ISL_RESTRICTION_PTR
        "unsigned long" -> Just ULONG
        "enum isl_error" -> Just ISL_ERROR
        "enum isl_dim_type" -> Just ISL_DIM_TYPE
        "struct isl_ctx *" -> Just ISL_CTX_PTR
        "const void *" -> Just VOID_PTR
        "isl_val * *" -> Just ISL_VAL_PTR_PTR
        "struct isl_val *" -> Just ISL_VAL_PTR
        "struct isl_val_list *" -> Just ISL_VAL_LIST_PTR
        "isl_qpolynomial_fold *" -> Just ISL_QPOLYNOMIAL_FOLD_PTR
        "isl_term *" -> Just ISL_TERM_PTR
        "isl_qpolynomial * *" -> Just ISL_QPOLYNOMIAL_PTR_PTR
        "isl_point *" -> Just ISL_POINT_PTR
        "int *" -> Just INT_PTR
        "struct isl_band *" -> Just ISL_BAND_PTR
        "isl_constraint * *" -> Just ISL_CONSTRAINT_PTR_PTR
        "struct isl_constraint * *" -> Just ISL_CONSTRAINT_PTR_PTR
        "isl_set * *" -> Just ISL_SET_PTR_PTR
        "struct isl_band_list *" -> Just ISL_BAND_LIST_PTR
        "struct isl_constraint_list *" -> Just ISL_CONSTRAINT_LIST_PTR
        "struct isl_basic_set_list *" -> Just ISL_BASIC_SET_LIST_PTR
        "const struct isl_basic_set_list *" -> Just ISL_BASIC_SET_LIST_PTR
        "struct isl_id *" -> Just ISL_ID_PTR
        "isl_vertex *" -> Just ISL_VERTEX_PTR
        "isl_vertices *" -> Just ISL_VERTICES_PTR
        "isl_cell *" -> Just ISL_CELL_PTR
        "struct isl_args *" -> Just ISL_ARGS_PTR
        "const struct isl_basic_map *" -> Just ISL_BASIC_MAP_PTR
        "const struct isl_map *" -> Just ISL_MAP_PTR
        "isl_pw_aff_list *" -> Just ISL_PW_AFF_LIST_PTR
        "struct isl_id_list *" -> Just ISL_ID_LIST_PTR
        "const struct isl_basic_set *" -> Just ISL_BASIC_SET_PTR
        "isl_ast_expr_list *" -> Just ISL_AST_EXPR_LIST_PTR
        "isl_ast_node_list *" -> Just ISL_AST_NODE_LIST_PTR
        "char * *" -> Just CHAR_PTR_PTR
        "isl_aff_list *" -> Just ISL_AFF_LIST_PTR
        "isl_local_space * *" -> Just ISL_LOCAL_SPACE_PTR_PTR
        "struct isl_mat * *" -> Just ISL_MAT_PTR_PTR
        "mpz_t" -> Just MPZ_T
        "mpz_t const" -> Just MPZ_T
        "isl_access_level_before" -> Just ISL_ACCESS_LEVEL_BEFORE
        "isl_union_map * *" -> Just ISL_UNION_MAP_PTR_PTR
        "isl_flow *" -> Just ISL_FLOW_PTR
        "isl_access_restrict" -> Just ISL_ACCESS_RESTRICT
        _ -> Nothing

toISLParam :: ParamDecl -> Maybe ISLParam
-- toISLParam p | trace ("Param: " ++ (render $ pretty p)) False = undefined
toISLParam p = do
  let (Ident name _ _) = declIdent p
  let (DeclAttrs _ _ attrs) = declAttrs p
  islType <- toISLType $ declType p
  islAnnotations <- sequence $ map toISLAnnotation attrs
  return $ ISLParam islAnnotations islType name

toISLFun :: (Ident, IdentDecl) -> Maybe ISLFunction
toISLFun (Ident name _ _, Declaration (Decl (VarDecl _ declAttrs (FunctionType fType fattrs)) _)) =
  case fType of
    FunType t paramDecls _ -> do
      let (DeclAttrs _ _ attrs) = declAttrs
      islAnnots <- sequence $ map toISLAnnotation attrs
      -- let islAnnots = []
      retType <- toISLType t
      islParams <- sequence $ map toISLParam paramDecls
      return $ ISLFunction islAnnots retType name islParams
    -- _ -> trace ("Unhandled function: "++name) Nothing
    _ -> Nothing
toISLFun _ = Nothing

sanitizeFun :: ISLFunction -> ISLFunction
sanitizeFun (ISLFunction annotations t name params) =
  ISLFunction annotations newType name newParams
  where
    newType =
      if t == INT &&
         (any (flip isInfixOf name) ["_is_", "_has_", "_can_"] ||
          any
            (flip isSuffixOf name)
            ["_compatible", "_match", "_lt", "_le", "_gt", "_ge", "_eq", "_ne"])
        then BOOL
        else t
    newParams = map sanitizeParam params
    sanitizeParam param@(ISLParam annotatations t name) =
      case M.lookup name paramReplacements of
        Just newParamName -> ISLParam annotations t newParamName
        Nothing -> param
    paramReplacements =
      M.fromList
        [ ("mod", "modulo")
        , ("type", "typ")
        --, ("val", "value")
        , ("in", "in_")
        -- , ("constraint", "constrnt")
        ]

mustKeepFun :: ISLFunction -> Bool
mustKeepFun (ISLFunction annots _ name _) =
  "equality" `isInfixOf` name ||
  not
    (elem ISL_NULL annots)
    -- ((elem ISL_NULL annots) ||
    --  (isSuffixOf "_free" name) ||
    --  (isSuffixOf "_copy" name) || (isSuffixOf "_get_ctx" name))

    -- ((elem ISL_NULL annots) ||
    --  (isSuffixOf "_free" name) ||
    --  (isSuffixOf "_copy" name) || (isSuffixOf "_get_ctx" name))

moduleMap =
  [ ("isl_aff_", "Aff")
  , ("isl_constraint_", "Constraint")
  , ("isl_local_space_", "LocalSpace")
  , ("isl_map_", "Map")
  , ("isl_id_", "Id")
  , ("isl_set_", "Set")
  , ("isl_space_", "Space")
  , ("isl_union_set_", "UnionSet")
  , ("isl_union_map_", "UnionMap")
  , ("isl_basic_set_", "BasicSet")
  , ("isl_basic_map_", "BasicMap")
  , ("isl_val_", "Val")
  , ("isl_ctx_", "Ctx")
  , ("isl_equality_", "Constraint")
  , ("isl_inequality_", "Constraint")
  ]

sanitizeFunName :: String -> String
sanitizeFunName name =
  case M.lookup name replacements of
    Just repl -> repl
    _ -> name
  where
    replacements =
      M.fromList
        [ ("mod", "modulo")
        , ("2exp", "pow2")
        ]

prefix = ""

islDir = prefix ++ "isl/"

handlePrefix :: String -> String -> String
handlePrefix prefix name =
  case prefix of
    "isl_equality_" -> drop 4 name
    "isl_inequality_" -> drop 4 name
    _ -> drop (length prefix) name

dispatch :: String -> Maybe (String, String)
dispatch name =
  case matches of
    (x:xs) -> Just x
    _ -> Nothing
  where
    matches = catMaybes $ map tryMatch moduleMap
    tryMatch (prefix, modName)
      | prefix `isPrefixOf` name =
        Just ( modName
             , Casing.camel . sanitizeFunName . handlePrefix prefix $ name)

    tryMatch (prefix, modName) = Nothing

addToModule :: HSFunction -> String -> ParseMonad ()
addToModule f name = do
  moduleMap <- get
  let mod = M.findWithDefault S.empty name moduleMap
  put $ M.insert name (S.insert f mod) moduleMap

addFunction :: ISLFunction -> ParseMonad ()
addFunction f
  | mustKeepFun f =
    let f'@(ISLFunction annots t name params) = sanitizeFun f
     in case dispatch (traceShowId name) of
          Nothing -> return $ trace ("Could not dispatch: " ++ name) ()
          Just (mlModuleName, mlFunctionName) ->
            addToModule (HSFunction f' mlFunctionName) mlModuleName
addFunction _ = return ()

collectHeaderFunctions :: String -> ParseMonad ()
collectHeaderFunctions header = do
  liftIO $ putStrLn header
  withParsedFile addFunctions header
  where
    headerFileName = takeFileName header
    addFunctions header decls = do
      islFuns <- mkIslFuns
      liftIO $ print $ length islFuns
      mapM_ addFunction islFuns
      where
        mkIslFuns = do
          let declList = M.toList $ gObjs decls
          catMaybes <$> (sequence $ flip map declList $
            \x@(Ident _ _ ni, _) -> case fileOfNode ni of
              Nothing -> return Nothing
              Just file -> do
                realPath <- liftIO $ canonicalizePath file
                return $
                  if ("/isl/" ++ headerFileName) `isSuffixOf` realPath
                  then toISLFun x
                  else Nothing)

processModule :: String -> (String, S.Set HSFunction) -> IO ()
processModule outPath (name, funs) = do
  putStrLn name
  writeModule outPath name $ S.toList funs

readHeaders :: String -> IO FunctionMap
readHeaders basePath = execStateT collectFunctions M.empty
  where collectFunctions = traverse collectHeaderFunctions headerNames
        headerNames = mkHeaderNames basePath

writeHaskellModules :: String -> FunctionMap -> IO ()
writeHaskellModules outPath funMap = do
  forM_ (M.toList funMap) (processModule outPath)

pipeline :: String -> String -> IO ()
pipeline basePath outPath =
  readHeaders basePath >>=
  writeHaskellModules outPath

main :: IO ()
main = do
  basePath:outPath:_ <- getArgs
  pipeline basePath outPath
  -- let headers = readHeaders basePath
  -- -- Collects header functions into a map:
  -- -- String -> (S.Set HSFunction)
  -- modulesMap <- execStateT (traverse collectHeaderFunctions headers) M.empty
  -- -- Processes the map, writing the module files.
  -- mapM_ processModule (M.toList modulesMap)
