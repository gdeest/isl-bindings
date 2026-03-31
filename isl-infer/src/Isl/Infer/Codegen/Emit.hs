-- | C source code emission utilities.
--
-- Simple string builder for generating readable C code with proper
-- indentation. Not a pretty-printer library — just enough to emit
-- clean for-loops and function bodies.
module Isl.Infer.Codegen.Emit
  ( CBuilder
  , runCBuilder
  , line
  , blank
  , block
  , indent
  , cInclude
  , cMacro
  , cFuncSig
  ) where

-- | A C code builder is just a list of indented lines, built in reverse.
data CBuilder = CBuilder
  { cbLines  :: [String]  -- reversed
  , cbIndent :: !Int
  }

runCBuilder :: CBuilder -> String
runCBuilder cb = unlines (reverse (cbLines cb))

-- | Empty builder at indent level 0.
empty :: CBuilder
empty = CBuilder [] 0

-- | Append a line at the current indentation level.
line :: String -> CBuilder -> CBuilder
line s cb = cb { cbLines = (replicate (cbIndent cb * 4) ' ' ++ s) : cbLines cb }

-- | Append a blank line.
blank :: CBuilder -> CBuilder
blank cb = cb { cbLines = "" : cbLines cb }

-- | Run a sub-builder inside braces at +1 indent.
block :: (CBuilder -> CBuilder) -> CBuilder -> CBuilder
block body cb =
  let cb1 = line "{" cb
      cb2 = body (cb1 { cbIndent = cbIndent cb + 1 })
      cb3 = cb2 { cbIndent = cbIndent cb }
  in line "}" cb3

-- | Increase indent for a sub-builder (no braces).
indent :: (CBuilder -> CBuilder) -> CBuilder -> CBuilder
indent body cb =
  let cb1 = body (cb { cbIndent = cbIndent cb + 1 })
  in cb1 { cbIndent = cbIndent cb }

-- | Emit @#include "header"@ or @#include <header>@.
cInclude :: String -> CBuilder -> CBuilder
cInclude header = line ("#include " ++ header)

-- | Emit @#define name value@.
cMacro :: String -> String -> CBuilder -> CBuilder
cMacro name value = line ("#define " ++ name ++ " " ++ value)

-- | Emit a function signature line (no body).
cFuncSig :: String -> String -> [(String, String)] -> CBuilder -> CBuilder
cFuncSig retType name params =
  line (retType ++ " " ++ name ++ "(" ++ paramStr ++ ")")
  where
    paramStr = case params of
      [] -> "void"
      ps -> intercalateWith ", " (\(ty, nm) -> ty ++ " " ++ nm) ps

intercalateWith :: String -> (a -> String) -> [a] -> String
intercalateWith _ _ [] = ""
intercalateWith _ f [x] = f x
intercalateWith sep f (x:xs) = f x ++ sep ++ intercalateWith sep f xs
