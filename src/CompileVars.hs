module CompileVars where

import ByteCode
import CompilerState
import Data.List (foldl')
import Grammar
import ProgTreeUtils
import Value

compileVars :: Program -> CompState -> IO CompState
compileVars pg cs = do
  let vars = concatMap exprVars (progExprs pg)
  print vars
  let cs2 = foldl' (flip compileVar) cs vars
  return cs2

compileVar :: String -> CompState -> CompState
compileVar name cs =
  let cs1 = emitOpCode OP_ALLOC cs
      (cs2, constant) = addConstantToCs (StringVal name) cs1
      cs3 = emitOpCode OP_DEFINE_VAR cs2
   in emitByte constant cs3