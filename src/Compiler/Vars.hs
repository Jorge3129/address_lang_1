module Compiler.Vars where

import ByteCode.Core
import Compiler.ProgTreeUtils
import Compiler.State
import Data.List (foldl', nub)
import Grammar
import Value.Core

compileVars :: Program -> CompState -> IO CompState
compileVars pg cs = do
  let vars = nub $ concatMap (exprVars) (progExprs pg)
  print vars
  let cs1 = foldl' (flip compileVar) cs vars
  return $ foldl' (flip compileVarInit) cs1 vars

compileVar :: String -> CompState -> CompState
compileVar name cs =
  let cs1 = emitOpCode OP_ALLOC cs
      (cs2, constant) = addConstantToCs (StringVal name) cs1
      cs3 = emitOpCode OP_DEFINE_VAR cs2
   in emitByte constant cs3

compileVarInit :: String -> CompState -> CompState
compileVarInit name cs =
  let cs1 = emitOpCode OP_ALLOC cs
      (cs2, constant) = addConstantToCs (StringVal name) cs1
      cs3 = emitOpCode OP_SET_VAR cs2
   in emitByte constant cs3