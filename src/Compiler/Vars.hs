{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Vars where

import ByteCode.Core
import Compiler.ProgTreeUtils
import Compiler.State
import Control.Arrow ((>>>))
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Grammar
import Value.Core

type VarsState = (String, FnVarMap)

collectProgVars :: Program -> FnVarMap
collectProgVars pg = snd $ collectVars (pLines pg) ("", Map.singleton "" [])

collectVars :: [ProgLine] -> VarsState -> VarsState
collectVars (l : ls) = collectLineVars l >>> collectVars ls
collectVars [] = id

collectLineVars :: ProgLine -> VarsState -> VarsState
collectLineVars (ProgLine {labels = (fnName : _), stmts = args@(Send Nil (Var _) : _)}) (_, varMap) =
  let argVars = nub $ concatMap exprVars (concatMap stmtExprs args)
   in (fnName, Map.insert fnName argVars varMap)
--
collectLineVars (ProgLine {stmts = [Ret]}) (_, varMap) = ("", varMap)
--
collectLineVars (ProgLine {stmts}) (fnName, varMap) =
  let oldSet = varMap Map.! fnName
      newVars = nub $ concatMap exprVars (concatMap stmtExprs stmts)
      newSet = nub $ oldSet ++ newVars
   in (fnName, Map.insert fnName newSet varMap)

compileVars :: [String] -> CompState -> IO CompState
compileVars vars cs = do
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
