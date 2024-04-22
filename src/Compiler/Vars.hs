{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Vars where

import ByteCode.Core
import Compiler.ProgTreeUtils
import Compiler.State
import Control.Arrow ((>>>))
import Control.Monad (foldM)
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Grammar
import Value.Core

collectProgVars :: Program -> FnVarMap
collectProgVars pg = snd $ collectVars (pLines pg) ("", Map.singleton "" [])

collectVars :: [ProgLine] -> (String, FnVarMap) -> (String, FnVarMap)
collectVars (l : ls) = collectLineVars l >>> collectVars ls
collectVars [] = id

collectLineVars :: ProgLine -> (String, FnVarMap) -> (String, FnVarMap)
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

collectProgFns :: Program -> LineFnMap
collectProgFns pg = snd $ collectFns (pLines pg) ("", Map.empty)

collectFns :: [ProgLine] -> (String, LineFnMap) -> (String, LineFnMap)
collectFns (l : ls) = collectLineFns l >>> collectFns ls
collectFns [] = id

collectLineFns :: ProgLine -> (String, LineFnMap) -> (String, LineFnMap)
collectLineFns (ProgLine {lineNum, labels = (fnName : _), stmts = (Send Nil (Var _) : _)}) (prevFnName, fnMap) =
  (fnName, Map.insert lineNum prevFnName fnMap)
collectLineFns (ProgLine {lineNum, stmts = [Ret]}) (fnName, fnMap) = ("", Map.insert lineNum fnName fnMap)
collectLineFns (ProgLine {lineNum}) (fnName, varMap) =
  (fnName, Map.insert lineNum fnName varMap)

compileVars :: [String] -> CompState -> IO CompState
compileVars vars cs = do
  cs1 <- foldM (flip compileVar) cs vars
  foldM (flip compileVarInit) cs1 vars

compileVar :: String -> CompState -> IO CompState
compileVar name cs = do
  cs1 <- emitOpCode OP_ALLOC cs
  let (cs2, constant) = addConstantToCs (StringVal name) cs1
  cs3 <- emitOpCode OP_DEFINE_VAR cs2
  emitByte constant cs3

compileVarInit :: String -> CompState -> IO CompState
compileVarInit name cs = do
  cs1 <- emitOpCode OP_ALLOC cs
  let (cs2, constant) = addConstantToCs (StringVal name) cs1
  cs3 <- emitOpCode OP_SET_VAR cs2
  emitByte constant cs3

toScopedLabel :: String -> CompState -> IO String
toScopedLabel lbl cs = do
  scopes <- getCurScopes cs
  return $ intercalate "." (scopes ++ [lbl])

getCurScopes :: CompState -> IO [String]
getCurScopes cs@(CompState {csFnMap, csRepls}) = do
  curLn1 <- getCurLine cs
  let curFn = csFnMap Map.! curLn1
  return $ [curFn | not (null curFn)] ++ map (("$r_" ++) . show) (reverse csRepls)
