{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Vars where

import ByteCode.Core
import Compiler.ProgTreeUtils
import Compiler.State
import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Parser.AST
import Value.Core

compileGlobalVars :: CompState -> IO ()
compileGlobalVars cs = compileVars (csFnVars cs Map.! "") cs

collectProgVars :: [ProgLine] -> FnVarMap
collectProgVars pLines = snd $ collectVars pLines ("", Map.singleton "" [])
  where
    collectVars :: [ProgLine] -> (String, FnVarMap) -> (String, FnVarMap)
    collectVars = foldr ((>>>) . collectLineVars) id

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

collectProgFns :: [ProgLine] -> LineFnMap
collectProgFns pLines = snd $ collectFns pLines ("", Map.empty)
  where
    collectFns :: [ProgLine] -> (String, LineFnMap) -> (String, LineFnMap)
    collectFns = foldr ((>>>) . collectLineFns) id

collectLineFns :: ProgLine -> (String, LineFnMap) -> (String, LineFnMap)
collectLineFns (ProgLine {lineNum, labels = (fnName : _), stmts = (Send Nil (Var _) : _)}) (prevFnName, fnMap) =
  (fnName, Map.insert lineNum prevFnName fnMap)
collectLineFns (ProgLine {lineNum, stmts = [Ret]}) (fnName, fnMap) = ("", Map.insert lineNum fnName fnMap)
collectLineFns (ProgLine {lineNum}) (fnName, varMap) =
  (fnName, Map.insert lineNum fnName varMap)

compileVars :: [String] -> CompState -> IO ()
compileVars vars cs = do
  forM_ vars (`compileVar` cs)
  forM_ vars (`compileVarInit` cs)

compileVar :: String -> CompState -> IO ()
compileVar name cs = do
  constant <- addConstantToCs (StringVal name) cs
  emitOpCode OP_DEFINE_VAR cs
  emitByte constant cs

compileVarInit :: String -> CompState -> IO ()
compileVarInit name cs = do
  emitOpCode OP_ALLOC cs
  constant <- addConstantToCs (StringVal name) cs
  emitOpCode OP_SET_VAR cs
  emitByte constant cs

toScopedLabel :: String -> CompState -> IO String
toScopedLabel lbl cs = do
  scopes <- getCurScopes cs
  return $ intercalate "." (scopes ++ [lbl])

getCurScopes :: CompState -> IO [String]
getCurScopes cs = do
  curLn <- getCurLine cs
  replacements <- reverse <$> getReplacements cs
  let curFn = csFnMap cs Map.! curLn
  return $ [curFn | not (null curFn)] ++ map (("$r_" ++) . show) (reverse replacements)
