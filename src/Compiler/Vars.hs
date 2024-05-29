module Compiler.Vars where

import ByteCode.Core
import Compiler.ProgTreeUtils
import Compiler.State
import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Parser.AST
import Utils.Core
import Value.Core

compileGlobalVars :: CompState -> IO ()
compileGlobalVars cs = compileVars (csFnVars cs `readMap` "") cs

collectProgVars :: [ProgLine] -> FnVarMap
collectProgVars pgLines = snd $ collectVars pgLines ("", Map.singleton "" [])
  where
    collectVars :: [ProgLine] -> (String, FnVarMap) -> (String, FnVarMap)
    collectVars = foldr ((>>>) . collectLineVars) id

collectLineVars :: ProgLine -> (String, FnVarMap) -> (String, FnVarMap)
collectLineVars pl (prevFnName, varMap)
  | isFnHead pl =
      let fnName = getFnName pl
       in (fnName, Map.insert fnName newVars varMap)
  | isFnEnd pl = ("", varMap)
  | otherwise =
      let oldSet = varMap `readMap` prevFnName
          newSet = nub $ oldSet ++ newVars
       in (prevFnName, Map.insert prevFnName newSet varMap)
  where
    newVars = nub $ concatMap exprVars (concatMap stmtExprs (stmts pl))

collectProgFns :: [ProgLine] -> LineFnMap
collectProgFns pgLines = snd $ collectFns pgLines ("", Map.empty)
  where
    collectFns :: [ProgLine] -> (String, LineFnMap) -> (String, LineFnMap)
    collectFns = foldr ((>>>) . collectLineFns) id

collectLineFns :: ProgLine -> (String, LineFnMap) -> (String, LineFnMap)
collectLineFns pl@(ProgLine {lineNum = ln}) (prevFnName, fnMap)
  | isFnHead pl = (getFnName pl, Map.insert ln prevFnName fnMap)
  | isFnEnd pl = ("", Map.insert ln prevFnName fnMap)
  | otherwise = (prevFnName, Map.insert ln prevFnName fnMap)

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
  let curFn = csFnMap cs `readMap` curLn
  return $ [curFn | not (null curFn)] ++ map (("$r_" ++) . show) (reverse replacements)
