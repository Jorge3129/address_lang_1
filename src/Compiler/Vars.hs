{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Vars where

import ByteCode.Core
import Compiler.ProgTreeUtils
import Compiler.State
import Control.Arrow ((>>>))
import Data.List (foldl', nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Grammar
import Value.Core

compileVars :: Program -> CompState -> IO CompState
compileVars pg cs = do
  let vars = nub $ concatMap exprVars (progExprs pg)
  print $ collectVars (pLines pg) ("", Map.singleton "" Set.empty)
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

type VarsState = (String, Map.Map String (Set.Set String))

collectVars :: [ProgLine] -> VarsState -> VarsState
collectVars (l : ls) = collectLineVars l >>> collectVars ls
collectVars [] = id

collectLineVars :: ProgLine -> VarsState -> VarsState
collectLineVars (ProgLine {labels = (fnName : _), stmts = args@(Send Nil (Var _) : _)}) (_, varMap) =
  let argVars = Set.fromList $ concatMap exprVars (concatMap stmtExprs args)
   in (fnName, Map.insert fnName argVars varMap)
--
collectLineVars (ProgLine {stmts = [Ret]}) (_, varMap) = ("", varMap)
--
collectLineVars (ProgLine {stmts}) (fnName, varMap) =
  let oldSet = varMap Map.! fnName
      newVars = Set.fromList $ concatMap exprVars (concatMap stmtExprs stmts)
   in (fnName, Map.insert fnName (Set.union oldSet newVars) varMap)
