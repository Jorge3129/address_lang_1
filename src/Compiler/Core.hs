{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Core where

import ByteCode.Core
import Compiler.LoopUtils
import Compiler.State
import Compiler.Vars
import Control.Arrow ((>>>))
import Data.List (foldl')
import Data.Map (insert, (!))
import Data.Maybe (fromMaybe)
import Grammar
import MyUtils
import Value.Core

compileProg :: Program -> IO Chunk
compileProg pg1 = do
  let pg@(Program {pLines}) = numerateLines pg1
      fnVars = collectProgVars pg
      cs = initCs {csFnVars = fnVars}
  csv <- compileVars (fnVars ! "") cs
  cs1 <- compileLines pLines csv
  let cs2 = patchLabelJumps cs1
      ch = curChunk cs2
      ch1 = ch {chLabelMap = labelOffsetMap cs2}
  return $ writeChunk (fromEnum OP_RETURN) (length pLines) ch1

numerateLines :: Program -> Program
numerateLines pg@(Program {pLines}) =
  let numLines = zipWith (\pl i -> pl {lineNum = i}) pLines [0 :: Int ..]
   in pg {pLines = numLines}

compileLines :: [ProgLine] -> CompState -> IO CompState
compileLines (l : ls) cs = compileLine l cs >>= compileLines ls
compileLines [] cs = return cs

compileLine :: ProgLine -> CompState -> IO CompState
compileLine (ProgLine lbls@(fnName : _) args@(Send Nil (Var _) : _) lineNum) cs = do
  let cs1 = compileLineLabels lbls (cs {curLine = lineNum})
  cs2 <- compileVars (csFnVars cs ! fnName) cs1
  compileStmts (reverse args) cs2
--
compileLine pl@(ProgLine {labels, stmts, lineNum}) cs = do
  let cs1 = compileLineLabels labels (cs {curLine = lineNum})
      cs2 = patchLoops (loopPatches cs1) pl cs1
  compileStmts stmts cs2

patchLoops :: [LoopPatch] -> ProgLine -> CompState -> CompState
patchLoops (lp : lps) pl@(ProgLine {labels}) cs =
  let shouldPatch = fromMaybe "" (scopeLabel lp) `elem` labels
   in if shouldPatch
        then patchLoops lps pl (patchLoop lp (cs {loopPatches = lps}))
        else cs
patchLoops [] _ cs = cs

compileLineLabels :: [String] -> CompState -> CompState
compileLineLabels lbls cs@(CompState {labelOffsetMap, curChunk}) =
  let offset = length $ code $ curChunk
      newLblMap = foldl' (\lblMap lbl -> insert lbl offset lblMap) labelOffsetMap lbls
   in cs {labelOffsetMap = newLblMap}

compileStmts :: [Statement] -> CompState -> IO CompState
compileStmts (st : stmts) cs = compileStmt st cs >>= compileStmts stmts
compileStmts [] cs = return cs

compileStmt :: Statement -> CompState -> IO CompState
compileStmt Stop cs = return $ emitOpCode OP_RETURN cs
--
compileStmt (BuiltinFunc "print" [ex]) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_PRINT cs1
--
compileStmt (BuiltinFunc "printRefs" [ex]) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_PRINT_REFS cs1
--
compileStmt (ExpSt ex) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_POP cs1
--
compileStmt (Send valEx (Var name)) cs = do
  cs1 <- compileExpr valEx cs
  cs2 <- compileExpr (Var name) cs1
  let cs3 = emitOpCode OP_SEND cs2
  let (cs4, arg) = addConstantToCs (StringVal name) cs3
  let cs5 = emitOpCode OP_MAKE_VAR_POINTER cs4
  return $ emitByte arg cs5
--
compileStmt (Send valEx (Deref innerExpr)) cs = do
  cs1 <- compileExpr valEx cs
  cs2 <- compileExpr innerExpr cs1
  let cs3 = emitOpCode OP_MAKE_POINTER cs2
  let cs4 = emitOpCode OP_DEREF cs3
  return $ emitOpCode OP_SEND cs4
--
compileStmt (Send valEx addrEx) cs = do
  cs1 <- compileExpr valEx cs
  cs2 <- compileExpr addrEx cs1
  return $ emitOpCode OP_SEND cs2
--
compileStmt (Exchange a b) cs = do
  cs1 <- compileExpr a cs
  cs2 <- compileExpr b cs1
  return $ emitOpCode OP_EXCHANGE cs2
--
compileStmt (Jump lbl) cs = do
  let cs1 = addLabelPatch (curChunkCount cs) lbl cs
      cs2 = emitOpCode OP_JUMP cs1
  return $ emitByte 0 cs2
--
compileStmt (Predicate ifExp thenStmts elseStmts) cs = do
  -- condition
  cs1 <- compileExpr ifExp cs
  let (cs2, toElseJump) = emitJump OP_JUMP_IF_FALSE cs1
  -- then clause
  let cs3 = emitOpCode OP_POP cs2
  cs4 <- compileStmts thenStmts cs3
  let (cs5, toEndJump) = emitJump OP_JUMP cs4
  -- else clause
  let cs6 = patchJump toElseJump cs5
      cs7 = emitOpCode OP_POP cs6
  cs8 <- compileStmts elseStmts cs7
  -- end
  return $ patchJump toEndJump cs8
--
compileStmt st@(LoopSimple {}) cs = compileStmt (desugarStmt st) cs
--
compileStmt st@(LoopComplex {}) cs = compileStmt (desugarStmt st) cs
--
compileStmt (LoopCommon initStmt stepStmt endCondition _ scope next) cs = do
  cs1 <- compileStmt initStmt cs
  let loopStart = curChunkCount cs1
  cs2 <- compileExpr endCondition cs1
  let (cs3, exitJump) = emitJump OP_JUMP_IF_FALSE cs2
      cs4 = emitOpCode OP_POP cs3
      (cs5, bodyJump) = emitJump OP_JUMP cs4
      stepStart = curChunkCount cs5
  cs6 <- compileStmt stepStmt cs5
  let cs7 = emitLoop loopStart cs6
      cs8 = patchJump bodyJump cs7
  return $
    addLoopPatch
      ( LoopPatch
          { scopeLabel = scope,
            nextLabel = next,
            stepStart = stepStart,
            exitJump = exitJump,
            loopLine = curLine cs8
          }
      )
      cs8
--
compileStmt (Assignment (Var name) lhs) cs = do
  csEx <- compileExpr lhs cs
  let (cs1, arg) = addConstantToCs (StringVal name) csEx
  let cs2 = emitOpCode OP_SET_VAR cs1
  return $ emitByte arg cs2
--
compileStmt (SubprogramCall name args _) cs = do
  cs1 <- compileExprs args cs
  let (cs2, constant) = addConstantToCs (StringVal name) cs1
  let cs3 = emitOpCode OP_CALL cs2
  return $ emitByte constant cs3
--
compileStmt Ret cs = return $ emitOpCode OP_RETURN cs
--
compileStmt st _ = error $ "cannot compile statement `" ++ show st ++ "` yet"

compileExprs :: [Expr] -> CompState -> IO CompState
compileExprs (ex : exs) cs = compileExpr ex cs >>= compileExprs exs
compileExprs [] cs = return cs

compileExpr :: Expr -> CompState -> IO CompState
compileExpr (Lit val) cs = do
  let (cs1, constant) = addConstantToCs (IntVal val) cs
      cs2 = emitOpCode OP_CONSTANT cs1
  return $ emitByte constant cs2
--
compileExpr (BinOpApp op a b) cs = do
  cs1 <- compileExpr a cs
  cs2 <- compileExpr b cs1
  return $ emitOpCodes (binOpToOpCode op) cs2
--
compileExpr (Deref ex) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_DEREF cs1
--
compileExpr (Var name) cs = do
  let (cs1, constant) = addConstantToCs (StringVal name) cs
      cs2 = emitOpCode OP_GET_VAR cs1
      cs3 = emitByte constant cs2
  return cs3
--
compileExpr Nil cs = do
  return cs
--
compileExpr ex _ = error $ "cannot compile expression `" ++ show ex ++ "` yet"

emitJump :: OpCode -> CompState -> (CompState, Int)
emitJump op cs =
  let cs1 = (emitOpCode op >>> emitByte 0) cs
   in (cs1, curChunkCount cs1 - 1)

emitLoop :: Int -> CompState -> CompState
emitLoop jumpToInstr cs =
  let jumpOffset = jumpToInstr - curChunkCount cs - 2
   in (emitOpCode OP_JUMP >>> emitByte jumpOffset) cs

patchJump :: Int -> CompState -> CompState
patchJump offset cs =
  let ch = curChunk cs
      jump = curChunkCount cs - offset - 1
      newCh = ch {code = replace offset jump (code ch)}
   in cs {curChunk = newCh}

patchLoop :: LoopPatch -> CompState -> CompState
patchLoop (LoopPatch {stepStart, exitJump}) =
  emitLoop stepStart
    >>> patchJump exitJump
    >>> emitOpCode OP_POP

addLoopPatch :: LoopPatch -> CompState -> CompState
addLoopPatch p cs = cs {loopPatches = p : loopPatches cs}

addLabelPatch :: Int -> String -> CompState -> CompState
addLabelPatch curOffset lbl cs@(CompState {labelJumpsToPatch}) =
  cs
    { labelJumpsToPatch = labelJumpsToPatch ++ [(curOffset, lbl)]
    }

patchLabelJumps :: CompState -> CompState
patchLabelJumps cs@(CompState {labelJumpsToPatch, labelOffsetMap, curChunk}) =
  let newCh =
        foldl'
          ( \ch@(Chunk {code}) (curOffset, lbl) ->
              let jumpToInstr = labelOffsetMap ! lbl
                  jumpOffset = jumpToInstr - curOffset - 2
               in ch {code = replace (curOffset + 1) jumpOffset code}
          )
          curChunk
          labelJumpsToPatch
   in cs {curChunk = newCh}

binOpToOpCode :: BinOp -> [OpCode]
binOpToOpCode Add = [OP_ADD]
binOpToOpCode Sub = [OP_SUB]
binOpToOpCode Mul = [OP_MUL]
binOpToOpCode Div = [OP_DIV]
binOpToOpCode Equal = [OP_EQUAL]
binOpToOpCode Greater = [OP_GREATER]
binOpToOpCode Less = [OP_LESS]
binOpToOpCode NotEqual = [OP_EQUAL, OP_NOT]
binOpToOpCode GreaterEqual = [OP_LESS, OP_NOT]
binOpToOpCode LessEqual = [OP_GREATER, OP_NOT]
binOpToOpCode _ = undefined