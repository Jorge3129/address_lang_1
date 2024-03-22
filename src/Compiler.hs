{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import ByteCode
import Control.Arrow ((>>>))
import Data.Map (Map, empty, insert, (!))
import Grammar
import MyUtils
import Value

type LabelOffsetMap = Map String Int

data CompState = CompState
  { curChunk :: Chunk,
    curLine :: Int,
    labelOffsetMap :: LabelOffsetMap,
    labelJumpsToPatch :: [(Int, String)]
  }
  deriving (Eq, Show)

initCs :: CompState
initCs =
  CompState
    { curChunk = initChunk,
      curLine = 0,
      labelOffsetMap = Data.Map.empty,
      labelJumpsToPatch = []
    }

compileProg :: Program -> IO Chunk
compileProg (Program {pLines}) = do
  let numLines = zip pLines [0 :: Int ..]
      cs = initCs
  cs1 <- compileLines numLines cs
  let cs2 = patchLabelJumps cs1
  return $ writeChunk (fromEnum OP_RETURN) (length pLines) (curChunk cs2)

compileLines :: [(ProgLine, Int)] -> CompState -> IO CompState
compileLines (l : ls) cs = compileLine l cs >>= compileLines ls
compileLines [] cs = return cs

compileLine :: (ProgLine, Int) -> CompState -> IO CompState
compileLine (ProgLine {labels, stmts}, lineNum) cs = do
  let cs1 = compileLineLabels labels (cs {curLine = lineNum})
  compileStmts stmts cs1

compileLineLabels :: [String] -> CompState -> CompState
compileLineLabels lbls cs@(CompState {labelOffsetMap, curChunk}) =
  let offset = length $ code $ curChunk
      newLblMap = foldl (\lblMap lbl -> insert lbl offset lblMap) labelOffsetMap lbls
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
compileStmt (ExpSt ex) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_POP cs1
--
compileStmt (Send valEx addrEx) cs = do
  cs1 <- compileExpr valEx cs
  cs2 <- compileExpr addrEx cs1
  return $ emitOpCode OP_SEND cs2
--
compileStmt (Jump lbl) cs = do
  let curOffset = length $ code $ curChunk cs
      cs1 = pushLblToPatch curOffset lbl cs
      cs2 = emitOpCode OP_JUMP cs1
  return $ emitByte 0 cs2
--
compileStmt (Predicate ifExp thenStmts elseStmts) cs = do
  cs1 <- compileExpr ifExp cs
  let (cs2, thenJump) = emitJump OP_JUMP_IF_FALSE cs1
      cs3 = emitOpCode OP_POP cs2
  cs4 <- compileStmts thenStmts cs3
  let (cs5, elseJump) = emitJump OP_JUMP cs4
      cs6 = patchJump thenJump cs5
      cs7 = emitOpCode OP_POP cs6
  cs8 <- compileStmts elseStmts cs7
  return $ patchJump elseJump cs8
--
compileStmt st _ = error $ "cannot compile statement `" ++ show st ++ "` yet"

patchJump :: Int -> CompState -> CompState
patchJump offset cs =
  let ch = curChunk cs
      jump = length (code ch) - offset - 1
      newCh = ch {code = replace offset jump (code ch)}
   in cs {curChunk = newCh}

emitJump :: OpCode -> CompState -> (CompState, Int)
emitJump op cs =
  let cs1 = (emitOpCode op >>> emitByte 0) cs
   in (cs1, length (code (curChunk cs1)) - 1)

compileExpr :: Expr -> CompState -> IO CompState
compileExpr (Lit val) cs = do
  let (cs1, constant) = addConstantToCs (IntVal val) cs
      cs2 = emitOpCode OP_CONSTANT cs1
  return $ emitByte constant cs2
--
compileExpr (BinOpApp op a b) cs = do
  cs1 <- compileExpr a cs
  cs2 <- compileExpr b cs1
  return $ emitOpCode (binOpToOpCode op) cs2
--
compileExpr (Deref ex) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_DEREF cs1
compileExpr ex _ = error $ "cannot compile expression `" ++ show ex ++ "` yet"

emitByte :: Int -> CompState -> CompState
emitByte byte cs@(CompState {curChunk, curLine}) =
  cs
    { curChunk = writeChunk (fromEnum byte) curLine curChunk
    }

emitOpCode :: OpCode -> CompState -> CompState
emitOpCode op = emitByte (fromEnum op)

addConstantToCs :: Value -> CompState -> (CompState, Int)
addConstantToCs val cs@(CompState {curChunk}) =
  let (newCh, constant) = addConstant val curChunk
   in (cs {curChunk = newCh}, constant)

pushLblToPatch :: Int -> String -> CompState -> CompState
pushLblToPatch curOffset lbl cs@(CompState {labelJumpsToPatch}) =
  cs
    { labelJumpsToPatch = labelJumpsToPatch ++ [(curOffset, lbl)]
    }

patchLabelJumps :: CompState -> CompState
patchLabelJumps cs@(CompState {labelJumpsToPatch, labelOffsetMap, curChunk}) =
  let newCh =
        foldl
          ( \ch@(Chunk {code}) (curOffset, lbl) ->
              let jumpToInstr = labelOffsetMap ! lbl
                  jumpOffset = jumpToInstr - curOffset - 2
               in ch {code = replace (curOffset + 1) jumpOffset code}
          )
          curChunk
          labelJumpsToPatch
   in cs {curChunk = newCh}

binOpToOpCode :: BinOp -> OpCode
binOpToOpCode Add = OP_ADD
binOpToOpCode Sub = OP_SUB
binOpToOpCode Mul = OP_MUL
binOpToOpCode Div = OP_DIV
binOpToOpCode Equal = OP_EQUAL
binOpToOpCode Greater = OP_GREATER
binOpToOpCode Less = OP_LESS
binOpToOpCode _ = undefined