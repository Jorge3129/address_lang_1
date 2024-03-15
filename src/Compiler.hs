{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import ByteCode
import Control.Arrow ((>>>))
import Data.Map (insert, (!))
import Grammar
import MyUtils
import Value

compileProg :: Program -> IO Chunk
compileProg (Program {pLines}) = do
  let numLines = zip pLines [0 :: Int ..]
      ch = initChunk
  ch1 <- compileLines numLines ch
  let ch2 = backPatchLabelJumps ch1
  return $ writeChunk (fromEnum OP_RETURN) (length pLines) ch2

compileLines :: [(ProgLine, Int)] -> Chunk -> IO Chunk
compileLines (l : ls) ch = compileLine l ch >>= compileLines ls
compileLines [] ch = return ch

compileLine :: (ProgLine, Int) -> Chunk -> IO Chunk
compileLine (ProgLine {labels, stmts}, lineNum) ch = do
  let ch1 = compileLineLabels labels ch
  compileStmts stmts lineNum ch1

compileLineLabels :: [String] -> Chunk -> Chunk
compileLineLabels lbls chunk@(Chunk {labelOffsetMap, code}) =
  let offset = length code
      newLblMap = foldl (\lblMap lbl -> insert lbl offset lblMap) labelOffsetMap lbls
   in chunk {labelOffsetMap = newLblMap}

compileStmts :: [Statement] -> Int -> Chunk -> IO Chunk
compileStmts (st : stmts) lineNum ch = compileStmt st lineNum ch >>= compileStmts stmts lineNum
compileStmts [] _ ch = return ch

compileStmt :: Statement -> Int -> Chunk -> IO Chunk
compileStmt Stop lineNum ch = return $ writeChunk (fromEnum OP_RETURN) lineNum ch
--
compileStmt (BuiltinFunc "print" [ex]) lineNum ch = do
  ch1 <- compileExpr ex lineNum ch
  return $ writeChunk (fromEnum OP_PRINT) lineNum ch1
--
compileStmt (ExpSt ex) lineNum ch = do
  ch1 <- compileExpr ex lineNum ch
  return $ writeChunk (fromEnum OP_POP) lineNum ch1
--
compileStmt (Send valEx addrEx) lineNum ch = do
  ch1 <- compileExpr valEx lineNum ch
  ch2 <- compileExpr addrEx lineNum ch1
  return $ writeChunk (fromEnum OP_SEND) lineNum ch2
--
compileStmt (Jump lbl) lineNum ch@(Chunk {code}) = do
  let curOffset = length code
      ch1 = pushLblToBackPatch curOffset lbl ch
      ch2 = writeChunk (fromEnum OP_JUMP) lineNum ch1
  return $ writeChunk 0 lineNum ch2
--
compileStmt (Predicate ifExp thenStmts elseStmts) lineNum ch = do
  ch1 <- compileExpr ifExp lineNum ch
  let ch2 = emitJump OP_JUMP_IF_FALSE lineNum ch1
      thenJump = length (code ch2) - 1
      ch3 = writeChunk (fromEnum OP_POP) lineNum ch2
  ch4 <- compileStmts thenStmts lineNum ch3
  let ch5 = emitJump OP_JUMP lineNum ch4
      elseJump = length (code ch5) - 1
      ch6 = patchJump thenJump ch5
      ch7 = writeChunk (fromEnum OP_POP) lineNum ch6
  ch8 <- compileStmts elseStmts lineNum ch7
  return $ patchJump elseJump ch8
--
compileStmt st _ _ = error $ "cannot compile statement `" ++ show st ++ "` yet"

patchJump :: Int -> Chunk -> Chunk
patchJump offset ch =
  let jump = length (code ch) - offset - 1
   in ch {code = replace offset jump (code ch)}

emitJump :: OpCode -> Int -> Chunk -> Chunk
emitJump op lineNum =
  writeChunk (fromEnum op) lineNum
    >>> writeChunk 0 lineNum

compileExpr :: Expr -> Int -> Chunk -> IO Chunk
compileExpr (Lit val) lineNum ch = do
  let (ch1, constant) = addConstant (IntVal val) ch
      ch2 = writeChunk (fromEnum OP_CONSTANT) lineNum ch1
  return $ writeChunk constant lineNum ch2
--
compileExpr (BinOpApp op a b) lineNum ch = do
  ch1 <- compileExpr a lineNum ch
  ch2 <- compileExpr b lineNum ch1
  return $ writeChunk (fromEnum (binOpToOpCode op)) lineNum ch2
--
compileExpr (Deref ex) lineNum ch = do
  ch1 <- compileExpr ex lineNum ch
  return $ writeChunk (fromEnum OP_DEREF) lineNum ch1
compileExpr ex _ _ = error $ "cannot compile expression `" ++ show ex ++ "` yet"

pushLblToBackPatch :: Int -> String -> Chunk -> Chunk
pushLblToBackPatch curOffset lbl ch@(Chunk {labelJumpsToBackPatch}) =
  ch
    { labelJumpsToBackPatch = labelJumpsToBackPatch ++ [(curOffset, lbl)]
    }

backPatchLabelJumps :: Chunk -> Chunk
backPatchLabelJumps chunk@(Chunk {labelJumpsToBackPatch, labelOffsetMap}) =
  foldl
    ( \ch@(Chunk {code}) (curOffset, lbl) ->
        let jumpToInstr = labelOffsetMap ! lbl
            jumpOffset = jumpToInstr - curOffset - 2
         in ch {code = replace (curOffset + 1) jumpOffset code}
    )
    chunk
    labelJumpsToBackPatch

binOpToOpCode :: BinOp -> OpCode
binOpToOpCode Add = OP_ADD
binOpToOpCode Sub = OP_SUB
binOpToOpCode Mul = OP_MUL
binOpToOpCode Div = OP_DIV
binOpToOpCode Equal = OP_EQUAL
binOpToOpCode Greater = OP_GREATER
binOpToOpCode Less = OP_LESS
binOpToOpCode _ = undefined