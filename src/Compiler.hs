{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import ByteCode
import Data.Map (insert, (!))
import Grammar
import Value

compileProg :: Program -> IO Chunk
compileProg (Program {pLines}) = do
  let numLines = zip pLines [0 :: Int ..]
      ch = initChunk
  ch1 <- compileLines numLines ch
  return $ writeChunk (fromEnum OP_RETURN) (length pLines) ch1

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
compileStmt (Jump label) lineNum ch@(Chunk {labelOffsetMap, code}) = do
  let curOffset = length code
      jumpToInstr = labelOffsetMap ! label
      jumpOffset = jumpToInstr - curOffset - 2
  let ch1 = writeChunk (fromEnum OP_JUMP) lineNum ch
  return $ writeChunk jumpOffset lineNum ch1
--
compileStmt st _ _ = error $ "cannot compile " ++ show st ++ " yet"

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
compileExpr _ _ _ = undefined

binOpToOpCode :: BinOp -> OpCode
binOpToOpCode Add = OP_ADD
binOpToOpCode Sub = OP_SUB
binOpToOpCode Mul = OP_MUL
binOpToOpCode Div = OP_DIV
binOpToOpCode Equal = OP_EQUAL
binOpToOpCode Greater = OP_GREATER
binOpToOpCode Less = OP_LESS
binOpToOpCode _ = undefined