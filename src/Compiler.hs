{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import ByteCode
import Grammar
import Value

compileProg :: Program -> IO Chunk
compileProg (Program {pLines}) = do
  let ch = initChunk
  ch1 <- compileLines pLines ch
  return $ writeChunk (fromEnum OP_RETURN) ch1

compileLines :: [ProgLine] -> Chunk -> IO Chunk
compileLines (l : ls) ch = compileLine l ch >>= compileLines ls
compileLines [] ch = return ch

compileLine :: ProgLine -> Chunk -> IO Chunk
compileLine (ProgLine {labels, stmts}) = compileStmts stmts

compileStmts :: [Statement] -> Chunk -> IO Chunk
compileStmts (st : stmts) ch = compileStmt st ch >>= compileStmts stmts
compileStmts [] ch = return ch

compileStmt :: Statement -> Chunk -> IO Chunk
compileStmt Stop ch = return $ writeChunk (fromEnum OP_RETURN) ch
--
compileStmt (BuiltinFunc "print" [ex]) ch = do
  ch1 <- compileExpr ex ch
  return $ writeChunk (fromEnum OP_PRINT) ch1
--
compileStmt (ExpSt ex) ch = do
  ch1 <- compileExpr ex ch
  return $ writeChunk (fromEnum OP_POP) ch1
compileStmt _ _ = undefined

compileExpr :: Expr -> Chunk -> IO Chunk
compileExpr (Lit val) ch = do
  let (ch1, constant) = addConstant (IntVal val) ch
      ch2 = writeChunk (fromEnum OP_CONSTANT) ch1
  return $ writeChunk constant ch2
--
compileExpr (BinOpApp op a b) ch = do
  ch1 <- compileExpr a ch
  ch2 <- compileExpr b ch1
  return $ writeChunk (fromEnum (binOpToOpCode op)) ch2
compileExpr _ _ = undefined

binOpToOpCode :: BinOp -> OpCode
binOpToOpCode Add = OP_ADD
binOpToOpCode Sub = OP_SUB
binOpToOpCode Mul = OP_MUL
binOpToOpCode Div = OP_DIV
binOpToOpCode Equal = OP_EQUAL
binOpToOpCode Greater = OP_GREATER
binOpToOpCode Less = OP_LESS
binOpToOpCode _ = undefined