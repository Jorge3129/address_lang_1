{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import ByteCode
import Grammar
import Value

compileProg :: Program -> IO Chunk
compileProg (Program {pLines}) = do
  let ch = initChunk
  return ch

compileLine :: Chunk -> ProgLine -> IO Chunk
compileLine ch (ProgLine {labels, stmts}) = undefined

compileStmt :: Chunk -> Statement -> IO Chunk
compileStmt ch (BuiltinFunc "print" [ex]) = do
  ch1 <- compileExpr ch ex
  return $ writeChunk (fromEnum OP_PRINT) ch1
--
compileStmt ch (ExpSt ex) = do
  ch1 <- compileExpr ch ex
  return $ writeChunk (fromEnum OP_POP) ch1
compileStmt _ _ = undefined

compileExpr :: Chunk -> Expr -> IO Chunk
compileExpr ch (Lit val) = do
  let (ch1, constant) = addConstant (IntVal val) ch
      ch2 = writeChunk (fromEnum OP_CONSTANT) ch1
  return $ writeChunk constant ch2
--
compileExpr ch (BinOpApp Add a b) = do
  ch1 <- compileExpr ch a
  ch2 <- compileExpr ch1 b
  return $ writeChunk (fromEnum OP_ADD) ch2
compileExpr _ _ = undefined