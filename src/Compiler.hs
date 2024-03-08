{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import ByteCode
import Grammar

compileProg :: Program -> IO Chunk
compileProg (Program {pLines}) = do
  let ch = initChunk
  return ch

compileLine :: Chunk -> ProgLine -> IO Chunk
compileLine ch (ProgLine {labels, stmts}) = undefined

compileStmt :: Chunk -> Statement -> IO Chunk
compileStmt ch (ExpSt ex) = compileExpr ch ex
compileStmt _ _ = undefined

compileExpr :: Chunk -> Expr -> IO Chunk
compileExpr ch (Lit val) = do
  let (ch1, constant) = addConstant ch (IntVal val)
      ch2 = writeChunk ch1 (fromEnum OP_CONSTANT)
  return $ writeChunk ch2 constant
--
compileExpr ch (BinOpApp Add a b) = do
  ch1 <- compileExpr ch a
  ch2 <- compileExpr ch1 b
  return $ writeChunk ch2 (fromEnum OP_ADD)
compileExpr _ _ = undefined