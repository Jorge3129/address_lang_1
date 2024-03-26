{-# LANGUAGE NamedFieldPuns #-}

module Compiler.ProgTreeUtils where

import Compiler.LoopUtils
import Grammar

progExprs :: Program -> [Expr]
progExprs (Program {pLines}) = concatMap pLineExprs pLines

pLineExprs :: ProgLine -> [Expr]
pLineExprs (ProgLine {stmts}) = concatMap stmtExprs stmts

stmtExprs :: Statement -> [Expr]
stmtExprs (Assignment a b) = [a, b]
stmtExprs (Send a b) = [a, b]
stmtExprs (Exchange a b) = [a, b]
stmtExprs (Predicate cond thenSts elseSts) =
  [cond]
    ++ concatMap stmtExprs thenSts
    ++ concatMap stmtExprs elseSts
stmtExprs st@(LoopSimple {}) = stmtExprs (desugarStmt st)
stmtExprs st@(LoopComplex {}) = stmtExprs (desugarStmt st)
stmtExprs (LoopCommon initSt stepSt endExpr cntExpr _ _) =
  stmtExprs initSt ++ stmtExprs stepSt ++ [endExpr] ++ [cntExpr]
stmtExprs (BuiltinFunc _ exs) = exs
stmtExprs (SubprogramCall _ exs _) = exs
stmtExprs (CompJump ex) = [ex]
stmtExprs (ExpSt ex) = [ex]
stmtExprs _ = []

exprVars :: Expr -> [String]
exprVars (Var name) = [name]
exprVars (BinOpApp _ a b) = exprVars a ++ exprVars b
exprVars (Deref a) = exprVars a
exprVars (MulDeref a b) = exprVars a ++ exprVars b
exprVars _ = []
