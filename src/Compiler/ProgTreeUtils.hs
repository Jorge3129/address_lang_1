module Compiler.ProgTreeUtils where

import Compiler.LoopUtils
import Grammar

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
stmtExprs (BuiltinProc _ exs) = exs
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

replaceOpStmt :: Statement -> Replacement -> Statement
replaceOpStmt (Assignment a b) r = Assignment (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpStmt (Send a b) r = Send (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpStmt (Exchange a b) r = Exchange (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpStmt (Predicate cond thenSts elseSts) r =
  Predicate
    (replaceOpExpr cond r)
    (map (`replaceOpStmt` r) thenSts)
    (map (`replaceOpStmt` r) elseSts)
replaceOpStmt st@(LoopSimple {}) r = replaceOpStmt (desugarStmt st) r
replaceOpStmt st@(LoopComplex {}) r = replaceOpStmt (desugarStmt st) r
replaceOpStmt (LoopCommon initSt stepSt endExpr cntExpr a b) r =
  LoopCommon
    (replaceOpStmt initSt r)
    (replaceOpStmt stepSt r)
    (replaceOpExpr endExpr r)
    (replaceOpExpr cntExpr r)
    a
    b
-- stmtExprs initSt ++ stmtExprs stepSt ++ [endExpr] ++ [cntExpr]
replaceOpStmt (BuiltinProc nm args) r = BuiltinProc nm (map (`replaceOpExpr` r) args)
replaceOpStmt (SubprogramCall nm args e) r = SubprogramCall nm (map (`replaceOpExpr` r) args) e
replaceOpStmt (CompJump ex) r = CompJump (ex `replaceOpExpr` r)
replaceOpStmt (ExpSt ex) r = ExpSt (ex `replaceOpExpr` r)
replaceOpStmt st _ = st

replaceOpExpr :: Expr -> Replacement -> Expr
replaceOpExpr (BinOpApp op a b) r@(BinOpReplace opSrc opDst) =
  let newOp = if op == opSrc then opDst else op
   in BinOpApp newOp (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpExpr (Deref a) r = Deref $ replaceOpExpr a r
replaceOpExpr (MulDeref a b) r = MulDeref (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpExpr (Negate a) r = Negate $ replaceOpExpr a r
replaceOpExpr (BuiltinFn nm args) r = BuiltinFn nm (map (`replaceOpExpr` r) args)
replaceOpExpr ex _ = ex
