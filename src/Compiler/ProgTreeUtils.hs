module Compiler.ProgTreeUtils where

import Parser.AST

desugarStmt :: Statement -> Statement
desugarStmt (LoopSimple initVal step end counter scope next) =
  LoopCommon
    (Send initVal counter)
    (getLoopStepStmt step counter)
    (getLoopEndExpr end counter)
    counter
    scope
    next
desugarStmt s = s

getLoopStepStmt :: LoopStep -> Expr -> Statement
getLoopStepStmt (LoopStepValue stepVal) counter = Send (BinOpApp Add (Deref counter) stepVal) counter
getLoopStepStmt (LoopStepExpr stepExpr) counter = Send (replaceNil stepExpr (Deref counter)) counter

getLoopEndExpr :: LoopEnd -> Expr -> Expr
getLoopEndExpr (LoopEndValue val) counter = BinOpApp LessEqual (Deref counter) val
getLoopEndExpr (LoopEndCondition ex) _ = ex

replaceNil :: Expr -> Expr -> Expr
replaceNil srcExpr r = replaceExprExpr srcExpr (ExprReplace Nil r)

stmtExprs :: Statement -> [Expr]
stmtExprs (Assignment a b) = [a, b]
stmtExprs (Send a b) = [a, b]
stmtExprs (Exchange a b) = [a, b]
stmtExprs (Predicate cond thenSts elseSts) =
  [cond]
    ++ concatMap stmtExprs thenSts
    ++ concatMap stmtExprs elseSts
stmtExprs st@(LoopSimple {}) = stmtExprs (desugarStmt st)
stmtExprs (LoopCommon initSt stepSt endExpr cntExpr _ _) =
  stmtExprs initSt ++ stmtExprs stepSt ++ [endExpr] ++ [cntExpr]
stmtExprs (BuiltinProc _ exs) = exs
stmtExprs (SubprogramCall _ exs _) = exs
stmtExprs _ = []

exprVars :: Expr -> [String]
exprVars (Var name) = [name]
exprVars (BinOpApp _ a b) = exprVars a ++ exprVars b
exprVars (Deref a) = exprVars a
exprVars (MulDeref a b) = exprVars a ++ exprVars b
exprVars _ = []

-- BinOpReplace
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
replaceOpStmt (LoopCommon initSt stepSt endExpr cntExpr a b) r =
  LoopCommon
    (replaceOpStmt initSt r)
    (replaceOpStmt stepSt r)
    (replaceOpExpr endExpr r)
    (replaceOpExpr cntExpr r)
    a
    b
replaceOpStmt (BuiltinProc nm args) r = BuiltinProc nm (map (`replaceOpExpr` r) args)
replaceOpStmt (SubprogramCall nm args e) r = SubprogramCall nm (map (`replaceOpExpr` r) args) e
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

-- ExprReplace
replaceExprStmt :: Statement -> Replacement -> Statement
replaceExprStmt (Assignment a b) r = Assignment (replaceExprExpr a r) (replaceExprExpr b r)
replaceExprStmt (Send a b) r = Send (replaceExprExpr a r) (replaceExprExpr b r)
replaceExprStmt (Exchange a b) r = Exchange (replaceExprExpr a r) (replaceExprExpr b r)
replaceExprStmt (Predicate cond thenSts elseSts) r =
  Predicate
    (replaceExprExpr cond r)
    (map (`replaceExprStmt` r) thenSts)
    (map (`replaceExprStmt` r) elseSts)
replaceExprStmt st@(LoopSimple {}) r = replaceExprStmt (desugarStmt st) r
replaceExprStmt (LoopCommon initSt stepSt endExpr cntExpr a b) r =
  LoopCommon
    (replaceExprStmt initSt r)
    (replaceExprStmt stepSt r)
    (replaceExprExpr endExpr r)
    (replaceExprExpr cntExpr r)
    a
    b
replaceExprStmt (BuiltinProc nm args) r = BuiltinProc nm (map (`replaceExprExpr` r) args)
replaceExprStmt (SubprogramCall nm args e) r = SubprogramCall nm (map (`replaceExprExpr` r) args) e
replaceExprStmt st _ = st

replaceExprExpr :: Expr -> Replacement -> Expr
replaceExprExpr ex r@(ExprReplace exSrc exDst)
  | ex == exSrc = exDst
  | otherwise = case ex of
      BinOpApp op a b -> BinOpApp op (replaceExprExpr a r) (replaceExprExpr b r)
      Deref a -> Deref $ replaceExprExpr a r
      MulDeref a b -> MulDeref (replaceExprExpr a r) (replaceExprExpr b r)
      Negate a -> Negate $ replaceExprExpr a r
      BuiltinFn nm args -> BuiltinFn nm (map (`replaceExprExpr` r) args)
      _ -> ex
replaceExprExpr ex _ = ex

-- StmtReplace
replaceStmtStmt :: Statement -> Replacement -> Statement
replaceStmtStmt st r@(StmtReplace stSrc stDst)
  | st == stSrc = stDst
  | otherwise = case st of
      Predicate cond thenSts elseSts ->
        Predicate
          cond
          (map (`replaceStmtStmt` r) thenSts)
          (map (`replaceStmtStmt` r) elseSts)
      st1@(LoopSimple {}) -> replaceStmtStmt (desugarStmt st1) r
      LoopCommon initSt stepSt endExpr cntExpr a b ->
        LoopCommon
          (replaceStmtStmt initSt r)
          (replaceStmtStmt stepSt r)
          endExpr
          cntExpr
          a
          b
      st1 -> st1
replaceStmtStmt st _ = st