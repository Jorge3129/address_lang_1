module CompilerUtils where

import Grammar

getLoopStepStmt :: Expr -> Expr -> Statement
getLoopStepStmt step counter = Send (BinOpApp Add (Deref counter) step) counter

getLoopEndExpr :: LoopEnd -> Expr -> Expr
getLoopEndExpr (LoopEndValue val) counter = BinOpApp LessEqual (Deref counter) val
getLoopEndExpr (LoopEndCondition ex) _ = ex

replaceNil :: Expr -> Expr -> Expr
replaceNil Nil newEx = newEx
replaceNil (BinOpApp op a b) newEx = BinOpApp op (replaceNil a newEx) (replaceNil b newEx)
replaceNil (Deref a) newEx = Deref (replaceNil a newEx)
replaceNil (MulDeref a b) newEx = MulDeref (replaceNil a newEx) (replaceNil b newEx)
replaceNil ex _ = ex