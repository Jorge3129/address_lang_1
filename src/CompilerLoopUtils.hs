module CompilerLoopUtils where

import Grammar

loopStepValToStmt :: Expr -> Expr -> Statement
loopStepValToStmt step counter = Send (BinOpApp Add (Deref counter) step) counter

loopStepExprToStmt :: Expr -> Expr -> Statement
loopStepExprToStmt stepExpr counter = Send (replaceNil stepExpr (Deref counter)) counter

desugarStmt :: Statement -> Statement
desugarStmt (LoopSimple initVal stepVal end counter scope next) =
  LoopCommon
    (Send initVal counter)
    (loopStepValToStmt stepVal counter)
    (getLoopEndExpr end counter)
    counter
    scope
    next
desugarStmt (LoopComplex initVal stepExpr end counter scope next) =
  LoopCommon
    (Send initVal counter)
    (loopStepExprToStmt stepExpr counter)
    (getLoopEndExpr end counter)
    counter
    scope
    next
desugarStmt s = s

getLoopEndExpr :: LoopEnd -> Expr -> Expr
getLoopEndExpr (LoopEndValue val) counter = BinOpApp LessEqual (Deref counter) val
getLoopEndExpr (LoopEndCondition ex) _ = ex

replaceNil :: Expr -> Expr -> Expr
replaceNil Nil newEx = newEx
replaceNil (BinOpApp op a b) newEx = BinOpApp op (replaceNil a newEx) (replaceNil b newEx)
replaceNil (Deref a) newEx = Deref (replaceNil a newEx)
replaceNil (MulDeref a b) newEx = MulDeref (replaceNil a newEx) (replaceNil b newEx)
replaceNil ex _ = ex