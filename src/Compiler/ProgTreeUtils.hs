module Compiler.ProgTreeUtils where

import Parser.AST

numerateLines :: [ProgLine] -> [ProgLine]
numerateLines pgLines = zipWith (\pl i -> pl {lineNum = i}) pgLines [0 :: Int ..]

-- Util Operators
(<+>) :: Expr -> Expr -> Expr
(<+>) = BinOpApp Add

(|<=|) :: Expr -> Expr -> Expr
(|<=|) = BinOpApp LessEqual

(|=>) :: Expr -> Expr -> Statement
(|=>) = Send

df :: Expr -> Expr
df = UnOpApp Deref

-- Predicate helpers
isFnHead :: ProgLine -> Bool
isFnHead (ProgLine {stmts = (Send Nil (Var _) : _)}) = True
isFnHead (ProgLine {stmts = (ArgReplace Nil (Var _) : _)}) = True
isFnHead _ = False

isFnEnd :: ProgLine -> Bool
isFnEnd (ProgLine {stmts = [Ret]}) = True
isFnEnd _ = False

getFnName :: ProgLine -> String
getFnName (ProgLine {labels = (name : _)}) = name
getFnName _ = undefined

-- Loops
getLoopRange :: Statement -> (Statement, Statement, Expr)
getLoopRange (Loop initVal step end counter _ _) =
  ( initVal |=> counter,
    getLoopStepStmt step counter,
    getLoopEndExpr end counter
  )
getLoopRange _ = undefined

getLoopStepStmt :: LoopStep -> Expr -> Statement
getLoopStepStmt (LoopStepValue stepVal) counter = (df counter <+> stepVal) |=> counter
getLoopStepStmt (LoopStepExpr stepExpr) counter = replaceNil stepExpr (df counter) |=> counter

getLoopEndExpr :: LoopEnd -> Expr -> Expr
getLoopEndExpr (LoopEndValue val) counter = df counter |<=| val
getLoopEndExpr (LoopEndCondition ex) _ = ex

loopStepExpr :: LoopStep -> Expr
loopStepExpr (LoopStepValue val) = val
loopStepExpr (LoopStepExpr ex) = ex

loopEndExpr :: LoopEnd -> Expr
loopEndExpr (LoopEndValue val) = val
loopEndExpr (LoopEndCondition ex) = ex

-- Vars
stmtExprs :: Statement -> [Expr]
stmtExprs (Assign a b) = [a, b]
stmtExprs (ArgReplace a b) = [a, b]
stmtExprs (Send a b) = [a, b]
stmtExprs (Exchange a b) = [a, b]
stmtExprs (Predicate cond thenSts elseSts) =
  [cond]
    ++ concatMap stmtExprs thenSts
    ++ concatMap stmtExprs elseSts
stmtExprs (Loop initVal step end counter _ _) =
  [initVal, loopStepExpr step, loopEndExpr end, counter]
stmtExprs (BuiltinProc _ exs) = exs
stmtExprs (SubprogramCall _ exs _) = exs
stmtExprs _ = []

exprVars :: Expr -> [String]
exprVars (Var name) = [name]
exprVars (BinOpApp _ a b) = exprVars a ++ exprVars b
exprVars (UnOpApp _ a) = exprVars a
exprVars _ = []

-- Replace
replaceNil :: Expr -> Expr -> Expr
replaceNil srcExpr r = replaceExprExpr srcExpr (ExprReplace Nil r)

-- BinOpReplace
replaceOpStmt :: Statement -> Replacement -> Statement
replaceOpStmt (Assign a b) r = Assign (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpStmt (Send a b) r = Send (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpStmt (Exchange a b) r = Exchange (replaceOpExpr a r) (replaceOpExpr b r)
replaceOpStmt (Predicate cond thenSts elseSts) r =
  Predicate
    (replaceOpExpr cond r)
    (map (`replaceOpStmt` r) thenSts)
    (map (`replaceOpStmt` r) elseSts)
replaceOpStmt (Loop initVal step end cntExpr a b) r =
  Loop
    (replaceOpExpr initVal r)
    (replaceOpLoopStep step r)
    (replaceOpLoopEnd end r)
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
replaceOpExpr (UnOpApp op a) r = UnOpApp op $ replaceOpExpr a r
replaceOpExpr (BuiltinFn nm args) r = BuiltinFn nm (map (`replaceOpExpr` r) args)
replaceOpExpr ex _ = ex

replaceOpLoopStep :: LoopStep -> Replacement -> LoopStep
replaceOpLoopStep (LoopStepValue val) r = LoopStepValue $ replaceOpExpr val r
replaceOpLoopStep (LoopStepExpr ex) r = LoopStepExpr $ replaceOpExpr ex r

replaceOpLoopEnd :: LoopEnd -> Replacement -> LoopEnd
replaceOpLoopEnd (LoopEndValue val) r = LoopEndValue $ replaceOpExpr val r
replaceOpLoopEnd (LoopEndCondition ex) r = LoopEndCondition $ replaceOpExpr ex r

-- ExprReplace
replaceExprStmt :: Statement -> Replacement -> Statement
replaceExprStmt (Assign a b) r = Assign (replaceExprExpr a r) (replaceExprExpr b r)
replaceExprStmt (Send a b) r = Send (replaceExprExpr a r) (replaceExprExpr b r)
replaceExprStmt (Exchange a b) r = Exchange (replaceExprExpr a r) (replaceExprExpr b r)
replaceExprStmt (Predicate cond thenSts elseSts) r =
  Predicate
    (replaceExprExpr cond r)
    (map (`replaceExprStmt` r) thenSts)
    (map (`replaceExprStmt` r) elseSts)
replaceExprStmt (Loop initVal step end cntExpr a b) r =
  Loop
    (replaceExprExpr initVal r)
    (replaceExprLoopStep step r)
    (replaceExprLoopEnd end r)
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
      UnOpApp op a -> UnOpApp op $ replaceExprExpr a r
      BuiltinFn nm args -> BuiltinFn nm (map (`replaceExprExpr` r) args)
      _ -> ex
replaceExprExpr ex _ = ex

replaceExprLoopStep :: LoopStep -> Replacement -> LoopStep
replaceExprLoopStep (LoopStepValue val) r = LoopStepValue $ replaceExprExpr val r
replaceExprLoopStep (LoopStepExpr ex) r = LoopStepExpr $ replaceExprExpr ex r

replaceExprLoopEnd :: LoopEnd -> Replacement -> LoopEnd
replaceExprLoopEnd (LoopEndValue val) r = LoopEndValue $ replaceExprExpr val r
replaceExprLoopEnd (LoopEndCondition ex) r = LoopEndCondition $ replaceExprExpr ex r

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
      st1 -> st1
replaceStmtStmt st _ = st