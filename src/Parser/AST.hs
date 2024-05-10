module Parser.AST where

import Value.Core

data BinOp
  = Add
  | PtrAdd
  | Sub
  | Mul
  | Div
  | Mod
  | Greater
  | Less
  | Equal
  | NotEqual
  | GreaterEqual
  | LessEqual
  | And
  | Or
  | MulDeref
  | MinDeref
  deriving (Eq, Show)

data UnOp
  = Negate
  | Deref
  | Not
  deriving (Eq, Show)

data Expr
  = Lit Value
  | Nil
  | Var String
  | UnOpApp UnOp Expr
  | BinOpApp BinOp Expr Expr
  | BuiltinFn String [Expr]
  | LabelRef String Bool
  deriving (Eq, Show)

data LoopStep
  = LoopStepValue Expr
  | LoopStepExpr Expr
  deriving (Eq, Show)

data LoopEnd
  = LoopEndValue Expr
  | LoopEndCondition Expr
  deriving (Eq, Show)

data Replacement
  = ExprReplace Expr Expr
  | StmtReplace Statement Statement
  | BinOpReplace BinOp BinOp
  deriving (Eq, Show)

data Statement
  = Assign Expr Expr
  | Send Expr Expr
  | Exchange Expr Expr
  | Predicate Expr [Statement] [Statement]
  | LoopSimple Expr LoopStep LoopEnd Expr (Maybe String) (Maybe String)
  | Replace [Replacement] String String
  | SubprogramCall Expr [Expr] (Maybe String)
  | BuiltinProc String [Expr]
  | Jump String
  | Ret
  | Stop
  deriving (Eq, Show)

data ProgLine = ProgLine
  { labels :: [String],
    stmts :: [Statement],
    lineNum :: Int
  }
  deriving (Eq, Show)

newtype Program = Program {pLines :: [ProgLine]}
  deriving (Eq, Show)