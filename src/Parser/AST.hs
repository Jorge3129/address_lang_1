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
  deriving (Eq, Show)

data Expr
  = Lit Value
  | Nil
  | Var String
  | BinOpApp BinOp Expr Expr
  | Negate Expr
  | Deref Expr
  | MulDeref Expr Expr
  | MinDeref Expr Expr
  | BuiltinFn String [Expr]
  | LabelRef String Bool
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
  = Assignment Expr Expr
  | Send Expr Expr
  | Exchange Expr Expr
  | Predicate Expr [Statement] [Statement]
  | --           start step end             counter scope-lbl   next-lbl
    LoopSimple Expr Expr LoopEnd Expr (Maybe String) (Maybe String)
  | LoopComplex Expr Expr LoopEnd Expr (Maybe String) (Maybe String)
  | LoopCommon Statement Statement Expr Expr (Maybe String) (Maybe String)
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