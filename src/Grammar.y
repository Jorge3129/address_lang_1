{
module Grammar where
import Tokens
}

%name parseProg
%tokentype { Token }
%error { parseError }

%token
    int { TokenNumber $$ }
    var { TokenIdentifier $$ }
    eol { TokenNewLine }
    ret { TokenKeyword "Ret"}
    '@' { TokenAt }
    '!' { TokenBang }
    '=' { TokenEqual }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenStar }
    '/' { TokenSlash }
    '(' { TokenLeftParen }
    ')' { TokenRightParen }
    ';' { TokenSemi }
    ',' { TokenComma }
    "..." { TokenEllipsis }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

program : progLines { Program $1 }

progLines : progLines eol progLine  { $1 ++ [$3] }
      | progLines eol { $1 }
      | progLine { [$1] }
      | {- empty -}		{ [] }

progLine : lineLabelsList stmts { ProgLine $1 $2 }

lineLabelsList : '@' lineLabels "..." { $2 }
      | {- empty -} { [] }

lineLabels : lineLabels ',' lineLabel          { $1 ++ [$3] }
      | lineLabel			{ [$1] }

lineLabel : var { $1 }

stmts : stmts ';' stmt          { $1 ++ [$3] }
      | stmts ';'               { $1 }
      | stmt			{ [$1] }
      | {- empty -}		{ [] }

stmt : '!' { Stop }
    | ret { Ret }
    | Exp { ExpSt $1 }
    | var '=' Exp { Assignment (Var $1) $3 }

Exp : Exp '+' Exp            { BinOpApp Add $1 $3 }
    | Exp '-' Exp            { BinOpApp Sub $1 $3 }
    | Exp '*' Exp            { BinOpApp Mul $1 $3 }
    | Exp '/' Exp            { BinOpApp Div $1 $3 }
    | '(' Exp ')'            { $2 }
    -- | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Lit $1 }
    | var                    { Var $1 }


{

parseError :: [Token] -> a
parseError x = error (show x)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Greater
  | Less
  | Equal
  | NotEqual
  | GreaterEqual
  | LessEqual
  deriving (Eq, Show)

data Expr
  = Lit Int
  | Var String
  | BinOpApp BinOp Expr Expr
  | Deref Expr
  | MulDeref Expr Expr
  | Nil
  deriving (Eq, Show)

data Statement
  = Assignment Expr Expr
  | Send Expr Expr
  | Exchange Expr Expr
  | Conditional Expr Statement Statement
  | SubprogramCall String [Expr] (Maybe String)
  | BuiltinFunc String [Expr]
  | Jump String
  | CompJump Expr
  | Ret
  | Stop
  | ExpSt Expr
  deriving (Eq, Show)

data ProgLine = ProgLine {
      labels :: [String],
      stmts :: [Statement]
} deriving (Eq, Show)

data Program = Program { pLines :: [ProgLine] } deriving (Eq, Show)

}
