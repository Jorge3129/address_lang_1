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
    not { TokenKeyword "not"}
    builtin { TokenBuiltin $$ }
    '@' { TokenAt }
    '|' { TokenVerticalBar }
    ';' { TokenSemi }
    ',' { TokenComma }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenStar }
    '/' { TokenSlash }
    '(' { TokenLeftParen }
    ')' { TokenRightParen }
    '{' { TokenLeftCurly }
    '}' { TokenRightCurly }
    '!' { TokenBang }
    '=' { TokenEqual }
    "==" { TokenEqualEqual }
    "/=" { TokenSlashEqual }
    '>' { TokenGreater }
    ">=" { TokenGreaterEqual }
    '<' { TokenLess }
    "<=" { TokenLessEqual }
    "'" { TokenSingleQuote }
    "`" { TokenBackTick }
    "=>" { TokenEqualGreater }
    "..." { TokenEllipsis }

%nonassoc "==" "/="
%nonassoc '>' '<' "<=" ">="
%left '+' '-'
%left '*' '/'
%left NEG
%left DEREF

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
    | builtin Exp { BuiltinFunc $1 [$2] }
    | ret { Ret }
    | Exp { ExpSt $1 }
    | var '=' Exp { Assignment (Var $1) $3 }

Exp :  Exp "==" Exp           { BinOpApp Equal $1 $3 }
    | Exp "/=" Exp           { BinOpApp NotEqual $1 $3 }
    | Exp "<=" Exp           { BinOpApp LessEqual $1 $3 }
    | Exp ">=" Exp           { BinOpApp GreaterEqual $1 $3 }
    | Exp '<' Exp            { BinOpApp Less $1 $3 }
    | Exp '>' Exp            { BinOpApp Greater $1 $3 }
    | Exp '+' Exp            { BinOpApp Add $1 $3 }
    | Exp '-' Exp            { BinOpApp Sub $1 $3 }
    | Exp '*' Exp            { BinOpApp Mul $1 $3 }
    | Exp '/' Exp            { BinOpApp Div $1 $3 }
    | '(' Exp ')'            { $2 }
    -- | '-' Exp %prec NEG      { Negate $2 }
    | "'" Exp %prec DEREF    { Deref $2 }
    | "`" Exp "`" Exp %prec DEREF    { MulDeref $2 $4 }
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
