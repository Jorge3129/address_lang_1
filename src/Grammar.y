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
    Ret { TokenKeyword "Ret"}
    P { TokenKeyword "P"}
    L { TokenKeyword "L"}
    Pg { TokenKeyword "Pg"}
    not { TokenKeyword "not"}
    builtin { TokenBuiltin $$ }
    Nil { TokenKeyword "Nil" }

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

progLine : lineLabels stmts { ProgLine $1 $2 }

lineLabels : lineLabels lineLabel          { $1 ++ [$2] }
      | lineLabel			{ [$1] }
      | {- empty -}           { [] }

lineLabel : '@' var "..." { $2 }

stmts : stmts stmtSep stmt        { $1 ++ [$3] }
      | stmts stmtSep             { $1 }
      | stmt			{ [$1] }
      | {- empty -}		{ [] }

stmtSep : ';' { () }
      | ','   { () }

stmt : '!' { Stop }
    | builtin Exp { BuiltinFunc $1 [$2] }
    | subprogCallSt { $1 }
    | predicateSt { $1 }
    | loopSt { $1}
    | assignSt { $1 }
    | sendSt { $1 }
    | Ret { Ret }
    | var { Jump $1 }
--     | Exp { ExpSt $1 }

predicateSt : P '{' Exp '}' stmts '|' stmts { Predicate $3 $5 $7}

loopSt : L '{' Exp '(' Exp ')' Exp "=>" Exp '}' loopScope loopNext { LoopSimple $3 $5 $7 $9 $11 $12}

loopScope : var { Just $1 }
      | {- empty -}  { Nothing }

loopNext : ',' var { Just $2 }
      | {- empty -}  { Nothing }

assignLhs : var { Var $1 }
      | "'" Exp %prec DEREF { Deref $2 }
      | "`" Exp "`" Exp %prec DEREF { MulDeref $2 $4 }

assignSt : assignLhs '=' Exp { Assignment $1 $3 }

sendSt : Exp "=>" Exp { Send $1 $3 }

subprogCallSt : Pg var '{' exprs '}' subProgNextLabel { SubprogramCall $2 $4 $6 }

subProgNextLabel : var  { Just $1 }
      | {- empty -}     { Nothing }

exprs : exprs ',' Exp   { $1 ++ [$3] }
      | Exp			{ [$1] }
      | {- empty -}	{ [] }

Exp :  Exp "==" Exp          { BinOpApp Equal $1 $3 }
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
    | Nil                    { Nil }


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
  | Predicate Expr [Statement] [Statement]
  --           start step end counter scope-lbl   next-lbl
  | LoopSimple Expr Expr Expr Expr (Maybe String) (Maybe String)
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
