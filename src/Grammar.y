{
module Grammar where
import Tokens
import Value.Core
}

%name parseProg
%tokentype { TokenInfo }
%error { parseError }

%token
    constInt      { TokenInfo _ _ (TokenInt $$) }
    constFloat    { TokenInfo _ _ (TokenFloat $$) }
    var           { TokenInfo _ _ (TokenIdentifier $$) }
    eol           { TokenInfo _ _ TokenNewLine }
    Ret           { TokenInfo _ _ (TokenKeyword "Ret") }
    P             { TokenInfo _ _ (TokenKeyword "P") }
    L             { TokenInfo _ _ (TokenKeyword "L") }
    R             { TokenInfo _ _ (TokenKeyword "R") }
    Pg            { TokenInfo _ _ (TokenKeyword "Pg") }
    "not"         { TokenInfo _ _ (TokenKeyword "not") }
    "and"         { TokenInfo _ _ (TokenKeyword "and") }
    "or"          { TokenInfo _ _ (TokenKeyword "or") }
    Nil           { TokenInfo _ _ (TokenKeyword "Nil") }
    builtinProc   { TokenInfo _ _ (TokenBuiltinProc $$) }
    builtinFn     { TokenInfo _ _ (TokenBuiltinFn $$) }
    lab           { TokenInfo _ _ (TokenLabel $$) }
    '&'           { TokenInfo _ _ TokenAnd }
    '|'           { TokenInfo _ _ TokenVerticalBar }
    ';'           { TokenInfo _ _ TokenSemi }
    ','           { TokenInfo _ _ TokenComma }
    '+'           { TokenInfo _ _ TokenPlus }
    '<+>'         { TokenInfo _ _ TokenLessPlusGreater }
    '-'           { TokenInfo _ _ TokenMinus }
    '*'           { TokenInfo _ _ TokenStar }
    '/'           { TokenInfo _ _ TokenSlash }
    '%'           { TokenInfo _ _ TokenPercent }
    '('           { TokenInfo _ _ TokenLeftParen }
    ')'           { TokenInfo _ _ TokenRightParen }
    '{'           { TokenInfo _ _ TokenLeftCurly }
    '}'           { TokenInfo _ _ TokenRightCurly }
    '['           { TokenInfo _ _ TokenLeftBracket }
    ']'           { TokenInfo _ _ TokenRightBracket }
    '!'           { TokenInfo _ _ TokenBang }
    '='           { TokenInfo _ _ TokenEqual }
    "=="          { TokenInfo _ _ TokenEqualEqual }
    "/="          { TokenInfo _ _ TokenSlashEqual }
    '>'           { TokenInfo _ _ TokenGreater }
    ">="          { TokenInfo _ _ TokenGreaterEqual }
    '<'           { TokenInfo _ _ TokenLess }
    "<="          { TokenInfo _ _ TokenLessEqual }
    "'"           { TokenInfo _ _ TokenSingleQuote }
    "`"           { TokenInfo _ _ TokenBackTick }
    "m`"          { TokenInfo _ _ TokenMBackTick }
    "->"          { TokenInfo _ _ TokenMinusGreater }
    "=>"          { TokenInfo _ _ TokenEqualGreater }
    "<=>"         { TokenInfo _ _ TokenLessEqualGreater }

%left "or"
%left "and"
%nonassoc "==" "/="
%nonassoc '>' '<' "<=" ">="
%left '+' '-' '<+>'
%left '*' '/' '%'
%left NEG
%left DEREF

%%

program : progLines                             { Program $1 }

progLines : progLines eol progLine              { $1 ++ [$3] }
      | progLines eol                           { $1 }
      | progLine                                { [$1] }
      | {- empty -}		                  { [] }

progLine : lineLabels stmts                     { ProgLine $1 $2 0 }

lineLabels : lineLabels lineLabel               { $1 ++ [$2] }
      | lineLabel			                  { [$1] }
      | {- empty -}                             { [] }

lineLabel : lab                                 { $1 }

stmts : stmts stmtSep stmt                      { $1 ++ [$3] }
      | stmts stmtSep                           { $1 }
      | stmt			                  { [$1] }
      | {- empty -}		                  { [] }

stmtSep : ';'                                   { () }
      | ','                                     { () }

stmt : '!'                                      { Stop }
    | builtinProc Exp                           { BuiltinProc $1 [$2] }
    | subprogCallSt                             { $1 }
    | exchangeSt                                { $1 }
    | predicateSt                               { $1 }
    | loopStSimple                              { $1 }
    | loopStComplex                             { $1 }
    | assignSt                                  { $1 }
    | sendSt                                    { $1 }
    | Ret                                       { Ret }
    | var                                       { Jump $1 }
    | replaceSt                                 { $1 }

replaceSt : R '{' replacements '}' var ',' var  { Replace $3 $5 $7 }

replacements : replacements ';' replacement     { $1 ++ [$3] }
      | replacement			            { [$1] }
      | {- empty -}	                        { [] }

replacement : Exp "->" Exp                      { ExprReplace $1 $3 }
      | stmt "->" stmt                          { StmtReplace $1 $3 }
      | binOp "->" binOp                        { BinOpReplace $1 $3 }

binOp : '+'                                     { Add }
    | '-'                                       { Sub }
    | '*'                                       { Mul }
    | '/'                                       { Div }
    | '%'                                       { Mod }
    | '<+>'                                     { PtrAdd }

exchangeSt : Exp "<=>" Exp                      { Exchange $1 $3 }

predicateSt : P '{' Exp '}' stmts '|' stmts     { Predicate $3 $5 $7 }

loopStSimple : L '{' Exp '(' Exp ')' loopEnd "=>" Exp '}' loopScope loopNext  { LoopSimple $3 $5 $7 $9 $11 $12 }

loopStComplex : L '{' Exp ',' Exp ',' loopEnd "=>" Exp '}' loopScope loopNext { LoopComplex $3 $5 $7 $9 $11 $12 }

loopEnd : P '{' Exp '}'                         { LoopEndCondition $3 }
      | Exp                                     { LoopEndValue $1 }

loopScope : var                                 { Just $1 }
      | {- empty -}                             { Nothing }

loopNext : ',' var                              { Just $2 }
      | {- empty -}                             { Nothing }

assignLhs : var                                 { Var $1 }
      | "'" Exp %prec DEREF                     { Deref $2 }
      | "`" Exp "`" Exp %prec DEREF             { MulDeref $2 $4 }

assignSt : assignLhs '=' Exp                    { Assignment $1 $3 }

sendSt : Exp "=>" Exp                           { Send $1 $3 }

subprogCallSt : Pg callValue '{' exprs '}' callNextLabel { SubprogramCall $2 $4 $6 }

callValue : '[' Exp ']'                         { $2 }
      | var                                     { LabelRef $1 False }

callNextLabel : var                             { Just $1 }
      | {- empty -}                             { Nothing }

Exp :  Exp "or" Exp                             { BinOpApp Or $1 $3 }
    | Exp "and" Exp                             { BinOpApp And $1 $3 }
    | Exp "==" Exp                              { BinOpApp Equal $1 $3 }
    | Exp "/=" Exp                              { BinOpApp NotEqual $1 $3 }
    | Exp "<=" Exp                              { BinOpApp LessEqual $1 $3 }
    | Exp ">=" Exp                              { BinOpApp GreaterEqual $1 $3 }
    | Exp '<' Exp                               { BinOpApp Less $1 $3 }
    | Exp '>' Exp                               { BinOpApp Greater $1 $3 }
    | Exp '+' Exp                               { BinOpApp Add $1 $3 }
    | Exp '-' Exp                               { BinOpApp Sub $1 $3 }
    | Exp '*' Exp                               { BinOpApp Mul $1 $3 }
    | Exp '/' Exp                               { BinOpApp Div $1 $3 }
    | Exp '%' Exp                               { BinOpApp Mod $1 $3 }
    | Exp '<+>' Exp                             { BinOpApp PtrAdd $1 $3 }
    | '-' Exp %prec NEG                         { Negate $2 }
    | "'" Exp %prec DEREF                       { Deref $2 }
    | "`" Exp "`" Exp %prec DEREF               { MulDeref $2 $4 }
    | "m`" Exp "`" Exp %prec DEREF              { MinDeref $2 $4 }
    | '&' var                                   { LabelRef $2 True }
    | constInt                                  { Lit (IntVal $1) }
    | constFloat                                { Lit (DoubleVal $1) }
    | var                                       { Var $1 }
    | Nil                                       { Nil }
    | builtinFnExp                              { $1 }
    | listExp                                   { $1 }
    | '(' Exp ')'                               { $2 }


exprs : exprs ',' Exp                           { $1 ++ [$3] }
      | Exp			                        { [$1] }
      | {- empty -}	                        { [] }

spaceExprs : spaceExprs Exp                     { $1 ++ [$2] }
      | Exp			                        { [$1] }
      | {- empty -}	                        { [] }

listExp : '[' exprs ']'                         { BuiltinFn "constrList" $2 }

builtinFnExp : builtinFn spaceExprs             { BuiltinFn $1 $2 }

{

parseError :: [TokenInfo] -> a
parseError ((TokenInfo (AlexPn _ ln col) str _):xs) = 
      error $ "Parse error: unexpected token " ++ show str ++ " at line " ++ show ln ++ ", column " ++ show col
parseError [] = error $ "parse error at the end of file"

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | PtrAdd
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

data LoopEnd = LoopEndValue Expr | LoopEndCondition Expr deriving (Eq, Show)

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
  --           start step end             counter scope-lbl   next-lbl
  | LoopSimple Expr Expr LoopEnd Expr (Maybe String) (Maybe String)
  | LoopComplex Expr Expr LoopEnd Expr (Maybe String) (Maybe String)
  | LoopCommon Statement Statement Expr Expr (Maybe String) (Maybe String)
  | Replace [Replacement] String String
  | SubprogramCall Expr [Expr] (Maybe String)
  | BuiltinProc String [Expr]
  | Jump String
  | CompJump Expr
  | Ret
  | Stop
  deriving (Eq, Show)

data ProgLine = ProgLine {
      labels :: [String],
      stmts :: [Statement],
      lineNum :: Int
} deriving (Eq, Show)

data Program = Program { pLines :: [ProgLine] } deriving (Eq, Show)

}
