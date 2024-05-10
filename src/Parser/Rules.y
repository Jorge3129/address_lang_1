{
module Parser.Rules where

import Parser.AST
import Lexer.Rules
import Value.Core
}

%name parseProg
%tokentype { Token }
%error { parseError }

%token
    intConst      { Token _ _ (TInt $$) }
    floatConst    { Token _ _ (TFloat $$) }
    identifier    { Token _ _ (TIdentifier $$) }
    labelDecl     { Token _ _ (TLabel $$) }
    builtinProc   { Token _ _ (TProc $$) }
    builtinFn     { Token _ _ (TFn $$) }
    eol           { Token _ _ TNewLine }
    "Ret"         { Token _ _ (TKeyword "Ret") }
    "P"           { Token _ _ (TKeyword "P") }
    "L"           { Token _ _ (TKeyword "L") }
    "R"           { Token _ _ (TKeyword "R") }
    "Pg"          { Token _ _ (TKeyword "Pg") }
    "not"         { Token _ _ (TKeyword "not") }
    "and"         { Token _ _ (TKeyword "and") }
    "or"          { Token _ _ (TKeyword "or") }
    "Nil"         { Token _ _ (TKeyword "Nil") }
    "&"           { Token _ _ TAnd }
    "|"           { Token _ _ TBar }
    ";"           { Token _ _ TSemi }
    ","           { Token _ _ TComma }
    "+"           { Token _ _ TPlus }
    "<+>"         { Token _ _ TCirclePlus }
    "-"           { Token _ _ TMinus }
    "*"           { Token _ _ TTimes }
    "/"           { Token _ _ TDiv }
    "%"           { Token _ _ TMod }
    "("           { Token _ _ TLPar }
    ")"           { Token _ _ TRPar }
    "{"           { Token _ _ TLCurly }
    "}"           { Token _ _ TRCurly }
    "["           { Token _ _ TLBrack }
    "]"           { Token _ _ TRBrack }
    "!"           { Token _ _ TStop }
    "="           { Token _ _ TAssign }
    "=="          { Token _ _ TEq }
    "/="          { Token _ _ TNeq }
    ">"           { Token _ _ TGt }
    ">="          { Token _ _ TGe }
    "<"           { Token _ _ TLt }
    "<="          { Token _ _ TLe }
    "'"           { Token _ _ TSQuote }
    "`"           { Token _ _ TBackTick }
    "m`"          { Token _ _ TMBackTick }
    "->"          { Token _ _ TSArrow }
    "=>"          { Token _ _ TDArrow }
    "<=>"         { Token _ _ TExchange }

%left "or"
%left "and"
%nonassoc "==" "/="
%nonassoc ">" "<" "<=" ">="
%left "+" "-" "<+>"
%left "*" "/" "%"
%left NEG
%left DEREF

%%

program : progLines                             { Program $1 }

progLines : progLines eol progLine              { $1 ++ [$3] }
      | progLine                                { [$1] }

progLine : lineLabels stmts                     { ProgLine $1 $2 0 }

lineLabels : lineLabels labelDecl               { $1 ++ [$2] }
      | labelDecl			                  { [$1] }
      | {- empty -}                             { [] }

-- Statements
stmts : stmts stmtSep stmt                      { $1 ++ [$3] }
      | stmts stmtSep                           { $1 }
      | stmt			                  { [$1] }
      | {- empty -}		                  { [] }

stmtSep : ";"                                   { () }
      | ","                                     { () }

stmt : "!"                                      { Stop }
    | builtinProc Exp                           { BuiltinProc $1 [$2] }
    | subprogCallSt                             { $1 }
    | exchangeSt                                { $1 }
    | predicateSt                               { $1 }
    | loopStSimple                              { $1 }
    | assignSt                                  { $1 }
    | sendSt                                    { $1 }
    | "Ret"                                     { Ret }
    | identifier                                { Jump $1 }
    | replaceSt                                 { $1 }

-- Assign
assignSt : assignLhs "=" Exp                    { Assign $1 $3 }

assignLhs : identifier                          { Var $1 }
      | "'" Exp %prec DEREF                     { Deref $2 }
      | "`" Exp "`" Exp %prec DEREF             { MulDeref $2 $4 }

sendSt : Exp "=>" Exp                           { Send $1 $3 }

exchangeSt : Exp "<=>" Exp                      { Exchange $1 $3 }

-- Predicate
predicateSt : "P" "{" Exp "}" stmts "|" stmts   { Predicate $3 $5 $7 }

-- Loops
loopStSimple : "L" "{" Exp loopStep loopEnd "=>" Exp "}" loopScope loopNext  { LoopSimple $3 $4 $5 $7 $9 $10 }

loopStep : "(" Exp ")"                          { LoopStepValue $2 }
      | "," Exp ","                             { LoopStepExpr $2 }

loopEnd : "P" "{" Exp "}"                       { LoopEndCondition $3 }
      | Exp                                     { LoopEndValue $1 }

loopScope : identifier                          { Just $1 }
      | {- empty -}                             { Nothing }

loopNext : "," identifier                       { Just $2 }
      | {- empty -}                             { Nothing }

-- Subprogram call
subprogCallSt : "Pg" callValue "{" exprs "}" callNextLabel { SubprogramCall $2 $4 $6 }

callValue : "[" Exp "]"                         { $2 }
      | identifier                              { LabelRef $1 False }

callNextLabel : identifier                      { Just $1 }
      | {- empty -}                             { Nothing }

-- Replace
replaceSt : "R" "{" replacements "}" identifier "," identifier  { Replace $3 $5 $7 }

replacements : replacements ";" replacement     { $1 ++ [$3] }
      | replacement			            { [$1] }
      | {- empty -}	                        { [] }

replacement : Exp "->" Exp                      { ExprReplace $1 $3 }
      | stmt "->" stmt                          { StmtReplace $1 $3 }
      | binOp "->" binOp                        { BinOpReplace $1 $3 }

binOp : "+"                                     { Add }
    | "-"                                       { Sub }
    | "*"                                       { Mul }
    | "/"                                       { Div }
    | "%"                                       { Mod }
    | "<+>"                                     { PtrAdd }

-- Expressions
Exp :  Exp "or" Exp                             { BinOpApp Or $1 $3 }
    | Exp "and" Exp                             { BinOpApp And $1 $3 }
    | Exp "==" Exp                              { BinOpApp Equal $1 $3 }
    | Exp "/=" Exp                              { BinOpApp NotEqual $1 $3 }
    | Exp "<=" Exp                              { BinOpApp LessEqual $1 $3 }
    | Exp ">=" Exp                              { BinOpApp GreaterEqual $1 $3 }
    | Exp "<" Exp                               { BinOpApp Less $1 $3 }
    | Exp ">" Exp                               { BinOpApp Greater $1 $3 }
    | Exp "+" Exp                               { BinOpApp Add $1 $3 }
    | Exp "-" Exp                               { BinOpApp Sub $1 $3 }
    | Exp "*" Exp                               { BinOpApp Mul $1 $3 }
    | Exp "/" Exp                               { BinOpApp Div $1 $3 }
    | Exp "%" Exp                               { BinOpApp Mod $1 $3 }
    | Exp "<+>" Exp                             { BinOpApp PtrAdd $1 $3 }
    | "-" Exp %prec NEG                         { Negate $2 }
    | "'" Exp %prec DEREF                       { Deref $2 }
    | "`" Exp "`" Exp %prec DEREF               { MulDeref $2 $4 }
    | "m`" Exp "`" Exp %prec DEREF              { MinDeref $2 $4 }
    |  "&" identifier                           { LabelRef $2 True }
    | intConst                                  { Lit (IntVal $1) }
    | floatConst                                { Lit (DoubleVal $1) }
    | identifier                                { Var $1 }
    | "Nil"                                     { Nil }
    | builtinFnExp                              { $1 }
    | listExp                                   { $1 }
    | "(" Exp ")"                               { $2 }

exprs : exprs "," Exp                           { $1 ++ [$3] }
      | Exp			                        { [$1] }
      | {- empty -}	                        { [] }

spaceExprs : spaceExprs Exp                     { $1 ++ [$2] }
      | Exp			                        { [$1] }
      | {- empty -}	                        { [] }

listExp : "[" exprs "]"                         { BuiltinFn "constrList" $2 }

builtinFnExp : builtinFn spaceExprs             { BuiltinFn $1 $2 }

{

parseError :: [Token] -> a
parseError ((Token (AlexPn _ ln col) tokStr _) : xs) =
  error $
    "Parse error: unexpected token "
      ++ show tokStr
      ++ " at line "
      ++ show ln
      ++ ", column "
      ++ show col
parseError [] = error $ "Parse error at the end of file"

}
