{
module Tokens where

import Data.Char (isAlphaNum)
}

%wrapper "basic"

$letter    = [a-zA-Z]
$digit     = 0-9
$white_no_nl = [\ \t]
$lf = \n
$cr = \r
@eol_pattern = $lf | $cr $lf | $cr $lf
@id = $letter ($letter | $digit | _)*
@decimal = $digit+
@kw = (P|L|R|Pg|Nil|Ret|Cj|not|and|or)
@builtinProc = (print|printList|printRefs)
@builtinFn = (getRefs|alloc|ptr|id|mulalloc)

tokens :-
  $white_no_nl+ ;
  "//".*        ;
  @eol_pattern      { \_ -> TokenNewLine }
  "@"       { \_ -> TokenAt }
  "|"       { \_ -> TokenVerticalBar }
  ";"       { \_ -> TokenSemi }
  ","       { \_ -> TokenComma }
  "+"       { \_ -> TokenPlus }
  "<+>"     { \_ -> TokenLessPlusGreater }
  "-"       { \_ -> TokenMinus }
  "*"       { \_ -> TokenStar }
  "/"       { \_ -> TokenSlash }
  "%"       { \_ -> TokenPercent }
  "("       { \_ -> TokenLeftParen }
  ")"       { \_ -> TokenRightParen }
  "{"       { \_ -> TokenLeftCurly }
  "}"       { \_ -> TokenRightCurly }
  "["       { \_ -> TokenLeftBracket }
  "]"       { \_ -> TokenRightBracket }
  "!"       { \_ -> TokenBang }
  "="       { \_ -> TokenEqual }
  "=="      { \_ -> TokenEqualEqual }
  "/="      { \_ -> TokenSlashEqual }
  ">"       { \_ -> TokenGreater }
  ">="      { \_ -> TokenGreaterEqual }
  "<"       { \_ -> TokenLess }
  "<="      { \_ -> TokenLessEqual }
  "'"       { \_ -> TokenSingleQuote }
  "`"       { \_ -> TokenBackTick }
  "m`"      { \_ -> TokenMBackTick }
  "->"      { \_ -> TokenMinusGreater }
  "=>"      { \_ -> TokenEqualGreater }
  "<=>"     { \_ -> TokenLessEqualGreater }
  "@" @id $white_no_nl* "..." { \s -> TokenLabel (takeWhile (\c -> isAlphaNum c || c `elem` "_") (tail s)) }
  @kw                   { \s -> TokenKeyword s}
  @builtinProc          { \s -> TokenBuiltinProc s}
  @builtinFn            { \s -> TokenBuiltinFn s}
  @id                   { \s -> TokenIdentifier s }
  @decimal              { \s -> TokenInt (read s) }
  @decimal \. @decimal  { \s -> TokenFloat (read s) }
  _                     { \_ -> TokenError }

{

data Token = 
    TokenNewLine
  | TokenAt
  | TokenVerticalBar
  | TokenSemi
  | TokenComma
  | TokenPlus
  | TokenLessPlusGreater
  | TokenMinus
  | TokenStar
  | TokenSlash
  | TokenPercent
  | TokenLeftParen
  | TokenRightParen
  | TokenLeftCurly
  | TokenRightCurly
  | TokenLeftBracket
  | TokenRightBracket
  | TokenHorizontal
  | TokenBang
  | TokenEqual
  | TokenEqualEqual
  | TokenSlashEqual
  | TokenGreater
  | TokenMinusGreater
  | TokenGreaterEqual
  | TokenLess
  | TokenLessEqual
  | TokenSingleQuote
  | TokenBackTick
  | TokenMBackTick
  | TokenEqualGreater
  | TokenLessEqualGreater
  | TokenLabel String
  | TokenIdentifier String
  | TokenKeyword String
  | TokenBuiltinProc String
  | TokenBuiltinFn String
  | TokenInt Int
  | TokenFloat Double
  | TokenError
  deriving (Show, Eq)


scanTokens = alexScanTokens

}
