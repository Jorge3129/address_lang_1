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
@id = $letter ($letter | $digit)*

tokens :-
  $white_no_nl+ ;
  "//".*        ;
  @eol_pattern      { \_ -> TokenNewLine }
  "@"      { \_ -> TokenAt }
  "|"      { \_ -> TokenVerticalBar }
  ";"       { \_ -> TokenSemi }
  ","       { \_ -> TokenComma }
  "+"       { \_ -> TokenPlus }
  "-"       { \_ -> TokenMinus }
  "*"       { \_ -> TokenStar }
  "/"       { \_ -> TokenSlash }
  "("       { \_ -> TokenLeftParen }
  ")"       { \_ -> TokenRightParen }
  "{"       { \_ -> TokenLeftCurly }
  "}"       { \_ -> TokenRightCurly }
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
  "->"      { \_ -> TokenMinusGreater }
  "=>"      { \_ -> TokenEqualGreater }
  "<=>"     { \_ -> TokenLessEqualGreater }
  "@" @id $white_no_nl* "..." { \s -> TokenLabel (takeWhile isAlphaNum (tail s)) }
  @id { \s -> 
      let keywords = ["P", "L", "Pg", "Nil", "Ret", "Cj", "not"]
          builtins = ["print", "printList", "printRefs"]
      in if elem s keywords then TokenKeyword s
          else if elem s builtins then TokenBuiltin s 
          else TokenIdentifier s 
    }
  $digit+ { \s -> TokenNumber (read s) }
  _         { \_ -> TokenError }

{

data Token = 
    TokenNewLine
  | TokenAt
  | TokenVerticalBar
  | TokenSemi
  | TokenComma
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenSlash
  | TokenLeftParen
  | TokenRightParen
  | TokenLeftCurly
  | TokenRightCurly
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
  | TokenEqualGreater
  | TokenLessEqualGreater
  | TokenLabel String
  | TokenIdentifier String
  | TokenKeyword String
  | TokenBuiltin String
  | TokenNumber Int
  | TokenError
  deriving (Show, Eq)


scanTokens = alexScanTokens

}
