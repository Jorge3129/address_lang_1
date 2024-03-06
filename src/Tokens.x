{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  $digit+                       { \s -> TokenInt (read s) }
  \=                            { \s -> TokenAssign }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token = 
    TokenNewLine
  | TokenSlash
  | TokenStar
  | TokenLeftParen
  | TokenRightParen
  | TokenLeftCurly
  | TokenRightCurly
  | TokenHorizontal
  | TokenBang
  | TokenBangEqual
  | TokenEqual
  | TokenEqualEqual
  | TokenGreater
  | TokenMinusGreater
  | TokenGreaterEqual
  | TokenLess
  | TokenLessEqual
  | TokenSingleQuote
  | TokenEqualGreater
  | TokenDots3
  | TokenLessEqualGreater
  | TokenIdentifier String
  | TokenKeyword String
  | TokenNumber Int
  | TokenError
  deriving (Show, Eq)


scanTokens = alexScanTokens

}
