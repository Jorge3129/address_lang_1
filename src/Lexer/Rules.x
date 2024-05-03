{
module Lexer.Rules where

import Data.Char (isAlphaNum)
}

%wrapper "posn"

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
@builtinFn = (alloc|ptr|id|mulalloc)

tokens :-
  $white_no_nl+ ;
  "//".*        ;
  @eol_pattern      { tok (\_ -> TokenNewLine) }
  "("               { tok (\_ -> TokenLeftParen) }
  ")"               { tok (\_ -> TokenRightParen) }
  "{"               { tok (\_ -> TokenLeftCurly) }
  "}"               { tok (\_ -> TokenRightCurly) }
  "["               { tok (\_ -> TokenLeftBracket) }
  "]"               { tok (\_ -> TokenRightBracket) }
  "|"               { tok (\_ -> TokenVerticalBar) }
  ";"               { tok (\_ -> TokenSemi) }
  ","               { tok (\_ -> TokenComma) }
  "+"               { tok (\_ -> TokenPlus) }
  "<+>"             { tok (\_ -> TokenLessPlusGreater) }
  "-"               { tok (\_ -> TokenMinus) }
  "*"               { tok (\_ -> TokenStar) }
  "/"               { tok (\_ -> TokenSlash) }
  "%"               { tok (\_ -> TokenPercent) }
  "=="              { tok (\_ -> TokenEqualEqual) }
  "/="              { tok (\_ -> TokenSlashEqual) }
  ">"               { tok (\_ -> TokenGreater) }
  ">="              { tok (\_ -> TokenGreaterEqual) }
  "<"               { tok (\_ -> TokenLess) }
  "<="              { tok (\_ -> TokenLessEqual) }
  "'"               { tok (\_ -> TokenSingleQuote) }
  "`"               { tok (\_ -> TokenBackTick) }
  "m`"              { tok (\_ -> TokenMBackTick) }
  "&"               { tok (\_ -> TokenAnd) }
  "->"              { tok (\_ -> TokenMinusGreater) }
  "=>"              { tok (\_ -> TokenEqualGreater) }
  "<=>"             { tok (\_ -> TokenLessEqualGreater) }
  "!"               { tok (\_ -> TokenBang) }
  "="               { tok (\_ -> TokenEqual) }
  "@" @id $white_no_nl* "..." { tok (\s -> TokenLabel (getLabelName s)) }
  @kw                   { tok (\s -> TokenKeyword s) }
  @builtinProc          { tok (\s -> TokenBuiltinProc s) }
  @builtinFn            { tok (\s -> TokenBuiltinFn s) }
  @id                   { tok (\s -> TokenIdentifier s) }
  @decimal              { tok (\s -> TokenInt (read s)) }
  @decimal \. @decimal  { tok (\s -> TokenFloat (read s)) }
  _                     { tok (\_ -> TokenError) }

{

data Token = 
    TokenNewLine
  | TokenAnd
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

getLabelName :: String -> String
getLabelName s = takeWhile (\c -> isAlphaNum c || c `elem` "_") (tail s)

data TokenInfo  = TokenInfo { 
  tokenPos :: AlexPosn, 
  tokenStr :: String, 
  tokenType :: Token
} deriving (Eq, Show)

tok :: (String -> Token) -> AlexPosn -> String -> TokenInfo
tok f p s = TokenInfo p s (f s)

scanTokens = alexScanTokens

}
