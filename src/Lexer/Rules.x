{
module Lexer.Rules where

import Data.Char (isAlphaNum)
}

%wrapper "posn"

$alpha    = [a-zA-Z]
$digit     = 0-9
$white_no_nl = [\ \t]
$lf = \n
$cr = \r
@eol_pattern = $lf | $cr $lf | $cr $lf
@identifier = $alpha ($alpha | $digit | _)*
@decimal = $digit+
@kw = (P|L|R|Pg|Nil|Ret|Cj|not|and|or)
@builtinProc = (print|printList|printRefs)
@builtinFn = (alloc|ptr|id|mulalloc)

tokens :-
  $white_no_nl+ ;
  "//".*        ;
  @eol_pattern      { tok' TNewLine }
  "("               { tok' TLPar }
  ")"               { tok' TRPar }
  "{"               { tok' TLCurly }
  "}"               { tok' TRCurly }
  "["               { tok' TLBrack }
  "]"               { tok' TRBrack }
  "|"               { tok' TBar }
  ";"               { tok' TSemi }
  ","               { tok' TComma }
  "+"               { tok' TPlus }
  "<+>"             { tok' TCirclePlus }
  "-"               { tok' TMinus }
  "*"               { tok' TTimes }
  "/"               { tok' TDiv }
  "%"               { tok' TMod }
  "=="              { tok' TEq }
  "/="              { tok' TNeq }
  ">"               { tok' TGt }
  ">="              { tok' TGe }
  "<"               { tok' TLt }
  "<="              { tok' TLe }
  "'"               { tok' TSQuote }
  "`"               { tok' TBackTick }
  "m`"              { tok' TMBackTick }
  "&"               { tok' TAnd }
  "->"              { tok' TSArrow }
  "=>"              { tok' TDArrow }
  "<=>"             { tok' TExchange }
  "!"               { tok' TStop }
  "="               { tok' TAssign }
  "@" @identifier $white_no_nl* "..." { tok (\s -> TLabel (getLabelName s)) }
  @kw                   { tok (\s -> TKeyword s) }
  @builtinProc          { tok (\s -> TProc s) }
  @builtinFn            { tok (\s -> TFn s) }
  @identifier                   { tok (\s -> TIdentifier s) }
  @decimal              { tok (\s -> TInt (read s)) }
  @decimal \. @decimal  { tok (\s -> TFloat (read s)) }
  _                     { tok' TError }

{

data TokenType = 
    TNewLine | TAnd | TBar | TSemi | TComma 
    | TPlus | TCirclePlus | TMinus | TTimes | TDiv | TMod 
    | TLPar | TRPar | TLCurly | TRCurly | TLBrack | TRBrack 
    | TStop | TAssign | TEq | TNeq | TGt | TGe | TLt | TLe 
    | TSQuote | TBackTick | TMBackTick | TSArrow | TDArrow | TExchange 
    | TLabel String | TIdentifier String | TKeyword String 
    | TProc String | TFn String | TInt Int | TFloat Double 
    | TError
  deriving (Show, Eq)

data Token  = Token { 
  tokenPos :: AlexPosn, 
  tokenStr :: String, 
  tokenType :: TokenType
} deriving (Eq, Show)

tok :: (String -> TokenType) -> AlexPosn -> String -> Token
tok f p s = Token p s (f s)

tok' :: TokenType -> AlexPosn -> String -> Token
tok' t p s = Token p s t

getLabelName :: String -> String
getLabelName s = takeWhile (\c -> isAlphaNum c || c `elem` "_") (tail s)

scanTokens = alexScanTokens

}
