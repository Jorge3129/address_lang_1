import Grammar
import Tokens

main :: IO ()
main = do
  tokens <- scanTokens <$> readFile "D:/DiplomaFiles/addr-lang1/test/data/test1.adpl"
  print tokens
  let ast = parseProg tokens
  print ast
