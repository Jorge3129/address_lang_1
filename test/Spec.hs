import Tokens

main :: IO ()
main = do
  ast <- scanTokens <$> readFile "D:/DiplomaFiles/addr-lang1/test/test.adpl"
  print ast
