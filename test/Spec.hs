import Tokens

main :: IO ()
main = do
  ast <- scanTokens <$> readFile "./test.adpl"
  print ast
