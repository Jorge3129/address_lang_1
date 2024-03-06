module Main (main) where

import Lib

main :: IO ()
main = do
  s <- getContents
  let ast = getAst s
  print ast
  print (run ast)
