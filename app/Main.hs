module Main (main) where

import Lib

main :: IO ()
main = do
  s <- getContents
  let ast = parseCalc (scanTokens s)
  print ast
  print (run ast)
