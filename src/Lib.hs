module Lib (execFile) where

import Compiler.Core
import Grammar
import System.Directory
import Tokens
import Vm.Core
import Vm.State

execFile :: String -> IO ()
execFile fileName = do
  rootDir <- getCurrentDirectory
  let basePath = rootDir ++ "/test/data"
  tokens <- scanTokens <$> readFile (basePath ++ "/" ++ fileName ++ ".adpl")
  let progAst = parseProg tokens
  ch <- compileProg progAst
  vm <- initVM ch
  res <- run vm
  print res
  return ()