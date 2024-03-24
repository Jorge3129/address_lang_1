import Compiler
import Debug
import Grammar
import System.Directory (getCurrentDirectory)
import Tokens
import Vm

main :: IO ()
main = do
  rootDir <- getCurrentDirectory
  let basePath = rootDir ++ "/test/data"
      fileName = "pointer"
  tokens <- scanTokens <$> readFile (basePath ++ "/" ++ fileName ++ ".adpl")
  -- print tokens
  let progAst = parseProg tokens
  print progAst
  ch <- compileProg progAst
  disassembleChunk ch "test chunk"
  res <- run (initVM ch)
  print res