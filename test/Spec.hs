import ByteCode
import Compiler
import Debug
import Grammar
import Tokens
import Vm

main :: IO ()
main = do
  tokens <- scanTokens <$> readFile "D:/DiplomaFiles/addr-lang1/test/data/test.adpl"
  -- print tokens
  let progAst = parseProg tokens
  print progAst

-- ch <- compileProg progAst
-- disassembleChunk ch "test chunk"
-- res <- run (initVM ch)
-- print res
