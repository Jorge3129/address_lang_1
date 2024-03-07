import Grammar
import Tokens
import Vm
import Vm (OpCode (OP_RETURN), initChunk)

main :: IO ()
main = do
  -- tokens <- scanTokens <$> readFile "D:/DiplomaFiles/addr-lang1/test/data/test1.adpl"
  -- print tokens
  -- let ast = parseProg tokens
  -- print ast
  -- print $ fromEnum OP_RETURN
  let ch = initChunk
      ch1 = writeChunk ch (fromEnum OP_RETURN)
  disassembleChunk ch1 "test chunk"
