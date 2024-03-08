import ByteCode
import Grammar
import Tokens
import Vm

main :: IO ()
main = do
  -- tokens <- scanTokens <$> readFile "D:/DiplomaFiles/addr-lang1/test/data/test1.adpl"
  -- print tokens
  -- let ast = parseProg tokens
  -- print ast
  -- print $ fromEnum OP_RETURN
  let ch = initChunk
      (ch1, constant) = addConstant ch (FloatVal 1.2)
      ch2 = writeChunk ch1 (fromEnum OP_CONSTANT)
      ch3 = writeChunk ch2 constant
      ch4 = writeChunk ch3 (fromEnum OP_RETURN)
  disassembleChunk ch4 "test chunk"
  res <- run (initVM ch4)
  print res
