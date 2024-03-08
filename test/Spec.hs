import ByteCode
import Compiler
import Grammar
import Tokens
import Vm

type Action = Chunk -> Chunk

actions :: [Action]
actions =
  [ \ch ->
      let (ch1, constant) = addConstant ch (FloatVal 1.2)
          ch2 = writeChunk ch1 (fromEnum OP_CONSTANT)
       in writeChunk ch2 constant,
    \ch ->
      let (ch1, constant) = addConstant ch (FloatVal 3.4)
          ch2 = writeChunk ch1 (fromEnum OP_CONSTANT)
       in writeChunk ch2 constant,
    \ch -> writeChunk ch (fromEnum OP_ADD),
    \ch -> writeChunk ch (fromEnum OP_RETURN)
  ]

main :: IO ()
main = do
  -- tokens <- scanTokens <$> readFile "D:/DiplomaFiles/addr-lang1/test/data/test1.adpl"
  -- print tokens
  -- let ast = parseProg tokens
  -- print ast
  -- print $ fromEnum OP_RETURN
  let ch = initChunk
  newCh <- compileExpr ch (BinOpApp Add (Lit 1) (Lit 2))
  let newCh1 = writeChunk newCh (fromEnum OP_RETURN)
  disassembleChunk newCh1 "test chunk"
  res <- run (initVM newCh1)
  print res
