import ByteCode
import Compiler
import Debug
import Grammar
import Tokens
import Value
import Vm

type Action = Chunk -> Chunk

actions :: [Action]
actions =
  [ \ch ->
      let (ch1, constant) = addConstant (FloatVal 1.2) ch
          ch2 = writeChunk (fromEnum OP_CONSTANT) ch1
       in writeChunk constant ch2,
    \ch ->
      let (ch1, constant) = addConstant (FloatVal 3.4) ch
          ch2 = writeChunk (fromEnum OP_CONSTANT) ch1
       in writeChunk constant ch2,
    writeChunk (fromEnum OP_ADD),
    writeChunk (fromEnum OP_RETURN)
  ]

main :: IO ()
main = do
  -- tokens <- scanTokens <$> readFile "D:/DiplomaFiles/addr-lang1/test/data/test1.adpl"
  -- print tokens
  -- let ast = parseProg tokens
  -- print ast
  -- print $ fromEnum OP_RETURN
  let ch = initChunk
  newCh1 <-
    writeChunk (fromEnum OP_RETURN)
      <$> compileStmt ch (BuiltinFunc "print" [(BinOpApp Add (Lit 1) (Lit 2))])
  disassembleChunk newCh1 "test chunk"
  res <- run (initVM newCh1)
  print res
