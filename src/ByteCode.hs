{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module ByteCode where

data OpCode
  = OP_RETURN
  | OP_CONSTANT
  | OP_ADD
  deriving (Eq, Show, Enum)

data Value = IntVal Int | FloatVal Float deriving (Eq)

addVals :: Value -> Value -> Value
addVals (IntVal a) (IntVal b) = IntVal $ a + b
addVals (IntVal a) (FloatVal b) = FloatVal $ (fromIntegral a :: Float) + b
addVals (FloatVal a) (IntVal b) = FloatVal $ a + (fromIntegral b :: Float)
addVals (FloatVal a) (FloatVal b) = FloatVal $ a + b

instance Show Value where
  show (IntVal v) = show v
  show (FloatVal v) = show v

data Chunk = Chunk
  { code :: [Int],
    constants :: [Value]
  }
  deriving (Eq, Show)

initChunk :: Chunk
initChunk = Chunk {code = [], constants = []}

writeChunk :: Chunk -> Int -> Chunk
writeChunk ch@(Chunk {code}) byte = ch {code = code ++ [byte]}

addConstant :: Chunk -> Value -> (Chunk, Int)
addConstant ch@(Chunk {constants}) val =
  let newChunk = ch {constants = constants ++ [val]}
   in (newChunk, length constants)

disassembleChunk :: Chunk -> String -> IO ()
disassembleChunk chunk name = do
  putStrLn $ "== " ++ name ++ " =="
  disassembleInstructions chunk 0

disassembleInstructions :: Chunk -> Int -> IO ()
disassembleInstructions chunk offset
  | offset < length (code chunk) = do
      newOffset <- disassembleInstruction chunk offset
      disassembleInstructions chunk newOffset
  | otherwise = return ()

lpad :: a -> Int -> [a] -> [a]
lpad pad m xs = replicate (m - length ys) pad ++ ys
  where
    ys = take m xs

disassembleInstruction :: Chunk -> Int -> IO Int
disassembleInstruction chunk offset = do
  putStr $ lpad '0' 4 (show offset) ++ " "
  let instruction = code chunk !! offset
  case toEnum instruction :: OpCode of
    OP_RETURN -> simpleInstruction "OP_RETURN" offset
    OP_ADD -> simpleInstruction "OP_ADD" offset
    OP_CONSTANT -> constantInstruction "OP_CONSTANT" chunk offset
    _ -> do
      putStrLn $ "Unknown opcode " ++ show instruction
      return $ offset + 1

simpleInstruction :: String -> Int -> IO Int
simpleInstruction name offset = do
  putStrLn name
  return $ offset + 1

constantInstruction :: String -> Chunk -> Int -> IO Int
constantInstruction name (Chunk {code, constants}) offset = do
  let constant = code !! (offset + 1)
  putStr $ name ++ " " ++ show constant ++ " "
  print (constants !! constant)
  return $ offset + 2