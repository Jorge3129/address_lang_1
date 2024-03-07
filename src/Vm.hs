{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vm where

import Control.Monad.Loops

data OpCode = OP_RETURN deriving (Eq, Show, Enum)

newtype Chunk = Chunk
  { code :: [Int]
  }
  deriving (Show)

initChunk :: Chunk
initChunk = Chunk {code = []}

writeChunk :: Chunk -> Int -> Chunk
writeChunk ch@(Chunk {code}) byte = ch {code = code ++ [byte]}

disassembleChunk :: Chunk -> String -> IO ()
disassembleChunk chunk name = do
  putStrLn $ "== " ++ name ++ " =="
  disassembleInstructions chunk 0

opcodeToString :: OpCode -> String
opcodeToString OP_RETURN = "OP_RETURN"

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
    _ -> do
      putStrLn $ "Unknown opcode " ++ show instruction
      return $ offset + 1

simpleInstruction :: String -> Int -> IO Int
simpleInstruction name offset = do
  putStrLn name
  return $ offset + 1