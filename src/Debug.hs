{-# LANGUAGE NamedFieldPuns #-}

module Debug where

import ByteCode

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

getPrintLineData :: Chunk -> Int -> String
getPrintLineData (Chunk {codeLines}) offset =
  if offset > 0 && (codeLines !! offset) == (codeLines !! (offset - 1))
    then "   |  "
    else fmt4 ' ' ((codeLines !! offset) + 1) ++ "  "

fmt4 :: Char -> Int -> String
fmt4 c x = lpad c 4 (show x)

disassembleInstruction :: Chunk -> Int -> IO Int
disassembleInstruction chunk offset = do
  putStr $ fmt4 '0' offset ++ " "
  putStr $ getPrintLineData chunk offset
  let instruction = code chunk !! offset
  case toEnum instruction :: OpCode of
    OP_RETURN -> simpleInstruction "OP_RETURN" offset
    OP_ADD -> simpleInstruction "OP_ADD" offset
    OP_SUB -> simpleInstruction "OP_SUB" offset
    OP_MUL -> simpleInstruction "OP_MUL" offset
    OP_DIV -> simpleInstruction "OP_DIV" offset
    OP_NOT -> simpleInstruction "OP_NOT" offset
    OP_PRINT -> simpleInstruction "OP_PRINT" offset
    OP_POP -> simpleInstruction "OP_POP" offset
    OP_SEND -> simpleInstruction "OP_SEND" offset
    OP_JUMP -> jumpInstruction "OP_JUMP" chunk offset
    OP_JUMP_IF_FALSE -> jumpInstruction "OP_JUMP_IF_FALSE" chunk offset
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

jumpInstruction :: String -> Chunk -> Int -> IO Int
jumpInstruction name (Chunk {code}) offset = do
  let jumpOffset = code !! (offset + 1)
  putStrLn $ name ++ " " ++ show jumpOffset ++ " "
  return $ offset + 2