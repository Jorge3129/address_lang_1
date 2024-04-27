{-# LANGUAGE NamedFieldPuns #-}

module Debug where

import ByteCode.Core
import Utils.Core (lpad)

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

getPrintLineData :: Chunk -> Int -> String
getPrintLineData (Chunk {codeLines}) offset =
  let curLine = codeLines !! offset
   in if offset > 0 && curLine == (codeLines !! (offset - 1))
        then "   |  "
        else fmt4 ' ' (curLine + 1) ++ "  "

getLineByOffset :: Int -> Chunk -> Int
getLineByOffset offset (Chunk {codeLines}) = codeLines !! offset

fmt4 :: Char -> Int -> String
fmt4 c x = lpad c 4 (show x)

disassembleInstruction :: Chunk -> Int -> IO Int
disassembleInstruction chunk offset = do
  putStr $ fmt4 '0' offset ++ " "
  putStr $ getPrintLineData chunk offset
  let instruction = code chunk !! offset
  case toEnum instruction :: OpCode of
    OP_RETURN -> simpleInstruction "OP_RETURN" offset
    --
    OP_ADD -> simpleInstruction "OP_ADD" offset
    OP_PTR_ADD -> simpleInstruction "OP_PTR_ADD" offset
    OP_SUB -> simpleInstruction "OP_SUB" offset
    OP_MUL -> simpleInstruction "OP_MUL" offset
    OP_DIV -> simpleInstruction "OP_DIV" offset
    OP_MOD -> simpleInstruction "OP_MOD" offset
    --
    OP_EQUAL -> simpleInstruction "OP_EQUAL" offset
    OP_GREATER -> simpleInstruction "OP_GREATER" offset
    OP_LESS -> simpleInstruction "OP_LESS" offset
    --
    OP_NOT -> simpleInstruction "OP_NOT" offset
    OP_AND -> simpleInstruction "OP_AND" offset
    OP_OR -> simpleInstruction "OP_OR" offset
    OP_NEGATE -> simpleInstruction "OP_NEGATE" offset
    --
    OP_POP -> simpleInstruction "OP_POP" offset
    OP_SEND -> simpleInstruction "OP_SEND" offset
    OP_DEREF -> simpleInstruction "OP_DEREF" offset
    OP_MUL_DEREF -> simpleInstruction "OP_MUL_DEREF" offset
    OP_MAKE_POINTER -> simpleInstruction "OP_MAKE_POINTER" offset
    OP_EXCHANGE -> simpleInstruction "OP_EXCHANGE" offset
    OP_GET_REFS -> simpleInstruction "OP_GET_REFS" offset
    --
    OP_ALLOC -> simpleInstruction "OP_ALLOC" offset
    --
    OP_JUMP -> jumpInstruction "OP_JUMP" chunk offset
    OP_JUMP_IF_FALSE -> jumpInstruction "OP_JUMP_IF_FALSE" chunk offset
    --
    OP_CONSTANT -> constantInstruction "OP_CONSTANT" chunk offset
    OP_DEFINE_VAR -> constantInstruction "OP_DEFINE_VAR" chunk offset
    OP_GET_VAR -> constantInstruction "OP_GET_VAR" chunk offset
    OP_SET_VAR -> constantInstruction "OP_SET_VAR" chunk offset
    OP_MAKE_VAR_POINTER -> constantInstruction "OP_MAKE_VAR_POINTER" chunk offset
    OP_CALL -> jumpInstruction "OP_CALL" chunk offset
    OP_CALL_PROC -> constantInstruction "OP_CALL_PROC" chunk offset
    OP_CALL_FN -> constantInstruction "OP_CALL_FN" chunk offset

-- _ -> do
--   putStrLn $ "Unknown opcode " ++ show instruction
--   return $ offset + 1

simpleInstruction :: String -> Int -> IO Int
simpleInstruction name offset = do
  putStrLn name
  return $ offset + 1

constantInstruction :: String -> Chunk -> Int -> IO Int
constantInstruction name (Chunk {code, constants}) offset = do
  let constant = code !! (offset + 1)
  -- putStr $ name ++ " " ++ show constant ++ " "
  putStr $ name ++ " "
  print (constants !! constant)
  return $ offset + 2

jumpInstruction :: String -> Chunk -> Int -> IO Int
jumpInstruction name (Chunk {code}) offset = do
  let jumpOffset = code !! (offset + 1)
  putStrLn $ name ++ " " ++ show jumpOffset ++ " "
  return $ offset + 2