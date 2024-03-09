{-# LANGUAGE NamedFieldPuns #-}

module ByteCode where

import Value

data OpCode
  = OP_RETURN
  | OP_CONSTANT
  | OP_NOT
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_EQUAL
  | OP_GREATER
  | OP_LESS
  | OP_PRINT
  | OP_POP
  deriving (Eq, Show, Enum)

data Chunk = Chunk
  { code :: [Int],
    constants :: [Value]
  }
  deriving (Eq, Show)

initChunk :: Chunk
initChunk = Chunk {code = [], constants = []}

writeChunk :: Int -> Chunk -> Chunk
writeChunk byte ch@(Chunk {code}) = ch {code = code ++ [byte]}

addConstant :: Value -> Chunk -> (Chunk, Int)
addConstant val ch@(Chunk {constants}) =
  let newChunk = ch {constants = constants ++ [val]}
   in (newChunk, length constants)
