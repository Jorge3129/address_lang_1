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
  | OP_JUMP
  | OP_JUMP_IF_FALSE
  | OP_SEND
  | OP_DEREF
  | OP_DEFINE_VAR
  | OP_SET_VAR
  | OP_GET_VAR
  deriving (Eq, Show, Enum)

data Chunk = Chunk
  { code :: [Int],
    codeLines :: [Int],
    constants :: [Value]
  }
  deriving (Eq, Show)

initChunk :: Chunk
initChunk =
  Chunk
    { code = [],
      codeLines = [],
      constants = []
    }

writeChunk :: Int -> Int -> Chunk -> Chunk
writeChunk byte codeLine ch@(Chunk {code, codeLines}) =
  ch
    { code = code ++ [byte],
      codeLines = codeLines ++ [codeLine]
    }

addConstant :: Value -> Chunk -> (Chunk, Int)
addConstant val ch@(Chunk {constants}) =
  let newChunk = ch {constants = constants ++ [val]}
   in (newChunk, length constants)
