{-# LANGUAGE NamedFieldPuns #-}

module ByteCode where

import Data.Map (Map, empty)
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
  deriving (Eq, Show, Enum)

type LabelOffsetMap = Map String Int

data Chunk = Chunk
  { code :: [Int],
    codeLines :: [Int],
    constants :: [Value],
    labelOffsetMap :: LabelOffsetMap,
    labelJumpsToPatch :: [(Int, String)]
  }
  deriving (Eq, Show)

initChunk :: Chunk
initChunk =
  Chunk
    { code = [],
      codeLines = [],
      constants = [],
      labelOffsetMap = Data.Map.empty,
      labelJumpsToPatch = []
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
