{-# LANGUAGE NamedFieldPuns #-}

module ByteCode where

import Data.Map (Map, fromList)
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
  deriving (Eq, Show, Enum)

type LabelLineMap = Map String Int

data Chunk = Chunk
  { code :: [Int],
    constants :: [Value],
    labelLineMap :: LabelLineMap
  }
  deriving (Eq, Show)

initChunk :: LabelLineMap -> Chunk
initChunk labelLineMap =
  Chunk
    { code = [],
      constants = [],
      labelLineMap = labelLineMap
    }

writeChunk :: Int -> Chunk -> Chunk
writeChunk byte ch@(Chunk {code}) = ch {code = code ++ [byte]}

addConstant :: Value -> Chunk -> (Chunk, Int)
addConstant val ch@(Chunk {constants}) =
  let newChunk = ch {constants = constants ++ [val]}
   in (newChunk, length constants)
