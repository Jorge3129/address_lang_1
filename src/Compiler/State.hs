{-# LANGUAGE NamedFieldPuns #-}

module Compiler.State where

import ByteCode.Core
import Data.List (foldl')
import Data.Map (Map, empty)
import Value.Core

type LabelOffsetMap = Map String Int

type FnVarMap = Map String [String]

type LineFnMap = Map Int String

data LoopPatch = LoopPatch
  { scopeLabel :: Maybe String,
    nextLabel :: Maybe String,
    loopLine :: Int,
    stepStart :: Int,
    exitJump :: Int
  }
  deriving (Eq, Show)

data CompState = CompState
  { curChunk :: Chunk,
    curLine :: Int,
    labelOffsetMap :: LabelOffsetMap,
    labelJumpsToPatch :: [(Int, String)],
    loopPatches :: [LoopPatch],
    csFnVars :: FnVarMap,
    csFnMap :: LineFnMap
  }
  deriving (Eq, Show)

initCs :: CompState
initCs =
  CompState
    { curChunk = initChunk,
      curLine = 0,
      labelOffsetMap = Data.Map.empty,
      labelJumpsToPatch = [],
      loopPatches = [],
      csFnVars = Data.Map.empty,
      csFnMap = Data.Map.empty
    }

emitByte :: Int -> CompState -> CompState
emitByte byte cs@(CompState {curChunk, curLine}) =
  cs
    { curChunk = writeChunk (fromEnum byte) curLine curChunk
    }

emitOpCode :: OpCode -> CompState -> CompState
emitOpCode op = emitByte (fromEnum op)

emitOpCodes :: [OpCode] -> CompState -> CompState
emitOpCodes ops cs = foldl' (flip emitOpCode) cs ops

addConstantToCs :: Value -> CompState -> (CompState, Int)
addConstantToCs val cs@(CompState {curChunk}) =
  let (newCh, constant) = addConstant val curChunk
   in (cs {curChunk = newCh}, constant)

curChunkCount :: CompState -> Int
curChunkCount = length . code . curChunk