{-# LANGUAGE NamedFieldPuns #-}

module Compiler.State where

import ByteCode.Core
import Control.Monad (foldM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Grammar (Program)
import Value.Core

type LabelOffsetMap = Map.Map String Int

type FnVarMap = Map.Map String [String]

type LineFnMap = Map.Map Int String

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
    curLine :: IORef Int,
    labelOffsetMap :: LabelOffsetMap,
    labelJumpsToPatch :: [(Int, String)],
    loopPatches :: [LoopPatch],
    csFnVars :: FnVarMap,
    csFnMap :: LineFnMap,
    csProg :: Program,
    csRepls :: [Int]
  }

initCs :: Program -> IO CompState
initCs prog = do
  curLine <- newIORef 0
  return $
    CompState
      { curChunk = initChunk,
        curLine = curLine,
        labelOffsetMap = Map.empty,
        labelJumpsToPatch = [],
        loopPatches = [],
        csFnVars = Map.empty,
        csFnMap = Map.empty,
        csProg = prog,
        csRepls = []
      }

getCurLine :: CompState -> IO Int
getCurLine cs = readIORef (curLine cs)

setCurLine :: Int -> CompState -> IO ()
setCurLine v cs = writeIORef (curLine cs) v

emitByte :: Int -> CompState -> IO CompState
emitByte byte cs = do
  let chunk = curChunk cs
  curLn <- getCurLine cs
  return $
    cs
      { curChunk = writeChunk (fromEnum byte) curLn chunk
      }

emitOpCode :: OpCode -> CompState -> IO CompState
emitOpCode op = emitByte (fromEnum op)

emitOpCodes :: [OpCode] -> CompState -> IO CompState
emitOpCodes ops cs = foldM (flip emitOpCode) cs ops

addConstantToCs :: Value -> CompState -> (CompState, Int)
addConstantToCs val cs@(CompState {curChunk}) =
  let (newCh, constant) = addConstant val curChunk
   in (cs {curChunk = newCh}, constant)

curChunkCount :: CompState -> Int
curChunkCount = length . code . curChunk