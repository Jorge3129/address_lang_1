{-# LANGUAGE NamedFieldPuns #-}

module Compiler.State where

import ByteCode.Core
import Control.Monad (foldM, foldM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
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
  { curChunk :: IORef Chunk,
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
  curChunk <- newIORef initChunk
  return $
    CompState
      { curChunk = curChunk,
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

getCurChunk :: CompState -> IO Chunk
getCurChunk cs = readIORef (curChunk cs)

updateChunk :: (Chunk -> Chunk) -> CompState -> IO ()
updateChunk f cs = modifyIORef (curChunk cs) f

emitByte :: Int -> CompState -> IO ()
emitByte byte cs = do
  curLn <- getCurLine cs
  modifyIORef (curChunk cs) (writeChunk byte curLn)

emitOpCode :: OpCode -> CompState -> IO ()
emitOpCode op = emitByte (fromEnum op)

emitOpCodes :: [OpCode] -> CompState -> IO ()
emitOpCodes ops cs = foldM_ (\cs_ op -> emitOpCode op cs_ >> return cs_) cs ops

addConstantToCs :: Value -> CompState -> IO Int
addConstantToCs val cs = do
  ch <- getCurChunk cs
  let (newCh, constant) = addConstant val ch
  writeIORef (curChunk cs) newCh
  return constant

curChunkCount :: CompState -> IO Int
curChunkCount cs = length . code <$> getCurChunk cs