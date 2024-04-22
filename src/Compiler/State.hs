module Compiler.State where

import ByteCode.Core
import Control.Monad (forM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Grammar (Program)
import MyUtils (replace)
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
  { curLine :: IORef Int,
    curChunk :: IORef Chunk,
    labelOffsetMap :: IORef LabelOffsetMap,
    jumpPatches :: IORef [(Int, String)],
    loopPatches :: IORef [LoopPatch],
    csFnVars :: FnVarMap,
    csFnMap :: LineFnMap,
    csProg :: Program,
    csRepls :: IORef [Int]
  }

initCs :: Program -> IO CompState
initCs prog = do
  initCurLine <- newIORef 0
  initCurChunk <- newIORef initChunk
  initLoopPatches <- newIORef []
  initJumpPatches <- newIORef []
  initLabelOffsetMap <- newIORef Map.empty
  initRepls <- newIORef []
  return $
    CompState
      { curChunk = initCurChunk,
        curLine = initCurLine,
        labelOffsetMap = initLabelOffsetMap,
        jumpPatches = initJumpPatches,
        loopPatches = initLoopPatches,
        csFnVars = Map.empty,
        csFnMap = Map.empty,
        csProg = prog,
        csRepls = initRepls
      }

getCurLine :: CompState -> IO Int
getCurLine cs = readIORef (curLine cs)

setCurLine :: Int -> CompState -> IO ()
setCurLine v cs = writeIORef (curLine cs) v

getCurChunk :: CompState -> IO Chunk
getCurChunk cs = readIORef (curChunk cs)

updateChunk :: (Chunk -> Chunk) -> CompState -> IO ()
updateChunk f cs = modifyIORef (curChunk cs) f

patchChunkCode :: Int -> Int -> CompState -> IO ()
patchChunkCode offset value = updateChunk (\ch -> ch {code = replace offset value (code ch)})

getLabelOffsetMap :: CompState -> IO LabelOffsetMap
getLabelOffsetMap cs = readIORef (labelOffsetMap cs)

setLabelOffsetMap :: LabelOffsetMap -> CompState -> IO ()
setLabelOffsetMap v cs = writeIORef (labelOffsetMap cs) v

getLoopPatches :: CompState -> IO [LoopPatch]
getLoopPatches cs = readIORef (loopPatches cs)

setLoopPatches :: [LoopPatch] -> CompState -> IO ()
setLoopPatches v cs = writeIORef (loopPatches cs) v

addLoopPatch :: LoopPatch -> CompState -> IO ()
addLoopPatch p cs = modifyIORef (loopPatches cs) (p :)

getJumpPatches :: CompState -> IO [(Int, String)]
getJumpPatches cs = readIORef (jumpPatches cs)

addJumpPatch :: Int -> String -> CompState -> IO ()
addJumpPatch curOffset lbl cs = do
  modifyIORef (jumpPatches cs) (++ [(curOffset, lbl)])

getReplacements :: CompState -> IO [Int]
getReplacements cs = readIORef (csRepls cs)

pushReplacement :: Int -> CompState -> IO ()
pushReplacement r cs = modifyIORef (csRepls cs) (r :)

popReplacement :: CompState -> IO ()
popReplacement cs = modifyIORef (csRepls cs) tail

emitByte :: Int -> CompState -> IO ()
emitByte byte cs = do
  curLn <- getCurLine cs
  modifyIORef (curChunk cs) (writeChunk byte curLn)

emitOpCode :: OpCode -> CompState -> IO ()
emitOpCode op = emitByte (fromEnum op)

emitOpCodes :: [OpCode] -> CompState -> IO ()
emitOpCodes ops cs = forM_ ops (`emitOpCode` cs)

addConstantToCs :: Value -> CompState -> IO Int
addConstantToCs val cs = do
  ch <- getCurChunk cs
  let (newCh, constant) = addConstant val ch
  writeIORef (curChunk cs) newCh
  return constant

curChunkCount :: CompState -> IO Int
curChunkCount cs = length . code <$> getCurChunk cs