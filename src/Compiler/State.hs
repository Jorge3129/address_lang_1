module Compiler.State where

import ByteCode.Core
import Control.Monad (forM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Parser.AST (ProgLine)
import Utils.Core (replace)
import Value.Core

type LabelOffsetMap = Map String Int

type FnVarMap = Map String [String]

type LineFnMap = Map Int String

data LoopPatch = LoopPatch
  { endLabel :: Maybe String,
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
    labelRefPatches :: IORef [(Int, String)],
    csFnVars :: FnVarMap,
    csFnMap :: LineFnMap,
    csProgLines :: [ProgLine],
    csRepls :: IORef [Int]
  }

initCs :: [ProgLine] -> FnVarMap -> LineFnMap -> IO CompState
initCs prog fnVars fnMap = do
  initCurLine <- newIORef 0
  initCurChunk <- newIORef initChunk
  initLoopPatches <- newIORef []
  initJumpPatches <- newIORef []
  initLabelPatches <- newIORef []
  initLabelOffsetMap <- newIORef Map.empty
  initRepls <- newIORef []
  return $
    CompState
      { curChunk = initCurChunk,
        curLine = initCurLine,
        labelOffsetMap = initLabelOffsetMap,
        jumpPatches = initJumpPatches,
        loopPatches = initLoopPatches,
        labelRefPatches = initLabelPatches,
        csFnVars = fnVars,
        csFnMap = fnMap,
        csProgLines = prog,
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

patchChunkConstant :: Int -> Value -> CompState -> IO ()
patchChunkConstant offset value =
  updateChunk
    ( \ch ->
        let constIndex = code ch !! offset
         in ch {constants = replace constIndex value (constants ch)}
    )

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

getLabelRefPatches :: CompState -> IO [(Int, String)]
getLabelRefPatches cs = readIORef (labelRefPatches cs)

addLabelRefPatch :: Int -> String -> CompState -> IO ()
addLabelRefPatch curOffset lbl cs = do
  modifyIORef (labelRefPatches cs) (++ [(curOffset, lbl)])

getReplacements :: CompState -> IO [Int]
getReplacements cs = readIORef (csRepls cs)

pushReplacement :: Int -> CompState -> IO ()
pushReplacement r cs = modifyIORef (csRepls cs) (r :)

popReplacement :: CompState -> IO ()
popReplacement cs = modifyIORef (csRepls cs) tail

-- Chunk utils
curChunkCount :: CompState -> IO Int
curChunkCount cs = length . code <$> getCurChunk cs

addConstantToCs :: Value -> CompState -> IO Int
addConstantToCs val cs = do
  ch <- getCurChunk cs
  let (newCh, constant) = addConstant val ch
  writeIORef (curChunk cs) newCh
  return constant

emitByte :: Int -> CompState -> IO ()
emitByte byte cs = do
  curLn <- getCurLine cs
  modifyIORef (curChunk cs) (writeChunk byte curLn)

emitOpCode :: OpCode -> CompState -> IO ()
emitOpCode op = emitByte (fromEnum op)

emitOpCodes :: [OpCode] -> CompState -> IO ()
emitOpCodes ops cs = forM_ ops (`emitOpCode` cs)

emitJump :: OpCode -> CompState -> IO Int
emitJump op cs = do
  emitOpCode op cs
  emitByte 0 cs
  chunkCount <- curChunkCount cs
  return $ chunkCount - 1

emitLoop :: Int -> CompState -> IO ()
emitLoop jumpToInstr cs = do
  chunkCount <- curChunkCount cs
  let jumpOffset = jumpToInstr - chunkCount - 2
  emitOpCode OP_JUMP cs
  emitByte jumpOffset cs