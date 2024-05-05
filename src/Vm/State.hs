{-# LANGUAGE NamedFieldPuns #-}

module Vm.State where

import ByteCode.Core
import qualified Data.Array as A
import qualified Data.Array.IO as IA
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Utils.Stack as Stack
import Value.Core

type VmMemory = IA.IOArray Int Value

type VmIp = IORef Int

type VmStack = Stack.Stack Value

type VmVarsMap = IORef (Map String Int)

type VmCalls = IORef [(String, Int)]

data VmChunk = VmChunk
  { vmcCode :: A.Array Int Int,
    vmcCodeLines :: A.Array Int Int,
    vmcConstants :: A.Array Int Value
  }

data VM = VM
  { chunk :: !VmChunk,
    ip :: !VmIp,
    stack :: !VmStack,
    memory :: !VmMemory,
    varsMap :: !VmVarsMap,
    vmCalls :: !VmCalls
  }

data InterpretResult = OK | RUNTIME_ERR deriving (Eq, Show)

memMax :: Int
memMax = 5000

stackMax :: Int
stackMax = 256

newMemory :: Int -> IO VmMemory
newMemory size = IA.newListArray (0, size - 1) (0 : replicate (size - 1) NilVal)

initVM :: Chunk -> IO VM
initVM ch = do
  mem <- newMemory memMax
  ip <- newIORef 0
  stack <- Stack.newStack stackMax
  varsMap <- newIORef Map.empty
  vmCalls <- newIORef []
  let chunk = newChunk ch
  return
    VM
      { chunk = chunk,
        ip = ip,
        stack = stack,
        memory = mem,
        varsMap = varsMap,
        vmCalls = vmCalls
      }

-- TODO change arg order
push :: VM -> Value -> IO ()
push vm val = Stack.push val (stack vm)

pop :: VM -> IO Value
pop vm = Stack.pop (stack vm)

popN :: Int -> VM -> IO [Value]
popN 0 _ = return []
popN n vm = do
  val <- pop vm
  restValues <- popN (n - 1) vm
  return $ val : restValues

peek :: Int -> VM -> IO Value
peek offset vm = Stack.peek' offset (stack vm)

addIp :: Int -> VM -> IO ()
addIp ipOffset vm = modifyIORef (ip vm) (+ ipOffset)

setIp :: Int -> VM -> IO ()
setIp val vm = writeIORef (ip vm) val

readIp :: VM -> IO Int
readIp vm = readIORef (ip vm)

readCalls :: VM -> IO [(String, Int)]
readCalls vm = readIORef (vmCalls vm)

pushCall :: (String, Int) -> VM -> IO ()
pushCall call vm = modifyIORef (vmCalls vm) (call :)

popCall :: VM -> IO ()
popCall vm = modifyIORef (vmCalls vm) tail

newChunk :: Chunk -> VmChunk
newChunk ch =
  let chCode = A.listArray (0, length (code ch) - 1) (code ch)
      chCodeLines = A.listArray (0, length (codeLines ch) - 1) (codeLines ch)
      chConstants = A.listArray (0, length (constants ch) - 1) (constants ch)
   in VmChunk
        { vmcCode = chCode,
          vmcCodeLines = chCodeLines,
          vmcConstants = chConstants
        }

readChunk :: VmChunk -> Int -> IO Int
readChunk ch i = return $ vmcCode ch A.! i

readChunkConst :: VmChunk -> Int -> IO Value
readChunkConst ch i = return $ vmcConstants ch A.! i

readByte :: VM -> IO Int
readByte vm = do
  curIp <- readIp vm
  instruction <- readChunk (chunk vm) curIp
  addIp 1 vm
  return instruction

readConst :: VM -> IO Value
readConst vm@(VM {chunk}) = do
  constPos <- readByte vm
  readChunkConst chunk constPos

readStr :: VM -> IO String
readStr vm = asStr <$> readConst vm
