{-# LANGUAGE NamedFieldPuns #-}

module Vm.State where

import ByteCode.Core
import Control.Monad.ST (RealWorld, ST)
import qualified Data.Array as A
import qualified Data.Array.ST as SA
import qualified Data.Map as Map
import Data.STRef
import qualified Utils.Stack as Stack
import Value.Core

type VmMemory = SA.STArray RealWorld Int Value

type VmIp = STRef RealWorld Int

type VmStack = Stack.Stack Value

type VmVarsMap = STRef RealWorld (Map.Map String Int)

type VmCalls = STRef RealWorld [(String, Int)]

data VmChunk = VmChunk
  { vmcCode :: A.Array Int Int,
    vmcCodeLines :: A.Array Int Int,
    vmcConstants :: A.Array Int Value,
    vmcLabelMap :: Map.Map String Int
  }

data VM = VM
  { chunk :: !VmChunk,
    ip :: !VmIp,
    stack :: !VmStack,
    memory :: !VmMemory,
    varsMap :: !VmVarsMap,
    vmCalls :: !VmCalls
  }

memMax :: Int
memMax = 5000

stackMax :: Int
stackMax = 256

newMemory :: Int -> ST RealWorld VmMemory
newMemory size = SA.newListArray (0, size - 1) (0 : replicate (size - 1) NilVal)

initVM :: Chunk -> ST RealWorld VM
initVM ch = do
  mem <- newMemory memMax
  ip <- newSTRef 0
  stack <- Stack.newStack stackMax
  varsMap <- newSTRef Map.empty
  vmCalls <- newSTRef []
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
push :: VM -> Value -> ST RealWorld ()
push vm val = Stack.push val (stack vm)

pop :: VM -> ST RealWorld Value
pop vm = Stack.pop (stack vm)

popStack :: [Value] -> Value
popStack [] = error "operand stack is empty"
popStack (x : _) = x

popN :: Int -> VM -> ST RealWorld [Value]
popN 0 _ = return []
popN n vm = do
  val <- pop vm
  restValues <- popN (n - 1) vm
  return $ val : restValues

peek :: Int -> VM -> ST RealWorld Value
peek offset vm = Stack.peek' offset (stack vm)

addIp :: Int -> VM -> ST RealWorld ()
addIp ipOffset vm = modifySTRef (ip vm) (+ ipOffset)

setIp :: Int -> VM -> ST RealWorld ()
setIp val vm = writeSTRef (ip vm) val

readIp :: VM -> ST RealWorld Int
readIp vm = readSTRef (ip vm)

readCalls :: VM -> ST RealWorld [(String, Int)]
readCalls vm = readSTRef (vmCalls vm)

pushCall :: (String, Int) -> VM -> ST RealWorld ()
pushCall call vm = modifySTRef (vmCalls vm) (call :)

popCall :: VM -> ST RealWorld ()
popCall vm = modifySTRef (vmCalls vm) tail

newChunk :: Chunk -> VmChunk
newChunk ch =
  let chCode = A.listArray (0, length (code ch) - 1) (code ch)
      chCodeLines = A.listArray (0, length (codeLines ch) - 1) (codeLines ch)
      chConstants = A.listArray (0, length (constants ch) - 1) (constants ch)
   in VmChunk
        { vmcCode = chCode,
          vmcCodeLines = chCodeLines,
          vmcConstants = chConstants,
          vmcLabelMap = chLabelMap ch
        }

readChunk :: VmChunk -> Int -> ST RealWorld Int
readChunk ch i = return $ vmcCode ch A.! i

readChunkConst :: VmChunk -> Int -> ST RealWorld Value
readChunkConst ch i = return $ vmcConstants ch A.! i

getFnOffset :: VmChunk -> String -> ST RealWorld Int
getFnOffset ch fnName = return $ vmcLabelMap ch Map.! fnName

readByte :: VM -> ST RealWorld Int
readByte vm = do
  curIp <- readIp vm
  instruction <- readChunk (chunk vm) curIp
  addIp 1 vm
  return instruction

readConst :: VM -> ST RealWorld Value
readConst vm@(VM {chunk}) = do
  constPos <- readByte vm
  readChunkConst chunk constPos

readStr :: VM -> ST RealWorld String
readStr vm = asStr <$> readConst vm
