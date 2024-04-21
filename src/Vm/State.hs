{-# LANGUAGE NamedFieldPuns #-}

module Vm.State where

import ByteCode.Core
import qualified Data.Array.IO as IA
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Value.Core

type VMMemory = IA.IOArray Int Value

type VmIp = IORef Int

type VmStack = IORef [Value]

data VM = VM
  { chunk :: !Chunk,
    ip :: !VmIp,
    stack :: !VmStack,
    memory :: !VMMemory,
    varsMap :: !(Map.Map String Int),
    vmCalls :: ![(String, Int)]
  }

memMax :: Int
memMax = 5000

newMemory :: Int -> IO VMMemory
newMemory size = IA.newListArray (0, size - 1) (0 : replicate (size - 1) NilVal)

initVM :: Chunk -> IO VM
initVM ch = do
  mem <- newMemory memMax
  ip <- newIORef 0
  stack <- newIORef []
  return
    VM
      { chunk = ch,
        ip = ip,
        stack = stack,
        memory = mem,
        varsMap = Map.empty,
        vmCalls = []
      }

-- TODO change arg order
push :: VM -> Value -> IO ()
push vm val = modifyIORef (stack vm) (val :)

pop :: VM -> IO Value
pop vm = do
  curStack <- readIORef (stack vm)
  let topVal = popStack curStack
  writeIORef (stack vm) (tail curStack)
  return topVal

popStack :: [Value] -> Value
popStack [] = error "operand stack is empty"
popStack (x : _) = x

popN :: Int -> VM -> IO [Value]
popN 0 _ = return []
popN n vm = do
  val <- pop vm
  restValues <- popN (n - 1) vm
  return $ val : restValues

peek :: Int -> VM -> IO Value
peek offset vm = do
  curStack <- readIORef (stack vm)
  return $ curStack !! offset

addIp :: Int -> VM -> IO ()
addIp ipOffset vm = do
  modifyIORef (ip vm) (+ ipOffset)

setIp :: Int -> VM -> IO ()
setIp val vm = do
  writeIORef (ip vm) val

readIp :: VM -> IO Int
readIp vm = readIORef (ip vm)

readByte :: VM -> IO Int
readByte vm = do
  curIp <- readIORef (ip vm)
  let instruction = code (chunk vm) !! curIp
  addIp 1 vm
  return instruction

readConst :: VM -> IO Value
readConst vm@(VM {chunk}) = do
  constPos <- readByte vm
  return $ constants chunk !! constPos

readStr :: VM -> IO String
readStr vm = asStr <$> readConst vm
