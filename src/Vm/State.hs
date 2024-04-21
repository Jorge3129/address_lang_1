{-# LANGUAGE NamedFieldPuns #-}

module Vm.State where

import ByteCode.Core
import qualified Data.Array.IO as IA
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Value.Core

type VMMemory = IA.IOArray Int Value

type VmIp = IORef Int

data VM = VM
  { chunk :: !Chunk,
    ip :: VmIp,
    stack :: ![Value],
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
  return
    VM
      { chunk = ch,
        ip = ip,
        stack = [],
        memory = mem,
        varsMap = Map.empty,
        vmCalls = []
      }

-- TODO change arg order
push :: VM -> Value -> VM
push vm@(VM {stack}) val =
  vm {stack = val : stack}

pop :: VM -> (Value, VM)
pop vm@(VM {stack}) =
  ( head stack,
    vm {stack = tail stack}
  )

popMap :: (Value -> a) -> VM -> (a, VM)
popMap f vm =
  let (val, vm1) = pop vm
      res = f val
   in res `seq` (f val, vm1)

popN :: Int -> VM -> ([Value], VM)
popN 0 vm = ([], vm)
popN n vm =
  let (val, vm1) = pop vm
      (restValues, vm2) = popN (n - 1) vm1
   in (val : restValues, vm2)

peek :: Int -> VM -> Value
peek offset (VM {stack}) = stack !! offset

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
