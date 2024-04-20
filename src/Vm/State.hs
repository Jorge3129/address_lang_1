{-# LANGUAGE NamedFieldPuns #-}

module Vm.State where

import ByteCode.Core
import qualified Data.Array.IO as IA
import qualified Data.Map as Map
import Data.Word (Word64)
import Value.Core
import Value.WordUtils

data VM = VM
  { chunk :: !Chunk,
    ip :: !Int,
    stack :: ![Value],
    memory :: !(IA.IOUArray Int Word64),
    varsMap :: !(Map.Map String Int),
    vmCalls :: ![(String, Int)]
  }

memMax :: Int
memMax = 5000

newMemory :: Int -> IO (IA.IOUArray Int Word64)
newMemory size = IA.newListArray (0, size - 1) (0 : replicate (size - 1) nilVal)
  where
    nilVal = valueToWord64 NilVal

initVM :: Chunk -> IO VM
initVM ch = do
  mem <- newMemory memMax
  return
    VM
      { chunk = ch,
        ip = 0,
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

addIp :: Int -> VM -> VM
addIp ipOffset vm@(VM {ip}) = vm {ip = ip + ipOffset}

readByte :: VM -> (Int, VM)
readByte vm@(VM {ip, chunk}) =
  let (Chunk {code}) = chunk
      instruction = code !! ip
      newVm = vm {ip = ip + 1}
   in (instruction, newVm)

readConst :: VM -> (Value, VM)
readConst vm@(VM {chunk}) =
  let (Chunk {constants}) = chunk
      (constPos, newVm) = readByte vm
   in (constants !! constPos, newVm)

readStr :: VM -> (String, VM)
readStr vm =
  let (val, vm1) = readConst vm
      res = asStr val
   in res `seq` (res, vm1)
