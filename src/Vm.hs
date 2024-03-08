{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vm where

import ByteCode
import Data.Maybe (isJust)
import MyUtils

data InterpretResult = OK | COMPILE_ERR | RUNTIME_ERR deriving (Eq, Show)

data VM = VM
  { chunk :: Chunk,
    ip :: Int
  }
  deriving (Eq, Show)

initVM :: Chunk -> VM
initVM ch = VM {chunk = ch, ip = 0}

run :: VM -> IO InterpretResult
run vm = do
  (_, Just intRes) <- untilM (\(_, res) -> isJust res) runStep (vm, Nothing)
  return intRes

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

runStep :: (VM, Maybe InterpretResult) -> IO (VM, Maybe InterpretResult)
runStep (vm, _) =
  let (instruction, newVm) = readByte vm
   in execInstruction (toEnum instruction) newVm

execInstruction :: OpCode -> VM -> IO (VM, Maybe InterpretResult)
execInstruction OP_RETURN vm = return (vm, Just OK)
execInstruction OP_CONSTANT vm = do
  let (val, newVm) = readConst vm
  print val
  return (newVm, Nothing)
execInstruction _ _ = undefined