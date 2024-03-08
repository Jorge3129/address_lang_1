{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vm where

import ByteCode
import Data.Maybe (isJust)
import MyUtils

data InterpretResult = OK | COMPILE_ERR | RUNTIME_ERR deriving (Eq, Show)

data VM = VM
  { chunk :: Chunk,
    ip :: Int,
    stack :: [Value]
  }
  deriving (Eq, Show)

initVM :: Chunk -> VM
initVM ch =
  VM
    { chunk = ch,
      ip = 0,
      stack = []
    }

push :: VM -> Value -> VM
push vm@(VM {stack}) val =
  vm {stack = val : stack}

pop :: VM -> (Value, VM)
pop vm@(VM {stack}) =
  ( head stack,
    vm {stack = tail stack}
  )

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
execInstruction OP_RETURN vm = do
  let (val, newVm) = pop vm
  print val
  return (newVm, Just OK)
execInstruction OP_CONSTANT vm = do
  let (val, newVm) = readConst vm
      newVm1 = push newVm val
  return (newVm1, Nothing)
execInstruction OP_ADD vm = do
  let (b, newVm) = pop vm
      (a, newVm1) = pop newVm
      newVm2 = push newVm1 (addVals a b)
  return (newVm2, Nothing)
execInstruction _ _ = undefined