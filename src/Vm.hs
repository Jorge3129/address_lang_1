{-# LANGUAGE NamedFieldPuns #-}

module Vm where

import ByteCode
import Data.Maybe (isJust)
import MyUtils
import Value

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
  return (vm, Just OK)
--
execInstruction OP_CONSTANT vm = do
  let (val, newVm) = readConst vm
      newVm1 = push newVm val
  return (newVm1, Nothing)
--
execInstruction OP_ADD vm = binaryInstr (+) vm
execInstruction OP_SUB vm = binaryInstr (-) vm
execInstruction OP_MUL vm = binaryInstr (*) vm
execInstruction OP_DIV vm = binaryInstr (/) vm
--
execInstruction OP_PRINT vm = do
  let (val, newVm) = pop vm
  print val
  return (newVm, Nothing)
--
execInstruction OP_NOT vm = do
  let (val, newVm) = pop vm
      newVal = DoubleVal $ if asNum val == 0 then 1 else 0
      newVm1 = push newVm $ newVal
  return (newVm1, Nothing)
--
execInstruction OP_POP vm = do
  let (_, newVm) = pop vm
  return (newVm, Nothing)
execInstruction _ _ = undefined

binaryInstr :: (Value -> Value -> Value) -> VM -> IO (VM, Maybe InterpretResult)
binaryInstr op vm = do
  let (b, newVm) = pop vm
      (a, newVm1) = pop newVm
      newVm2 = push newVm1 $ op a b
  return (newVm2, Nothing)