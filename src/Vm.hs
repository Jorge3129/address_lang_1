{-# LANGUAGE NamedFieldPuns #-}

module Vm where

import ByteCode
import Data.Map (Map, empty, insert, (!))
import Data.Maybe (isJust)
import MemUtils
import MyUtils
import Value

data InterpretResult = OK | COMPILE_ERR | RUNTIME_ERR deriving (Eq, Show)

data VM = VM
  { chunk :: Chunk,
    ip :: Int,
    stack :: [Value],
    memory :: [Value],
    varsMap :: Map String Int
  }
  deriving (Eq, Show)

memMax :: Int
memMax = 1000

initVM :: Chunk -> VM
initVM ch =
  VM
    { chunk = ch,
      ip = 0,
      stack = [],
      memory = replicate memMax NilVal,
      varsMap = Data.Map.empty
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

run :: VM -> IO InterpretResult
run vm = do
  (_, Just intRes) <- untilM (\(_, res) -> isJust res) runStep (vm, Nothing)
  return intRes

runStep :: (VM, Maybe InterpretResult) -> IO (VM, Maybe InterpretResult)
runStep (vm, _) =
  let (instruction, newVm) = readByte vm
   in do
        -- print $ (toEnum instruction :: OpCode)
        (resVM, intRes) <- execInstruction (toEnum instruction) newVm
        -- print $ take 10 (memory resVM)
        -- print $ varsMap resVM
        -- print $ stack resVM
        return (resVM, intRes)

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
execInstruction OP_GREATER vm = compInstr (>) vm
execInstruction OP_LESS vm = compInstr (<) vm
execInstruction OP_EQUAL vm = compInstr (==) vm
--
execInstruction OP_PRINT vm = do
  let (val, newVm) = pop vm
  print val
  return (newVm, Nothing)
--
execInstruction OP_SEND vm = do
  let (addr, newVm) = pop vm
      (val, newVm1) = pop newVm
      newVm2 = newVm1 {memory = replace (asInt addr) val (memory newVm1)}
  return (newVm2, Nothing)
--
execInstruction OP_DEREF vm = do
  let (addr, newVm) = pop vm
      val = memory vm !! asInt addr
      newVm1 = push newVm val
  return (newVm1, Nothing)
--
execInstruction OP_NOT vm = do
  let (val, newVm) = pop vm
      newVal = IntVal $ if isFalsy val then 1 else 0
      newVm1 = push newVm $ newVal
  return (newVm1, Nothing)
--
execInstruction OP_POP vm = do
  let (_, newVm) = pop vm
  return (newVm, Nothing)
--
execInstruction OP_JUMP vm = do
  let (jumpOffset, newVm) = readByte vm
  return (addIp jumpOffset newVm, Nothing)
--
execInstruction OP_JUMP_IF_FALSE vm = do
  let (jumpOffset, newVm) = readByte vm
      newVm1 =
        if isFalsy (peek 0 vm)
          then addIp jumpOffset newVm
          else newVm
  return (newVm1, Nothing)
--
execInstruction OP_DEFINE_VAR vm = do
  let (name, vm1) = readConst vm
      (addr, vm2) = pop vm1
      vm3 =
        vm2
          { varsMap = insert (asStr name) (asInt addr) (varsMap vm2),
            memory = replace (asInt addr) 0 (memory vm2)
          }
  return (vm3, Nothing)
--
execInstruction OP_GET_VAR vm = do
  let (name, vm1) = readConst vm
      addr = varsMap vm1 ! asStr name
      vm2 = push vm1 (memory vm1 !! addr)
  return (vm2, Nothing)
--
execInstruction OP_ALLOC vm = do
  let free = IntVal $ allocNewVal (memory vm)
  return (push vm free, Nothing)
--
execInstruction instr _ = error $ "cannot run instruction " ++ show instr ++ " yet"

binaryInstr :: (Value -> Value -> Value) -> VM -> IO (VM, Maybe InterpretResult)
binaryInstr op vm = do
  let (b, newVm) = pop vm
      (a, newVm1) = pop newVm
      newVm2 = push newVm1 $ op a b
  return (newVm2, Nothing)

compInstr :: (Value -> Value -> Bool) -> VM -> IO (VM, Maybe InterpretResult)
compInstr op vm = do
  let (b, newVm) = pop vm
      (a, newVm1) = pop newVm
      newVm2 = push newVm1 $ if op a b then 1 else 0
  return (newVm2, Nothing)
