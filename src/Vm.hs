{-# LANGUAGE NamedFieldPuns #-}

module Vm where

import ByteCode
import Control.Exception as Exc
import Data.Bifunctor
import Data.Map (Map, empty, insert, toList, (!))
import Data.Maybe (isJust)
import Debug
import Debug.Trace
import MemUtils
import MyUtils
import Value

data InterpretResult = OK | COMPILE_ERR | RUNTIME_ERR deriving (Eq, Show)

data VM = VM
  { chunk :: !Chunk,
    ip :: !Int,
    stack :: ![Value],
    memory :: ![Value],
    varsMap :: !(Map String Int),
    vmCalls :: ![(String, Int)]
  }
  deriving (Eq, Show)

-- data VM = VM
--   { chunk :: Chunk,
--     ip :: Int,
--     stack :: [Value],
--     memory :: [Value],
--     varsMap :: Map String Int,
--     vmCalls :: [(String, Int)]
--   }
--   deriving (Eq, Show)

memMax :: Int
memMax = 2000

initVM :: Chunk -> VM
initVM ch =
  VM
    { chunk = ch,
      ip = 0,
      stack = [],
      memory = replicate memMax NilVal,
      varsMap = Data.Map.empty,
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
runStep (vm, _) = do
  -- _ <- disassembleInstruction (chunk vm) (ip vm)
  let lineNum = getLineByOffset (ip vm) (chunk vm) + 1
  print $ getLineByOffset (ip vm) (chunk vm) + 1
  let (instruction, newVm) = readByte vm
  -- print $ (toEnum instruction :: OpCode)
  (resVM, intRes) <- Exc.catch (evaluate =<< (execInstruction (toEnum instruction) newVm)) (handler lineNum)
  -- print $ map (lpad '0' 2 . show) [0 :: Int .. 30]
  -- print $ map (lpad '0' 2 . show) (take 31 (memory resVM))
  -- print $ map (second (memory resVM !!)) (toList (varsMap resVM))
  -- print $ vmCalls resVM
  return (resVM, intRes)
  where
    handler :: Int -> ErrorCall -> IO (VM, Maybe InterpretResult)
    handler lineNum (ErrorCallWithLocation msg _) = do
      putStrLn $ "Runtime error at line " ++ show lineNum
      putStrLn msg
      return (vm, Just RUNTIME_ERR)

execInstruction :: OpCode -> VM -> IO (VM, Maybe InterpretResult)
execInstruction OP_RETURN vm@(VM {vmCalls = []}) = returnOk vm
execInstruction OP_RETURN vm@(VM {vmCalls = ((fn, ret) : calls)}) = do
  -- print $ "Returning from " ++ fn
  let vm1 = vm {vmCalls = calls, ip = ret}
  return' vm1
--
execInstruction OP_CONSTANT vm = do
  let (val, newVm) = readConst vm
      newVm1 = push newVm val
  return' newVm1
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
  return' newVm
--
execInstruction OP_PRINT_REFS vm = do
  let (val, newVm) = pop vm
      addr = asInt val
  let refs = [i | (c, i) <- zip (memory vm) [0 :: Int ..], isPointer c && asInt c == addr]
  putStrLn $ "Refs to " ++ show addr ++ ": " ++ show refs
  return' newVm
--
execInstruction OP_SEND vm = do
  let (addr, vm1) = pop vm
      (val, vm2) = pop vm1
      vm3 = vm2 {memory = replace (asInt addr) val (memory vm2)}
  return' vm3
--
execInstruction OP_DEREF vm = do
  let (addr, newVm) = pop vm
      val = memory vm !! asInt addr
      newVm1 = push newVm val
  return' newVm1
--
execInstruction OP_NOT vm = do
  let (val, newVm) = pop vm
      newVal = IntVal $ if isFalsy val then 1 else 0
      newVm1 = push newVm newVal
  return' newVm1
--
execInstruction OP_POP vm = do
  let (_, newVm) = pop vm
  return' newVm
--
execInstruction OP_JUMP vm = do
  let (jumpOffset, newVm) = readByte vm
  return' $ addIp jumpOffset newVm
--
execInstruction OP_JUMP_IF_FALSE vm = do
  let (jumpOffset, newVm) = readByte vm
      newVm1 =
        if isFalsy (peek 0 vm)
          then addIp jumpOffset newVm
          else newVm
  return' newVm1
--
execInstruction OP_DEFINE_VAR vm = do
  let (name, vm1) = readConst vm
      (addr, vm2) = pop vm1
      vm3 = vm2 {varsMap = insert (asStr name) (asInt addr) (varsMap vm2)}
  return' vm3
-- memory = replace (asInt addr) 0 (memory vm2)
--
execInstruction OP_GET_VAR vm = do
  let (name, vm1) = readConst vm
      addr = varsMap vm1 ! asStr name
      vm2 = push vm1 (memory vm1 !! addr)
  return' vm2
--
execInstruction OP_SET_VAR vm = do
  let (name, vm1) = readConst vm
      addr = varsMap vm1 ! asStr name
      oldVal = memory vm1 !! addr
      (val, vm2) = pop vm1
      castVal = if isPointer oldVal then asPointer val else val
      vm3 = vm2 {memory = replace addr castVal (memory vm2)}
  return' vm3
--
execInstruction OP_SET_POINTER vm = do
  let (name, vm1) = readConst vm
      addr = varsMap vm1 ! asStr name
      oldVal = memory vm1 !! addr
      vm2 = vm1 {memory = replace addr (asPointer oldVal) (memory vm1)}
  return' vm2
--
execInstruction OP_MAKE_POINTER vm = do
  let addr = asInt (peek 0 vm)
      oldVal = memory vm !! addr
      vm1 = vm {memory = replace addr (asPointer oldVal) (memory vm)}
  return' vm1
--
execInstruction OP_ALLOC vm = do
  let free = allocNewVal (memory vm)
      vm1 = vm {memory = replace free 0 (memory vm)}
  return' $ push vm1 (IntVal free)
--
execInstruction OP_CALL vm = do
  let (name, vm1) = readConst vm
      fnName = asStr name
  let jumpTo = chLabelMap (chunk vm1) ! fnName
      vm2 = vm1 {vmCalls = (fnName, ip vm1) : vmCalls vm}
      vm3 = vm2 {ip = jumpTo}
  return' vm3
--
execInstruction instr _ = error $ "cannot run instruction " ++ show instr ++ " yet"

binaryInstr :: (Value -> Value -> Value) -> VM -> IO (VM, Maybe InterpretResult)
binaryInstr op vm = do
  let (b, newVm) = pop vm
      (a, newVm1) = pop newVm
      res = op a b
      newVm2 = res `seq` push newVm1 res
  return' newVm2

compInstr :: (Value -> Value -> Bool) -> VM -> IO (VM, Maybe InterpretResult)
compInstr op vm = do
  let (b, newVm) = pop vm
      (a, newVm1) = pop newVm
      res = if op a b then 1 else 0
      newVm2 = res `seq` push newVm1 res
  return' newVm2

return' :: VM -> IO (VM, Maybe InterpretResult)
return' vm = do
  res <- evaluate vm
  return (res, Nothing)

returnOk :: VM -> IO (VM, Maybe InterpretResult)
returnOk vm = do
  res <- evaluate vm
  return (res, Just OK)