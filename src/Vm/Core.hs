{-# LANGUAGE NamedFieldPuns #-}

module Vm.Core where

import ByteCode.Core
import Control.Exception as Exc
import Data.Bifunctor
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Debug
import MyUtils
import Value.Core
import Vm.MemUtils
import Vm.State

data InterpretResult = OK | COMPILE_ERR | RUNTIME_ERR deriving (Eq, Show)

run :: VM -> IO InterpretResult
run vm = do
  (_, Just intRes) <- untilM (\(_, res) -> isJust res) runStep (vm, Nothing)
  return intRes

runStep :: (VM, Maybe InterpretResult) -> IO (VM, Maybe InterpretResult)
runStep (vm, _) = do
  -- _ <- disassembleInstruction (chunk vm) (ip vm)
  let lineNum = getLineByOffset (ip vm) (chunk vm) + 1
  -- print $ getLineByOffset (ip vm) (chunk vm) + 1
  let (instruction, newVm) = readByte vm
  -- print $ (toEnum instruction :: OpCode)
  (resVM, intRes) <- Exc.catch (execInstruction (toEnum instruction) newVm) (handler lineNum)
  -- print $ map (lpad '0' 2 . show) [0 :: Int .. 30]
  -- print $ map (lpad '0' 2 . show) (take 31 (memory resVM))
  -- print $ map (second (memory resVM !!)) (Map.toList (varsMap resVM))
  -- print $ vmCalls resVM
  -- print $ stack resVM
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
  let vmFreed = freeVars vm
      vm1 = vmFreed {vmCalls = calls, ip = ret}
  return' vm1
--
execInstruction OP_CONSTANT vm = do
  let (val, newVm) = readConst vm
      newVm1 = val `seq` push newVm val
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
execInstruction OP_PRINT_LIST vm = do
  let (val, newVm) = pop vm
      list = parseList val vm
  print list
  return' newVm
--
execInstruction OP_PRINT_REFS vm = do
  let (addr, newVm) = popMap asInt vm
  let refs = getRefsToAddr addr vm
  putStrLn $ "Refs to " ++ show addr ++ ": " ++ show refs
  return' newVm
--
execInstruction OP_GET_REFS vm = do
  let (addr, vm1) = popMap asInt vm
  let refs = getRefsToAddr addr vm
  let (listHead, vm2) = constructList (map IntVal refs) vm1
  return' $ push vm2 listHead
--
execInstruction OP_SEND vm = do
  let (addr, vm1) = popMap asInt vm
      (val, vm2) = pop vm1
      oldVal = memory vm2 !! addr
      castVal = if isPointer oldVal then asPointer val else val
      destAddr =
        if addr > 0
          then addr
          else error $ "Cannot send to memory at " ++ show addr
      vm3 = destAddr `seq` memSet destAddr castVal vm2
  return' vm3
--
execInstruction OP_DEREF vm = do
  let (addr, newVm) = popMap asInt vm
      val =
        if addr > 0
          then memory vm !! addr
          else error $ "Cannot dereference memory at " ++ show addr
      newVm1 = val `seq` push newVm val
  return' newVm1
--
execInstruction OP_EXCHANGE vm = do
  let (addrB, vm1) = popMap asInt vm
      (addrA, vm2) = popMap asInt vm1
      valA = memory vm2 !! addrA
      valB = memory vm2 !! addrB
      vm3 = valB `seq` memSet addrA valB vm2
      vm4 = valA `seq` memSet addrB valA vm3
  return' vm4
--
execInstruction OP_NOT vm = do
  let (val, newVm) = pop vm
      newVal = IntVal $ if isFalsy val then 1 else 0
      newVm1 = newVal `seq` push newVm newVal
  return' newVm1
--
execInstruction OP_NEGATE vm = do
  let (val, newVm) = pop vm
      newVal = negate val
      newVm1 = newVal `seq` push newVm newVal
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
  let (name, vm1) = readStr vm
      (addr, vm2) = popMap asInt vm1
      vm3 = vm2 {varsMap = Map.insert (scopedVar vm name) addr (varsMap vm2)}
  return' vm3
--
execInstruction OP_GET_VAR vm = do
  let (name, vm1) = readStr vm
      addr = varsMap vm1 Map.! scopedVar vm name
      val = memory vm1 !! addr
  return' $ push vm1 val
--
execInstruction OP_SET_VAR vm = do
  let (name, vm1) = readStr vm
      addr = varsMap vm1 Map.! scopedVar vm name
      oldVal = memory vm1 !! addr
      (val, vm2) = pop vm1
      castVal = if isPointer oldVal then asPointer val else val
  return' $ memSet addr castVal vm2
--
execInstruction OP_MAKE_VAR_POINTER vm = do
  let (name, vm1) = readStr vm
      addr = varsMap vm1 Map.! scopedVar vm name
      oldVal = memory vm1 !! addr
  return' $ memSet addr (asPointer oldVal) vm1
--
execInstruction OP_MAKE_POINTER vm = do
  let addr = asInt (peek 0 vm)
      oldVal = memory vm !! addr
  return' $ memSet addr (asPointer oldVal) vm
--
execInstruction OP_ALLOC vm = do
  let (freeAddr, vm1) = allocNInit 1 vm
  return' $ push vm1 (IntVal freeAddr)
--
execInstruction OP_ALLOC_N vm = do
  let (n, vm1) = popMap asInt vm
      (freeAddr, vm2) = allocNInit n vm1
  return' $ push vm2 (PointerVal freeAddr)
--
execInstruction OP_CALL vm = do
  let (fnName, vm1) = readStr vm
  let jumpTo = chLabelMap (chunk vm1) Map.! fnName
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