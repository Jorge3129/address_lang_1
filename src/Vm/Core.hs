module Vm.Core where

import ByteCode.Core
import Control.Exception as Exc
import qualified Control.Monad as CM
import Data.Bifunctor
import Data.List (foldl')
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
  curIp <- readIp vm
  let lineNum = getLineByOffset curIp (chunk vm) + 1
  -- print $ getLineByOffset (ip vm) (chunk vm) + 1
  instruction <- readByte vm
  -- print $ (toEnum instruction :: OpCode)
  (resVM, intRes) <- Exc.catch (execInstruction (toEnum instruction) vm) (handler lineNum)
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
execInstruction OP_RETURN vm@(VM {vmCalls = ((_, ret) : calls)}) = do
  freeVars vm
  let vm1 = vm {vmCalls = calls}
  setIp ret vm1
  return' vm1
--
execInstruction OP_CONSTANT vm = do
  val <- readConst vm
  val `seq` push vm val
  return' vm
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
execInstruction OP_AND vm = logInstr (&&) vm
execInstruction OP_OR vm = logInstr (||) vm
--
execInstruction OP_PRINT vm = do
  val <- pop vm
  print val
  return' vm
--
execInstruction OP_PRINT_LIST vm = do
  val <- pop vm
  list <- parseList val vm
  print list
  return' vm
--
execInstruction OP_PRINT_REFS vm = do
  addr <- asInt <$> pop vm
  refs <- getRefsToAddr addr vm
  putStrLn $ "Refs to " ++ show addr ++ ": " ++ show refs
  return' vm
--
execInstruction OP_GET_REFS vm = do
  addr <- asInt <$> pop vm
  refs <- getRefsToAddr addr vm
  listHead <- constructList (map IntVal refs) vm
  push vm listHead
  return' vm
--
execInstruction OP_CONSTR_LIST vm = do
  len <- asInt <$> pop vm
  elems <- popN len vm
  listHead <- constructList (reverse elems) vm
  push vm listHead
  return' vm
--
execInstruction OP_SEND vm = do
  addr <- asInt <$> pop vm
  val <- pop vm
  let destAddr = checkAddrForSend addr
  oldVal <- deref addr vm
  let castVal = castAsType oldVal val
  castVal `seq` memSet destAddr castVal vm
  return' vm
--
execInstruction OP_DEREF vm = do
  addr <- asInt <$> pop vm
  val <- deref addr vm
  push vm val
  return' vm
--
execInstruction OP_MUL_DEREF vm = do
  addrVal <- pop vm
  count <- asInt <$> pop vm
  val <- mulDeref count addrVal vm
  push vm val
  return' vm
--
execInstruction OP_EXCHANGE vm = do
  addrB <- asInt <$> pop vm
  addrA <- asInt <$> pop vm
  valA <- deref addrA vm
  valB <- deref addrB vm
  memSet addrA valB vm
  memSet addrB valA vm
  return' vm
--
execInstruction OP_NOT vm = do
  val <- pop vm
  let newVal = IntVal $ if isFalsy val then 1 else 0
  newVal `seq` push vm newVal
  return' vm
--
execInstruction OP_NEGATE vm = do
  val <- pop vm
  let newVal = negate val
  newVal `seq` push vm newVal
  return' vm
--
execInstruction OP_POP vm = do
  _ <- pop vm
  return' vm
--
execInstruction OP_JUMP vm = do
  jumpOffset <- readByte vm
  addIp jumpOffset vm
  return' vm
--
execInstruction OP_JUMP_IF_FALSE vm = do
  jumpOffset <- readByte vm
  topVal <- peek 0 vm
  CM.when (isFalsy topVal) $ addIp jumpOffset vm
  return' vm
--
execInstruction OP_DEFINE_VAR vm = do
  name <- readStr vm
  addr <- asInt <$> pop vm
  defineVar name addr vm
  return' vm
--
execInstruction OP_GET_VAR vm = do
  name <- readStr vm
  addr <- getVarAddr name vm
  val <- deref addr vm
  push vm val
  return' vm
--
execInstruction OP_SET_VAR vm = do
  name <- readStr vm
  addr <- getVarAddr name vm
  oldVal <- deref addr vm
  val <- pop vm
  let castVal = castAsType oldVal val
  memSet addr castVal vm
  return' vm
--
execInstruction OP_MAKE_VAR_POINTER vm = do
  name <- readStr vm
  addr <- getVarAddr name vm
  oldVal <- deref addr vm
  memSet addr (asPointer oldVal) vm
  return' vm
--
execInstruction OP_MAKE_POINTER vm = do
  addr <- asInt <$> peek 0 vm
  oldVal <- deref addr vm
  memSet addr (asPointer oldVal) vm
  return' vm
--
execInstruction OP_CAST_AS_PTR vm = do
  oldVal <- pop vm
  push vm $ asPointer oldVal
  return' vm
--
execInstruction OP_ALLOC vm = do
  freeAddr <- allocNInit 1 vm
  push vm $ IntVal freeAddr
  return' vm
--
execInstruction OP_ALLOC_N vm = do
  n <- asInt <$> pop vm
  freeAddr <- allocNInit n vm
  push vm $ PointerVal freeAddr
  return' vm
--
execInstruction OP_CALL vm = do
  fnName <- readStr vm
  let jumpTo = chLabelMap (chunk vm) Map.! fnName
  curIp <- readIp vm
  let vm1 = vm {vmCalls = (fnName, curIp) : vmCalls vm}
  setIp jumpTo vm1
  return' vm1
--
execInstruction instr _ = error $ "cannot run instruction " ++ show instr ++ " yet"

binaryInstr :: (Value -> Value -> Value) -> VM -> IO (VM, Maybe InterpretResult)
binaryInstr op vm = do
  b <- pop vm
  a <- pop vm
  let res = op a b
  res `seq` push vm res
  return' vm

compInstr :: (Value -> Value -> Bool) -> VM -> IO (VM, Maybe InterpretResult)
compInstr op vm = do
  b <- pop vm
  a <- pop vm
  let res = if op a b then 1 else 0
  res `seq` push vm res
  return' vm

logInstr :: (Bool -> Bool -> Bool) -> VM -> IO (VM, Maybe InterpretResult)
logInstr op vm = do
  b <- pop vm
  a <- pop vm
  let res = if op (isTruthy a) (isTruthy b) then 1 else 0
  res `seq` push vm res
  return' vm

return' :: VM -> IO (VM, Maybe InterpretResult)
return' vm = do
  res <- evaluate vm
  return (res, Nothing)

return'' :: IO VM -> IO (VM, Maybe InterpretResult)
return'' vm = do
  res <- vm
  return (res, Nothing)

returnOk :: VM -> IO (VM, Maybe InterpretResult)
returnOk vm = do
  res <- evaluate vm
  return (res, Just OK)