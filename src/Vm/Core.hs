module Vm.Core where

import ByteCode.Core
import Control.Exception as Exc
import qualified Control.Monad as CM
import Control.Monad.ST (stToIO)
import Data.Maybe (isJust)
import Utils.Core
import Value.Core
import Vm.BuiltinProcs (execBuiltinFn, execBuiltinProc)
import Vm.MemUtils
import Vm.State
import Vm.VmUtils

run :: VM -> IO InterpretResult
run vm = do
  (_, Just intRes) <- untilM (\(_, res) -> isJust res) runStep (vm, Nothing)
  return intRes

runStep :: (VM, Maybe InterpretResult) -> IO (VM, Maybe InterpretResult)
runStep (vm, _) = do
  lineNum <- getCurrentLine vm
  instruction <- stToIO $ readByte vm
  (resVM, intRes) <- Exc.catch (execInstruction (toEnum instruction) vm) (handler lineNum)
  return (resVM, intRes)
  where
    handler :: Int -> ErrorCall -> IO (VM, Maybe InterpretResult)
    handler lineNum (ErrorCallWithLocation msg _) = do
      putStrLn $ "Runtime error at line " ++ show lineNum
      putStrLn msg
      return (vm, Just RUNTIME_ERR)

execReturn :: [(String, Int)] -> VM -> IO (VM, Maybe InterpretResult)
execReturn [] vm = returnOk vm
execReturn ((_, ret) : _) vm = stToIO $ do
  freeVars vm
  popCall vm
  setIp ret vm
  return' vm

execInstruction :: OpCode -> VM -> IO (VM, Maybe InterpretResult)
execInstruction OP_RETURN vm = do
  curVmCalls <- stToIO $ readCalls vm
  execReturn curVmCalls vm
--
execInstruction OP_CONSTANT vm = stToIO $ do
  val <- readConst vm
  val `seq` push vm val
  return' vm
--
execInstruction OP_ADD vm = binaryInstr (+) vm
execInstruction OP_SUB vm = binaryInstr (-) vm
execInstruction OP_MUL vm = binaryInstr (*) vm
execInstruction OP_DIV vm = binaryInstr (/) vm
execInstruction OP_MOD vm = binaryInstr (\a b -> IntVal $ asInt a `mod` asInt b) vm
--
execInstruction OP_GREATER vm = compInstr (>) vm
execInstruction OP_LESS vm = compInstr (<) vm
execInstruction OP_EQUAL vm = compInstr (==) vm
--
execInstruction OP_AND vm = logInstr (&&) vm
execInstruction OP_OR vm = logInstr (||) vm
--
execInstruction OP_PTR_ADD vm = binaryInstr ptrAdd vm
--
execInstruction OP_GET_REFS vm = stToIO $ do
  addr <- asInt <$> pop vm
  refs <- getRefsToAddr addr vm
  listHead <- constructList (map IntVal refs) vm
  push vm listHead
  return' vm
--
execInstruction OP_SEND vm = stToIO $ do
  addr <- asInt <$> pop vm
  val <- pop vm
  let destAddr = checkAddrForSend addr
  oldVal <- deref addr vm
  let castVal = castAsType oldVal val
  castVal `seq` memSet destAddr castVal vm
  return' vm
--
execInstruction OP_DEREF vm = stToIO $ do
  addr <- asInt <$> pop vm
  val <- deref addr vm
  push vm val
  return' vm
--
execInstruction OP_MUL_DEREF vm = stToIO $ do
  addrVal <- pop vm
  count <- asInt <$> pop vm
  val <- mulDeref count addrVal vm
  push vm val
  return' vm
--
execInstruction OP_EXCHANGE vm = stToIO $ do
  addrB <- asInt <$> pop vm
  addrA <- asInt <$> pop vm
  valA <- deref addrA vm
  valB <- deref addrB vm
  memSet addrA valB vm
  memSet addrB valA vm
  return' vm
--
execInstruction OP_NOT vm = stToIO $ do
  val <- pop vm
  let newVal = IntVal $ if isFalsy val then 1 else 0
  newVal `seq` push vm newVal
  return' vm
--
execInstruction OP_NEGATE vm = stToIO $ do
  val <- pop vm
  let newVal = negate val
  newVal `seq` push vm newVal
  return' vm
--
execInstruction OP_POP vm = stToIO $ do
  _ <- pop vm
  return' vm
--
execInstruction OP_JUMP vm = stToIO $ do
  jumpOffset <- readByte vm
  addIp jumpOffset vm
  return' vm
--
execInstruction OP_JUMP_IF_FALSE vm = stToIO $ do
  jumpOffset <- readByte vm
  topVal <- peek 0 vm
  CM.when (isFalsy topVal) $ addIp jumpOffset vm
  return' vm
--
execInstruction OP_DEFINE_VAR vm = stToIO $ do
  name <- readStr vm
  addr <- allocNInit 1 vm
  defineVar name addr vm
  return' vm
--
execInstruction OP_GET_VAR vm = stToIO $ do
  name <- readStr vm
  addr <- getVarAddr name vm
  val <- deref addr vm
  push vm val
  return' vm
--
execInstruction OP_SET_VAR vm = stToIO $ do
  name <- readStr vm
  addr <- getVarAddr name vm
  oldVal <- deref addr vm
  val <- pop vm
  let castVal = castAsType oldVal val
  memSet addr castVal vm
  return' vm
--
execInstruction OP_MAKE_VAR_POINTER vm = stToIO $ do
  name <- readStr vm
  addr <- getVarAddr name vm
  oldVal <- deref addr vm
  memSet addr (asPointer oldVal) vm
  return' vm
--
execInstruction OP_MAKE_POINTER vm = stToIO $ do
  addr <- asInt <$> peek 0 vm
  oldVal <- deref addr vm
  memSet addr (asPointer oldVal) vm
  return' vm
--
execInstruction OP_ALLOC vm = stToIO $ do
  freeAddr <- allocNInit 1 vm
  push vm $ IntVal freeAddr
  return' vm
--
execInstruction OP_CALL vm = stToIO $ do
  argCount <- readByte vm
  fnOffset <- asInt <$> peek argCount vm
  curIp <- readIp vm
  pushCall ("", curIp) vm -- TODO fix fnName here
  setIp fnOffset vm
  return' vm
--
execInstruction OP_CALL_FN vm = do
  fnName <- stToIO $ readStr vm
  len <- stToIO $ asInt <$> pop vm
  execBuiltinFn fnName len vm
--
execInstruction OP_CALL_PROC vm = do
  fnName <- stToIO $ readStr vm
  execBuiltinProc fnName vm
--
execInstruction instr _ = error $ "cannot run instruction " ++ show instr ++ " yet"

binaryInstr :: (Value -> Value -> Value) -> VM -> IO (VM, Maybe InterpretResult)
binaryInstr op vm = stToIO $ do
  b <- pop vm
  a <- pop vm
  let res = op a b
  res `seq` push vm res
  return' vm

compInstr :: (Value -> Value -> Bool) -> VM -> IO (VM, Maybe InterpretResult)
compInstr op vm = stToIO $ do
  b <- pop vm
  a <- pop vm
  let res = if op a b then 1 else 0
  res `seq` push vm res
  return' vm

logInstr :: (Bool -> Bool -> Bool) -> VM -> IO (VM, Maybe InterpretResult)
logInstr op vm = stToIO $ do
  b <- pop vm
  a <- pop vm
  let res = if op (isTruthy a) (isTruthy b) then 1 else 0
  res `seq` push vm res
  return' vm