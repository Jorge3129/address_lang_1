module Vm.Core where

import ByteCode.Core
import Control.Exception
import qualified Control.Monad as CM
import Data.Maybe (isJust)
import Utils.Core
import Value.Core
import Vm.BuiltinProcs (execBuiltinFn, execBuiltinProc)
import Vm.MemUtils
import Vm.State
import Vm.VmUtils

initAndRunVm :: Chunk -> IO ()
initAndRunVm ch = do
  vm <- initVM ch
  _ <- runVm vm
  return ()

runVm :: VM -> IO InterpretResult
runVm vm = do
  (Just intRes) <- untilM isJust runStep Nothing
  return intRes
  where
    runStep _ = do
      instr <- toEnum <$> readByte vm
      execInstruction instr vm `catch` handler
      where
        handler (ErrorCallWithLocation msg _) = do
          lineNum <- getCurrentLine vm
          putStrLn $ "Runtime error at line " ++ show lineNum
          putStrLn msg
          return $ Just RUNTIME_ERR

execInstruction :: OpCode -> VM -> IO (Maybe InterpretResult)
execInstruction OP_CONSTANT vm = do
  val <- readConst vm
  val `seq` push vm val
  return'
--
execInstruction OP_ADD vm = binaryInstr (+) vm
execInstruction OP_SUB vm = binaryInstr (-) vm
execInstruction OP_MUL vm = binaryInstr (*) vm
execInstruction OP_DIV vm = binaryInstr (/) vm
execInstruction OP_MOD vm = binaryInstr valueMod vm
execInstruction OP_PTR_ADD vm = binaryInstr ptrAdd vm
--
execInstruction OP_GREATER vm = compInstr (>) vm
execInstruction OP_LESS vm = compInstr (<) vm
execInstruction OP_EQUAL vm = compInstr (==) vm
--
execInstruction OP_AND vm = logInstr (&&) vm
execInstruction OP_OR vm = logInstr (||) vm
--
execInstruction OP_NOT vm = unaryInstr valueNot vm
execInstruction OP_NEGATE vm = unaryInstr negate vm
--
execInstruction OP_DEREF vm = do
  addr <- asInt <$> pop vm
  val <- memRead addr vm
  push vm val
  return'
--
execInstruction OP_MUL_DEREF vm = do
  addrVal <- pop vm
  count <- asInt <$> pop vm
  val <- mulDeref count addrVal vm
  push vm val
  return'
--
execInstruction OP_MIN_DEREF vm = do
  addr <- asInt <$> pop vm
  count <- asInt <$> pop vm
  listHead <- minDeref count addr vm
  push vm listHead
  return'
--
execInstruction OP_SEND vm = do
  addr <- asInt <$> pop vm
  val <- pop vm
  memWrite (checkAddrForSend addr) val vm
  return'
--
execInstruction OP_EXCHANGE vm = do
  addrB <- asInt <$> pop vm
  addrA <- asInt <$> pop vm
  valA <- memRead addrA vm
  valB <- memRead addrB vm
  memWrite addrA valB vm
  memWrite addrB valA vm
  return'
--
execInstruction OP_POP vm = do
  _ <- pop vm
  return'
--
execInstruction OP_JUMP vm = do
  jumpOffset <- readByte vm
  addIp jumpOffset vm
  return'
--
execInstruction OP_JUMP_IF_FALSE vm = do
  jumpOffset <- readByte vm
  topVal <- peek 0 vm
  CM.when (isFalsy topVal) $ addIp jumpOffset vm
  return'
--
execInstruction OP_DEFINE_VAR vm = do
  name <- readStr vm
  addr <- allocNInit 1 vm
  defineVar name addr vm
  return'
--
execInstruction OP_GET_VAR vm = do
  name <- readStr vm
  val <- readVar name vm
  push vm val
  return'
--
execInstruction OP_SET_VAR vm = do
  name <- readStr vm
  addr <- getVarAddr name vm
  oldVal <- memRead addr vm
  val <- pop vm
  let castVal = castAsType oldVal val
  memWrite addr castVal vm
  return'
--
execInstruction OP_ALLOC vm = do
  freeAddr <- allocNInit 1 vm
  push vm $ IntVal freeAddr
  return'
--
execInstruction OP_CALL vm = do
  argCount <- readByte vm
  fnOffset <- asInt <$> peek argCount vm
  curIp <- readIp vm
  pushCall (fnOffset, curIp) vm
  setIp fnOffset vm
  return'
--
execInstruction OP_CALL_FN vm = do
  fnName <- readStr vm
  len <- asInt <$> pop vm
  execBuiltinFn fnName len vm
--
execInstruction OP_CALL_PROC vm = do
  fnName <- readStr vm
  execBuiltinProc fnName vm
--
execInstruction OP_RETURN vm = do
  curVmCalls <- readCalls vm
  execReturn curVmCalls vm

execReturn :: [VmCallFrame] -> VM -> IO (Maybe InterpretResult)
execReturn [] _ = returnOk
execReturn ((_, returnAddr) : _) vm = do
  freeVars vm
  popCall vm
  setIp returnAddr vm
  return'

binaryInstr :: (Value -> Value -> Value) -> VM -> IO (Maybe InterpretResult)
binaryInstr op vm = do
  b <- pop vm
  a <- pop vm
  let res = op a b
  res `seq` push vm res
  return'

unaryInstr :: (Value -> Value) -> VM -> IO (Maybe InterpretResult)
unaryInstr op vm = do
  a <- pop vm
  let res = op a
  res `seq` push vm res
  return'

compInstr :: (Value -> Value -> Bool) -> VM -> IO (Maybe InterpretResult)
compInstr op vm = do
  b <- pop vm
  a <- pop vm
  let res = if op a b then 1 else 0
  res `seq` push vm res
  return'

logInstr :: (Bool -> Bool -> Bool) -> VM -> IO (Maybe InterpretResult)
logInstr op vm = do
  b <- pop vm
  a <- pop vm
  let res = if op (isTruthy a) (isTruthy b) then 1 else 0
  res `seq` push vm res
  return'