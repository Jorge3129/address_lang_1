module Vm.BuiltinProcs where

import Value.Core
import Vm.MemUtils
import Vm.State
import Vm.VmUtils

execBuiltinProc :: String -> VM -> IO (Maybe InterpretResult)
execBuiltinProc "print" vm = do
  val <- pop vm
  print val
  return'
--
execBuiltinProc "printList" vm = do
  val <- pop vm
  list <- parseList val vm
  print list
  return'
--
execBuiltinProc "printRefs" vm = do
  addr <- asInt <$> pop vm
  refs <- getRefsToAddr addr vm
  putStrLn $ "Refs to " ++ show addr ++ ": " ++ show refs
  return'
execBuiltinProc name _ = error $ "procedure " ++ name ++ " is not defined"

execBuiltinFn :: String -> Int -> VM -> IO (Maybe InterpretResult)
execBuiltinFn "constrList" len vm = do
  elems <- popN len vm
  listHead <- constructList (reverse elems) vm
  push vm listHead
  return'
--
execBuiltinFn "ptr" _ vm = unaryFn asPointer vm
--
execBuiltinFn "int" _ vm = unaryFn (IntVal . asInt) vm
--
execBuiltinFn "abs" _ vm = unaryFn abs vm
--
execBuiltinFn "signum" _ vm = unaryFn signum vm
--
execBuiltinFn "id" _ _ = return'
--
execBuiltinFn "alloc" _ vm = do
  n <- asInt <$> pop vm
  freeAddr <- allocNInit n vm
  push vm $ newPtrWithSize freeAddr n
  return'
--
execBuiltinFn "mulalloc" _ vm = do
  count <- asInt <$> pop vm
  size <- asInt <$> pop vm
  freeAddr <- allocNInit (size * count) vm
  push vm $ PointerVal freeAddr size count
  return'
--
execBuiltinFn name _ _ = error $ "function " ++ name ++ " is not defined"

unaryFn :: (Value -> Value) -> VM -> IO (Maybe InterpretResult)
unaryFn fn vm = do
  oldVal <- pop vm
  let res = fn oldVal
  res `seq` push vm res
  return'