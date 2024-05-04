module Vm.BuiltinProcs where

import Value.Core
import Vm.MemUtils
import Vm.State
import Vm.VmUtils

execBuiltinProc :: String -> VM -> IO (VM, Maybe InterpretResult)
execBuiltinProc "print" vm = do
  val <- pop vm
  print val
  returnIO' vm
--
execBuiltinProc "printList" vm = do
  val <- pop vm
  list <- parseList val vm
  print list
  returnIO' vm
--
execBuiltinProc "printRefs" vm = do
  addr <- asInt <$> pop vm
  refs <- getRefsToAddr addr vm
  putStrLn $ "Refs to " ++ show addr ++ ": " ++ show refs
  returnIO' vm
execBuiltinProc name _ = error $ "procedure " ++ name ++ " is not defined"

execBuiltinFn :: String -> Int -> VM -> IO (VM, Maybe InterpretResult)
execBuiltinFn "constrList" len vm = do
  elems <- popN len vm
  listHead <- constructList (reverse elems) vm
  push vm listHead
  return' vm
--
execBuiltinFn "ptr" _ vm = do
  oldVal <- pop vm
  push vm $ asPointer oldVal
  return' vm
--
execBuiltinFn "id" _ vm = returnIO' vm
--
execBuiltinFn "alloc" _ vm = do
  n <- asInt <$> pop vm
  freeAddr <- allocNInit n vm
  push vm $ newPtrWithSize freeAddr n
  return' vm
--
execBuiltinFn "mulalloc" _ vm = do
  count <- asInt <$> pop vm
  size <- asInt <$> pop vm
  freeAddr <- allocNInit (size * count) vm
  push vm $ PointerVal freeAddr size count
  return' vm
--
execBuiltinFn name _ _ = error $ "function " ++ name ++ " is not defined"