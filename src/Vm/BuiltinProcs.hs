module Vm.BuiltinProcs where

import Control.Monad.ST (stToIO)
import Value.Core
import Vm.MemUtils
import Vm.State
import Vm.VmUtils

execBuiltinProc :: String -> VM -> IO (VM, Maybe InterpretResult)
execBuiltinProc "print" vm = do
  val <- stToIO $ pop vm
  print val
  returnIO' vm
--
execBuiltinProc "printList" vm = do
  val <- stToIO $ pop vm
  list <- stToIO $ parseList val vm
  print list
  returnIO' vm
--
execBuiltinProc "printRefs" vm = do
  addr <- stToIO $ asInt <$> pop vm
  refs <- stToIO $ getRefsToAddr addr vm
  putStrLn $ "Refs to " ++ show addr ++ ": " ++ show refs
  returnIO' vm
execBuiltinProc _ _ = undefined

execBuiltinFn :: String -> Int -> VM -> IO (VM, Maybe InterpretResult)
execBuiltinFn "constrList" len vm = stToIO $ do
  elems <- popN len vm
  listHead <- constructList (reverse elems) vm
  push vm listHead
  return' vm
--
execBuiltinFn "ptr" _ vm = stToIO $ do
  oldVal <- pop vm
  push vm $ asPointer oldVal
  return' vm
--
execBuiltinFn "id" _ vm = returnIO' vm
--
execBuiltinFn _ _ _ = undefined