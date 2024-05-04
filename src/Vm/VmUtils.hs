module Vm.VmUtils where

import Control.Exception (evaluate)
import qualified Data.Array as A
import Vm.State

returnIO' :: VM -> IO (VM, Maybe InterpretResult)
returnIO' vm = do
  res <- evaluate vm
  return (res, Nothing)

return' :: VM -> IO (VM, Maybe InterpretResult)
return' vm = do
  return (vm, Nothing)

returnOk :: VM -> IO (VM, Maybe InterpretResult)
returnOk vm = do
  return (vm, Just OK)

getCurrentLine :: VM -> IO Int
getCurrentLine vm = do
  curIp <- readIp vm
  let ch = chunk vm
  let chLines = vmcCodeLines ch
  return $ (chLines A.! curIp) + 1
