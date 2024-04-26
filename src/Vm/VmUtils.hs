module Vm.VmUtils where

import Control.Exception (evaluate)
import Control.Monad.ST (RealWorld, ST)
import Vm.State

returnIO' :: VM -> IO (VM, Maybe InterpretResult)
returnIO' vm = do
  res <- evaluate vm
  return (res, Nothing)

return' :: VM -> ST RealWorld (VM, Maybe InterpretResult)
return' vm = do
  return (vm, Nothing)

returnOk :: VM -> IO (VM, Maybe InterpretResult)
returnOk vm = do
  return (vm, Just OK)