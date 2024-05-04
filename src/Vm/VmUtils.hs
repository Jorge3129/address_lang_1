module Vm.VmUtils where

import qualified Data.Array as A
import Vm.State

return' :: IO (Maybe InterpretResult)
return' = return Nothing

returnOk :: IO (Maybe InterpretResult)
returnOk = return $ Just OK

getCurrentLine :: VM -> IO Int
getCurrentLine vm = do
  curIp <- readIp vm
  let ch = chunk vm
  let chLines = vmcCodeLines ch
  return $ (chLines A.! curIp) + 1
