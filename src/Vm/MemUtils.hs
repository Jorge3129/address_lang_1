{-# LANGUAGE BangPatterns #-}

module Vm.MemUtils where

import Data.List (foldl')
import qualified Data.Map as Map
import Value.Core
import Vm.State

allocN :: Int -> [Value] -> Int
allocN n values = allocHelper values 0 0
  where
    allocHelper :: [Value] -> Int -> Int -> Int
    allocHelper [] !currentIndex !count
      | count >= n = currentIndex - count
      | otherwise = error $ "Cound not allocate " ++ show n ++ " cells of memory"
    allocHelper (x : xs) !currentIndex !count
      | count >= n = currentIndex - count
      | x == NilVal = allocHelper xs (currentIndex + 1) (count + 1)
      | otherwise = allocHelper xs (currentIndex + 1) 0

allocNInit :: Int -> VM -> (Int, VM)
allocNInit n vm =
  let freeAddr = allocN n (memory vm)
      vm1 =
        foldl'
          ( \vm_ offset ->
              memSet (freeAddr + offset) 0 vm_
          )
          vm
          [0 .. (n - 1)]
   in (freeAddr, vm1)

parseList :: Value -> VM -> [Value]
parseList val vm =
  let firstAddr = asInt $ memory vm !! asInt val
   in if firstAddr == 0
        then []
        else parseList' firstAddr [] vm
  where
    parseList' :: Int -> [Value] -> VM -> [Value]
    parseList' 0 acc _ = acc
    parseList' curAddr acc vm_ =
      let nextAddr = asInt $ memory vm_ !! curAddr
          curVal = memory vm_ !! (curAddr + 1)
       in parseList' nextAddr (acc ++ [curVal]) vm_

constructList :: [Value] -> VM -> (Value, VM)
constructList [] vm =
  let (freeAddr, vm1) = allocNInit 1 vm
   in (PointerVal freeAddr, vm1)
constructList vals vm =
  let (freeAddr, vm1) = allocNInit 1 vm
      vm2 = consList' freeAddr vals vm1
   in (PointerVal freeAddr, vm2)
  where
    consList' :: Int -> [Value] -> VM -> VM
    consList' prevAddr (x : xs) vm_ =
      let newAddr = allocN 2 (memory vm_)
          vm1_ = memSet prevAddr (PointerVal newAddr) vm_
          vm2_ = memSet newAddr (PointerVal 0) vm1_
          vm3_ = memSet (newAddr + 1) x vm2_
       in consList' newAddr xs vm3_
    consList' _ [] vm_ = vm_

freeVars :: VM -> VM
freeVars vm =
  let curScope = length (vmCalls vm)
      locals = [addr | (varName, addr) <- Map.toList (varsMap vm), getVarScope varName == curScope]
   in vm {memory = [if addr `elem` locals then NilVal else val | (addr, val) <- zip [0 ..] (memory vm)]}

getRefsToAddr :: Int -> VM -> [Int]
getRefsToAddr addr vm = [i | (c, i) <- zip (memory vm) [0 :: Int ..], isPointer c && asInt c == addr]