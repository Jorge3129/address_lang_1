{-# LANGUAGE BangPatterns #-}

module Vm.MemUtils where

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

parseList :: Value -> VM -> [Value]
parseList val vm =
  let firstAddr = asInt $ memory vm !! asInt val
   in if firstAddr == 0
        then []
        else parseList' firstAddr [] vm

parseList' :: Int -> [Value] -> VM -> [Value]
parseList' 0 acc _ = acc
parseList' curAddr acc vm =
  let nextAddr = asInt $ memory vm !! curAddr
      curVal = memory vm !! (curAddr + 1)
   in parseList' nextAddr (acc ++ [curVal]) vm

freeVars :: VM -> VM
freeVars vm =
  let curScope = length (vmCalls vm)
      locals = [addr | (varName, addr) <- Map.toList (varsMap vm), getVarScope varName == curScope]
   in vm {memory = [if addr `elem` locals then NilVal else val | (addr, val) <- zip [0 ..] (memory vm)]}

getRefsToAddr :: Int -> VM -> [Int]
getRefsToAddr addr vm = [i | (c, i) <- zip (memory vm) [0 :: Int ..], isPointer c && asInt c == addr]