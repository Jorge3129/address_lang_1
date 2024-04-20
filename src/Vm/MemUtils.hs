{-# LANGUAGE BangPatterns #-}

module Vm.MemUtils where

import Control.Monad (foldM, forM, forM_)
import qualified Data.Array.IO as IA
import Data.List (foldl')
import qualified Data.Map as Map
import MyUtils (lpad)
import Value.Core
import Vm.State

allocN :: Int -> VMMemory -> IO Int
allocN n valuesRaw = do
  values <- IA.getElems valuesRaw
  return $ allocHelper values 0 0
  where
    allocHelper :: [Value] -> Int -> Int -> Int
    allocHelper [] !currentIndex !count
      | count >= n = currentIndex - count
      | otherwise = error $ "Cound not allocate " ++ show n ++ " cells of memory"
    allocHelper (x : xs) !currentIndex !count
      | count >= n = currentIndex - count
      | x == NilVal = allocHelper xs (currentIndex + 1) (count + 1)
      | otherwise = allocHelper xs (currentIndex + 1) 0

allocNInit :: Int -> VM -> IO (Int, VM)
allocNInit n vm = do
  freeAddr <- allocN n (memory vm)
  vm1 <-
    foldM
      ( \vm_ offset ->
          memSet (freeAddr + offset) 0 vm_ >> return vm_
      )
      vm
      [0 .. (n - 1)]
  return (freeAddr, vm1)

parseList :: Value -> VM -> IO [Value]
parseList val vm = do
  firstAddr <- asInt <$> deref (asInt val) vm
  if firstAddr == 0
    then return []
    else parseList' firstAddr [] vm
  where
    parseList' :: Int -> [Value] -> VM -> IO [Value]
    parseList' 0 acc _ = return acc
    parseList' curAddr acc vm_ = do
      nextAddr <- deref curAddr vm_
      curVal <- deref (curAddr + 1) vm_
      parseList' (asInt nextAddr) (acc ++ [curVal]) vm_

constructList :: [Value] -> VM -> IO (Value, VM)
constructList [] vm = do
  (freeAddr, vm1) <- allocNInit 1 vm
  return (PointerVal freeAddr, vm1)
constructList vals vm = do
  (freeAddr, vm1) <- allocNInit 1 vm
  vm2 <- consList' freeAddr vals vm1
  return (PointerVal freeAddr, vm2)
  where
    consList' :: Int -> [Value] -> VM -> IO VM
    consList' prevAddr (x : xs) vm_ = do
      newAddr <- allocN 2 (memory vm_)
      memSet prevAddr (PointerVal newAddr) vm_
      memSet newAddr (PointerVal 0) vm_
      memSet (newAddr + 1) x vm_
      consList' newAddr xs vm_
    consList' _ [] vm_ = return vm_

freeVars :: VM -> IO VM
freeVars vm = do
  let curScope = length (vmCalls vm)
      locals = [(varName, addr) | (varName, addr) <- Map.toList (varsMap vm), getVarScope varName == curScope]
      localAddrs = map snd locals
  forM_ localAddrs $ \addr -> do
    IA.writeArray (memory vm) addr NilVal
  return
    vm
      { varsMap = foldl' (flip Map.delete) (varsMap vm) (map fst locals)
      }

getRefsToAddr :: Int -> VM -> IO [Int]
getRefsToAddr addr vm = do
  bounds <- IA.getBounds (memory vm)
  let (low, high) = bounds
  indexes <- forM [low + 1 .. high] $ \i -> do
    val <- IA.readArray (memory vm) i
    if isPointer val && asInt val == addr
      then return i
      else return (-1)
  return $ filter (/= -1) indexes

deref :: Int -> VM -> IO Value
deref addr vm
  | addr > 0 = IA.readArray (memory vm) addr
  | otherwise = error $ "Cannot dereference memory at " ++ show addr

mulDeref :: Int -> Value -> VM -> IO Value
mulDeref count addrVal vm
  | count < 0 = error $ "Cannot execute multi-stroke operation for a negative number " ++ show count
  | count == 0 = return addrVal
  | otherwise = do
      addr <- deref (asInt addrVal) vm
      mulDeref (count - 1) addr vm

checkAddrForSend :: Int -> Int
checkAddrForSend addr
  | addr > 0 = addr
  | otherwise = error $ "Cannot send to memory at " ++ show addr

castAsType :: Value -> Value -> Value
castAsType oldVal val
  | isPointer oldVal = asPointer val
  | otherwise = val

memSet :: Int -> Value -> VM -> IO ()
memSet addr val vm
  | addr >= 0 = IA.writeArray (memory vm) addr val
  | otherwise = error $ "Cannot set memory at address " ++ show addr

scopedVar :: VM -> String -> String
scopedVar vm name =
  let scope = lpad '0' 8 $ show $ length $ vmCalls vm
   in scope ++ name

getVarScope :: String -> Int
getVarScope s = read $ take 8 s