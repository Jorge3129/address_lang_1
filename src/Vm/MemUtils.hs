module Vm.MemUtils where

import Control.Monad (forM, forM_)
import Control.Monad.Loops (concatM)
import Control.Monad.ST (RealWorld, ST)
import qualified Data.Array.ST as SA
import qualified Data.Map as Map
import Data.STRef (modifySTRef, readSTRef)
import Utils.Core (lpad)
import Value.Core
import Vm.State

allocN :: Int -> VmMemory -> ST RealWorld Int
allocN n vmMemory = do
  (low, high) <- SA.getBounds vmMemory
  allocHelper 0 (low + 1) high
  where
    allocHelper :: Int -> Int -> Int -> ST RealWorld Int
    allocHelper count currentIndex endIndex
      | currentIndex > endIndex = error $ "Cound not allocate " ++ show n ++ " cells of memory"
      | count >= n = return $ currentIndex - count
      | otherwise = do
          curVal <- SA.readArray vmMemory currentIndex
          if curVal == NilVal
            then allocHelper (count + 1) (currentIndex + 1) endIndex
            else allocHelper 0 (currentIndex + 1) endIndex

allocNInit :: Int -> VM -> ST RealWorld Int
allocNInit n vm = do
  freeAddr <- allocN n (memory vm)
  forM_ [0 .. (n - 1)] $ \offset ->
    memSet (freeAddr + offset) 0 vm
  return freeAddr

parseList :: Value -> VM -> ST RealWorld [Value]
parseList val vm = do
  firstAddr <- asInt <$> deref (asInt val) vm
  if firstAddr == 0
    then return []
    else parseList' firstAddr [] vm
  where
    parseList' :: Int -> [Value] -> VM -> ST RealWorld [Value]
    parseList' 0 acc _ = return acc
    parseList' curAddr acc vm_ = do
      nextAddr <- deref curAddr vm_
      curVal <- deref (curAddr + 1) vm_
      parseList' (asInt nextAddr) (acc ++ [curVal]) vm_

constructList :: [Value] -> VM -> ST RealWorld Value
constructList [] vm = newPtr <$> allocNInit 1 vm
constructList vals vm = do
  freeAddr <- allocNInit 1 vm
  consList' freeAddr vals
  return $ newPtr freeAddr
  where
    consList' :: Int -> [Value] -> ST RealWorld ()
    consList' prevAddr (x : xs) = do
      let sz = 2 :: Int
      newAddr <- allocN sz (memory vm)
      memSet prevAddr (newPtrWithSize newAddr sz) vm
      memSet newAddr (newPtr 0) vm
      memSet (newAddr + 1) x vm
      consList' newAddr xs
    consList' _ [] = return ()

defineVar :: String -> Int -> VM -> ST RealWorld ()
defineVar nm addr vm = do
  curScope <- length <$> readCalls vm
  modifySTRef (varsMap vm) (Map.insert (scopedVar curScope nm) addr)

getVarAddr :: String -> VM -> ST RealWorld Int
getVarAddr name vm = do
  curScope <- length <$> readCalls vm
  (Map.! scopedVar curScope name) <$> readSTRef (varsMap vm)

getLocalVars :: VM -> ST RealWorld [(String, Int)]
getLocalVars vm = do
  curScope <- length <$> readCalls vm
  allVars <- Map.toList <$> readSTRef (varsMap vm)
  return $ [(nm, add) | (nm, add) <- allVars, getVarScope nm == curScope]

freeVars :: VM -> ST RealWorld ()
freeVars vm = do
  locals <- getLocalVars vm
  forM_ locals $ \(nm, addr) -> do
    modifySTRef (varsMap vm) (Map.delete nm)
    SA.writeArray (memory vm) addr NilVal

getRefsToAddr :: Int -> VM -> ST RealWorld [Int]
getRefsToAddr addr vm = do
  (low, high) <- SA.getBounds (memory vm)
  indexes <- forM [low + 1 .. high] $ \i -> do
    val <- SA.readArray (memory vm) i
    if isPointer val && asInt val == addr
      then return i
      else return (-1)
  return $ filter (/= -1) indexes

deref :: Int -> VM -> ST RealWorld Value
deref addr vm
  | addr > 0 = SA.readArray (memory vm) addr
  | otherwise = error $ "Cannot dereference memory at " ++ show addr

mulDeref :: Int -> Value -> VM -> ST RealWorld Value
mulDeref count addrVal vm
  | count < 0 = error $ "Cannot execute multiple dereference operation for a negative number " ++ show count
  | count == 0 = return addrVal
  | otherwise = do
      addr <- deref (asInt addrVal) vm
      mulDeref (count - 1) addr vm

minDeref :: Int -> Value -> VM -> ST RealWorld Value
minDeref count startAddrVal vm
  | count <= 0 = error $ "Cannot execute minus dereference operation for a non-positive number " ++ show count
  | otherwise = do
      refs <- minDeref' count [asInt startAddrVal] vm
      constructList (map IntVal refs) vm

minDeref' :: Int -> [Int] -> VM -> ST RealWorld [Int]
minDeref' count addrs vm
  | count <= 0 = return addrs
  | otherwise = do
      refs <- concat <$> mapM (`getRefsToAddr` vm) addrs
      minDeref' (count - 1) refs vm

checkAddrForSend :: Int -> Int
checkAddrForSend addr
  | addr > 0 = addr
  | otherwise = error $ "Cannot send to memory at " ++ show addr

castAsType :: Value -> Value -> Value
castAsType oldVal val
  | isPointer oldVal = asPointer val
  | otherwise = val

memSet :: Int -> Value -> VM -> ST RealWorld ()
memSet addr val vm
  | addr >= 0 = SA.writeArray (memory vm) addr val
  | otherwise = error $ "Cannot set memory at address " ++ show addr

scopedVar :: Int -> String -> String
scopedVar scopeNum name =
  let scope = lpad '0' 8 $ show scopeNum
   in scope ++ name

getVarScope :: String -> Int
getVarScope s = read $ take 8 s