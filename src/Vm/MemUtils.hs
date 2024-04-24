module Vm.MemUtils where

import Control.Monad (forM, forM_)
import qualified Data.Array.IO as IA
import Data.IORef (modifyIORef, readIORef)
import qualified Data.Map as Map
import Utils.Core (lpad)
import Value.Core
import Vm.State

allocN :: Int -> VmMemory -> IO Int
allocN n vmMemory = do
  (low, high) <- IA.getBounds vmMemory
  allocHelper 0 (low + 1) high
  where
    allocHelper :: Int -> Int -> Int -> IO Int
    allocHelper count currentIndex endIndex
      | currentIndex > endIndex = error $ "Cound not allocate " ++ show n ++ " cells of memory"
      | count >= n = return $ currentIndex - count
      | otherwise = do
          curVal <- IA.readArray vmMemory currentIndex
          if curVal == NilVal
            then allocHelper (count + 1) (currentIndex + 1) endIndex
            else allocHelper 0 (currentIndex + 1) endIndex

allocNInit :: Int -> VM -> IO Int
allocNInit n vm = do
  freeAddr <- allocN n (memory vm)
  forM_ [0 .. (n - 1)] $ \offset ->
    memSet (freeAddr + offset) 0 vm
  return freeAddr

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

constructList :: [Value] -> VM -> IO Value
constructList [] vm = PointerVal <$> allocNInit 1 vm
constructList vals vm = do
  freeAddr <- allocNInit 1 vm
  consList' freeAddr vals vm
  return $ PointerVal freeAddr
  where
    consList' :: Int -> [Value] -> VM -> IO ()
    consList' prevAddr (x : xs) vm_ = do
      newAddr <- allocN 2 (memory vm_)
      memSet prevAddr (PointerVal newAddr) vm_
      memSet newAddr (PointerVal 0) vm_
      memSet (newAddr + 1) x vm_
      consList' newAddr xs vm_
    consList' _ [] _ = return ()

defineVar :: String -> Int -> VM -> IO ()
defineVar nm addr vm = do
  curScope <- length <$> readCalls vm
  modifyIORef (varsMap vm) (Map.insert (scopedVar curScope nm) addr)

getVarAddr :: String -> VM -> IO Int
getVarAddr name vm = do
  curScope <- length <$> readCalls vm
  (Map.! scopedVar curScope name) <$> readIORef (varsMap vm)

getLocalVars :: VM -> IO [(String, Int)]
getLocalVars vm = do
  curScope <- length <$> readCalls vm
  allVars <- Map.toList <$> readIORef (varsMap vm)
  return $ [(nm, add) | (nm, add) <- allVars, getVarScope nm == curScope]

freeVars :: VM -> IO ()
freeVars vm = do
  locals <- getLocalVars vm
  forM_ locals $ \(nm, addr) -> do
    modifyIORef (varsMap vm) (Map.delete nm)
    IA.writeArray (memory vm) addr NilVal

getRefsToAddr :: Int -> VM -> IO [Int]
getRefsToAddr addr vm = do
  (low, high) <- IA.getBounds (memory vm)
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

scopedVar :: Int -> String -> String
scopedVar scopeNum name =
  let scope = lpad '0' 8 $ show scopeNum
   in scope ++ name

getVarScope :: String -> Int
getVarScope s = read $ take 8 s