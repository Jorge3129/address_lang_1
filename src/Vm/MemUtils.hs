module Vm.MemUtils where

import Control.Monad (forM, forM_)
import qualified Data.Array.IO as IA
import Data.IORef
import qualified Data.Map as Map
import Utils.Core
import Value.Core
import Vm.State

-- Memory read, write
memRead :: Int -> VM -> IO Value
memRead addr vm
  | addr > 0 = IA.readArray (memory vm) addr
  | otherwise = memReadError addr

memWrite :: Int -> Value -> VM -> IO ()
memWrite addr val vm
  | addr > 0 = IA.writeArray (memory vm) addr val
  | otherwise = memWriteError addr

-- Memory allocation
allocN :: Int -> VmMemory -> IO Int
allocN n vmMemory = do
  (low, high) <- IA.getBounds vmMemory
  allocHelper 0 (low + 1) high
  where
    allocHelper :: Int -> Int -> Int -> IO Int
    allocHelper count currentIndex endIndex
      | currentIndex > endIndex = allocationError n
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
    memWrite (freeAddr + offset) 0 vm
  return freeAddr

-- Variables
defineVar :: String -> Int -> VM -> IO ()
defineVar nm addr vm = do
  curScope <- length <$> readCalls vm
  modifyIORef (varsMap vm) (Map.insert (scopedVar curScope nm) addr)

getVarAddr :: String -> VM -> IO Int
getVarAddr name vm = do
  curScope <- length <$> readCalls vm
  (`readMap` scopedVar curScope name) <$> readIORef (varsMap vm)

readVar :: String -> VM -> IO Value
readVar name vm = do
  addr <- getVarAddr name vm
  memRead addr vm

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

scopedVar :: Int -> String -> String
scopedVar scopeNum name =
  let scope = lpad '0' 8 $ show scopeNum
   in scope ++ name

getVarScope :: String -> Int
getVarScope s = read $ take 8 s

-- Dereference operations
mulDeref :: Int -> Value -> VM -> IO Value
mulDeref count addr vm
  | count < 0 = mulDerefError count
  | count == 0 = return addr
  | otherwise = do
      nextAddr <- memRead (asInt addr) vm
      mulDeref (count - 1) nextAddr vm

minDeref :: Int -> Int -> VM -> IO Value
minDeref count startAddr vm
  | count <= 0 = minDerefError count
  | otherwise = do
      refs <- minDeref' count [startAddr]
      constructList (map IntVal refs) vm
  where
    minDeref' :: Int -> [Int] -> IO [Int]
    minDeref' cnt addrs
      | cnt <= 0 = return addrs
      | otherwise = do
          refs <- concat <$> mapM (`getRefsToAddr` vm) addrs
          minDeref' (cnt - 1) refs

getRefsToAddr :: Int -> VM -> IO [Int]
getRefsToAddr addr vm = do
  (low, high) <- IA.getBounds (memory vm)
  indexes <- forM [low + 1 .. high] $ \i -> do
    val <- IA.readArray (memory vm) i
    if isPointer val && asInt val == addr
      then return i
      else return (-1)
  return $ filter (/= -1) indexes

checkAddrForSend :: Int -> Int
checkAddrForSend addr
  | addr > 0 = addr
  | otherwise = sendError addr

castAsType :: Value -> Value -> Value
castAsType oldVal val
  | isPointer oldVal = asPointer val
  | otherwise = val

-- List utils
parseList :: Value -> VM -> IO [Value]
parseList val vm = do
  firstAddr <- asInt <$> memRead (asInt val) vm
  if firstAddr == 0
    then return []
    else parseList' firstAddr [] vm
  where
    parseList' :: Int -> [Value] -> VM -> IO [Value]
    parseList' 0 acc _ = return acc
    parseList' curAddr acc vm_ = do
      nextAddr <- memRead curAddr vm_
      curVal <- memRead (curAddr + 1) vm_
      parseList' (asInt nextAddr) (acc ++ [curVal]) vm_

constructList :: [Value] -> VM -> IO Value
constructList [] vm = newPtr <$> allocNInit 1 vm
constructList vals vm = do
  freeAddr <- allocNInit 1 vm
  consList' freeAddr vals
  return $ newPtr freeAddr
  where
    consList' :: Int -> [Value] -> IO ()
    consList' prevAddr (x : xs) = do
      let sz = 2 :: Int
      newAddr <- allocN sz (memory vm)
      memWrite prevAddr (newPtrWithSize newAddr sz) vm
      memWrite newAddr (newPtr 0) vm
      memWrite (newAddr + 1) x vm
      consList' newAddr xs
    consList' _ [] = return ()

-- errors
memReadError :: Int -> a
memReadError addr = error $ "Cannot access memory at " ++ show addr

memWriteError :: Int -> a
memWriteError addr = error $ "Cannot write to memory at address " ++ show addr

sendError :: Int -> a
sendError addr = error $ "Cannot send to memory at " ++ show addr

allocationError :: Int -> a
allocationError n = error $ "Cound not allocate " ++ show n ++ " cells of memory"

mulDerefError :: Int -> a
mulDerefError count = error $ "Cannot execute multiple dereference operation for a negative number " ++ show count

minDerefError :: Int -> a
minDerefError count = error $ "Cannot execute minus dereference operation for a non-positive number " ++ show count