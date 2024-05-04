module Utils.Stack where

import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IA
import Data.IORef

data Stack a = Stack
  { capacity :: Int,
    topIndex :: IORef Int,
    stackArray :: IOArray Int a
  }

newStack :: Int -> IO (Stack a)
newStack cap = do
  Stack cap <$> newIORef (-1 :: Int) <*> IA.newArray_ (0, cap - 1)

isFull :: Stack a -> IO Bool
isFull (Stack cap topRef _) = do
  top <- readIORef topRef
  return (top == cap - 1)

push :: a -> Stack a -> IO ()
push x st@(Stack _ topRef arr) = do
  full <- isFull st
  if full
    then error "Stack overflow"
    else do
      top <- readIORef topRef
      IA.writeArray arr (top + 1) x
      modifyIORef' topRef (+ 1)

isEmpty :: Stack a -> IO Bool
isEmpty (Stack _ topRef _) = (== -1) <$> readIORef topRef

pop :: Stack a -> IO a
pop s = do
  empty <- isEmpty s
  if empty
    then error "Stack underflow"
    else do
      top <- readIORef (topIndex s)
      val <- IA.readArray (stackArray s) top
      modifyIORef' (topIndex s) (\x -> x - 1)
      return val

peek :: Int -> Stack a -> IO (Maybe a)
peek offset (Stack _ topRef arr) = do
  top <- readIORef topRef
  if offset <= top && offset >= 0
    then Just <$> IA.readArray arr (top - offset)
    else return Nothing

peek' :: Int -> Stack a -> IO a
peek' offset s = do
  res <- peek offset s
  return $ case res of
    Just v -> v
    Nothing -> error "Negative peek index"
