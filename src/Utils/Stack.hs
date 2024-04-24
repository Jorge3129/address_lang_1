module Utils.Stack where

import Control.Exception as Exc
import qualified Data.Array.IO as IA
import Data.IORef

data Stack a = Stack
  { capacity :: Int,
    topIndex :: IORef Int,
    stackArray :: IA.IOArray Int a
  }

newStack :: Int -> IO (Stack a)
newStack cap = Stack cap <$> newIORef (-1) <*> IA.newArray_ (0, cap - 1)

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
pop (Stack _ topRef arr) = do
  empty <- isEmpty (Stack 0 topRef arr)
  if empty
    then error "Stack underflow"
    else do
      top <- readIORef topRef
      val <- IA.readArray arr top
      modifyIORef' topRef (\x -> x - 1)
      return val

example :: IO ()
example = do
  stack <- newStack 5
  push (1 :: Int) stack
  push 2 stack
  push 3 stack
  push 4 stack
  push 5 stack
  Exc.catch (push 6 stack) (\(ErrorCallWithLocation msg _) -> print msg)
  val1 <- pop stack
  val2 <- pop stack
  val3 <- pop stack
  val4 <- pop stack
  val5 <- pop stack
  val6 <- Exc.catch (pop stack) (\(ErrorCallWithLocation msg _) -> print msg >> return 0)
  print (val1, val2, val3, val4, val5, val6)

main :: IO ()
main = example
