module Utils.Stack where

import Control.Exception as Exc
import Control.Monad.ST (RealWorld, ST, stToIO)
import qualified Data.Array.ST as IS
import Data.STRef

data Stack a = Stack
  { capacity :: Int,
    topIndex :: STRef RealWorld Int,
    stackArray :: IS.STArray RealWorld Int a
  }

newStack :: Int -> IO (Stack a)
newStack cap = stToIO $ do
  Stack cap <$> newSTRef (-1 :: Int) <*> IS.newArray_ (0, cap - 1)

isFull :: Stack a -> ST RealWorld Bool
isFull (Stack cap topRef _) = do
  top <- readSTRef topRef
  return (top == cap - 1)

push :: a -> Stack a -> IO ()
push x st@(Stack _ topRef arr) = stToIO $ do
  full <- isFull st
  if full
    then error "Stack overflow"
    else do
      top <- readSTRef topRef
      IS.writeArray arr (top + 1) x
      modifySTRef' topRef (+ 1)

isEmpty :: Stack a -> ST RealWorld Bool
isEmpty (Stack _ topRef _) = (== -1) <$> readSTRef topRef

pop :: Stack a -> IO a
pop s = stToIO $ do
  empty <- isEmpty s
  if empty
    then error "Stack underflow"
    else do
      top <- readSTRef (topIndex s)
      val <- IS.readArray (stackArray s) top
      modifySTRef' (topIndex s) (\x -> x - 1)
      return val

peek :: Int -> Stack a -> IO (Maybe a)
peek offset (Stack _ topRef arr) = stToIO $ do
  top <- readSTRef topRef
  if offset <= top && offset >= 0
    then Just <$> IS.readArray arr (top - offset)
    else return Nothing

peek' :: Int -> Stack a -> IO a
peek' offset s = do
  res <- peek offset s
  return $ case res of
    Just v -> v
    Nothing -> error "Negative peek index"

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
