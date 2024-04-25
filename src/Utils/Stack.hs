module Utils.Stack where

import Control.Exception as Exc
import Control.Monad.ST (RealWorld, ST, stToIO)
import qualified Data.Array.ST as SA
import Data.STRef

data Stack a = Stack
  { capacity :: Int,
    topIndex :: STRef RealWorld Int,
    stackArray :: SA.STArray RealWorld Int a
  }

newStack :: Int -> ST RealWorld (Stack a)
newStack cap = do
  Stack cap <$> newSTRef (-1 :: Int) <*> SA.newArray_ (0, cap - 1)

isFull :: Stack a -> ST RealWorld Bool
isFull (Stack cap topRef _) = do
  top <- readSTRef topRef
  return (top == cap - 1)

push :: a -> Stack a -> ST RealWorld ()
push x st@(Stack _ topRef arr) = do
  full <- isFull st
  if full
    then error "Stack overflow"
    else do
      top <- readSTRef topRef
      SA.writeArray arr (top + 1) x
      modifySTRef' topRef (+ 1)

isEmpty :: Stack a -> ST RealWorld Bool
isEmpty (Stack _ topRef _) = (== -1) <$> readSTRef topRef

pop :: Stack a -> ST RealWorld a
pop s = do
  empty <- isEmpty s
  if empty
    then error "Stack underflow"
    else do
      top <- readSTRef (topIndex s)
      val <- SA.readArray (stackArray s) top
      modifySTRef' (topIndex s) (\x -> x - 1)
      return val

peek :: Int -> Stack a -> ST RealWorld (Maybe a)
peek offset (Stack _ topRef arr) = do
  top <- readSTRef topRef
  if offset <= top && offset >= 0
    then Just <$> SA.readArray arr (top - offset)
    else return Nothing

peek' :: Int -> Stack a -> ST RealWorld a
peek' offset s = do
  res <- peek offset s
  return $ case res of
    Just v -> v
    Nothing -> error "Negative peek index"
