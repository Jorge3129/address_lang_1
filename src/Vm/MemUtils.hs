module Vm.MemUtils where

import Data.List (elemIndex)
import Value.Core

allocNewVal :: [Value] -> Int
allocNewVal mem =
  let freeStart = NilVal `elemIndex` mem
   in case freeStart of
        (Just start) -> start
        Nothing -> error $ "Out of free memory"

-- allocNewVal :: [Value] -> Int
-- allocNewVal mem =
--   let freeStart = findFreeCells mem
--    in case freeStart of
--         (Just start) -> start + 1
--         Nothing -> error $ "Out of free memory"

-- findFreeCells :: [Value] -> Maybe Int
-- findFreeCells = findZeros 0
--   where
--     findZeros _ [] = Nothing
--     findZeros _ [_] = Nothing
--     findZeros _ [_, _] = Nothing
--     findZeros index (x : y : z : xs)
--       | x == NilVal && y == NilVal && z == NilVal = Just index
--       | otherwise = findZeros (index + 1) (y : z : xs)