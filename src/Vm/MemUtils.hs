{-# LANGUAGE BangPatterns #-}

module Vm.MemUtils where

import Value.Core

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