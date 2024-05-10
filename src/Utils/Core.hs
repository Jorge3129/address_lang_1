module Utils.Core where

import Data.Map (Map)
import qualified Data.Map as Map

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x
  | p x = return x
  | otherwise = f x >>= untilM p f

replace :: Int -> a -> [a] -> [a]
replace i val xs = [if xi == i then val else x | (x, xi) <- zip xs [0 :: Int ..]]

lpad :: a -> Int -> [a] -> [a]
lpad pad m xs = replicate (m - length ys) pad ++ ys
  where
    ys = take m xs

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start) (drop start xs)

readMap :: (Ord k, Show k) => Map k v -> k -> v
readMap m k = case Map.lookup k m of
  Nothing -> error $ "key " ++ show k ++ " not found in Map"
  Just x -> x

bimap' :: (a -> b) -> (a, a) -> (b, b)
bimap' f (a, b) = (f a, f b)
