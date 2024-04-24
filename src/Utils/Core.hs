module Utils.Core where

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