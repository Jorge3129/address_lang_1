module MyUtils where

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x
  | p x = return x
  | otherwise = f x >>= untilM p f

replace :: Int -> a -> [a] -> [a]
replace i val xs = [if xi == i then val else x | (x, xi) <- zip xs [0 :: Int ..]]