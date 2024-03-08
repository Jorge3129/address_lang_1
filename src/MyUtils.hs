module MyUtils where

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x
  | p x = return x
  | otherwise = f x >>= untilM p f