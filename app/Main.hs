module Main (main) where

import Lib (execFile)

main :: IO ()
main = do
  execFile "fmap"
  return ()
