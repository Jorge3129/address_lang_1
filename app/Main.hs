module Main (main) where

import Lib (execFile)

main :: IO ()
main = do
  execFile "label_ref"
  return ()
