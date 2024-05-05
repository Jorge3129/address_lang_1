module Main (main) where

import Control.Monad (when)
import Lib (execFile)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  when (null args) (error "Please provide the argument for adpl file")
  execFile $ head args
  return ()
