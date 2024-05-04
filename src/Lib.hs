{-# LANGUAGE LambdaCase #-}

module Lib (execFile) where

import Compiler.Core
import System.Directory
import Vm.Core

execFile :: String -> IO ()
execFile fileName = do
  rootDir <- getCurrentDirectory
  let basePath = rootDir ++ "/test/data"
  src <- readFile (basePath ++ "/" ++ fileName ++ ".adpl")
  compileSrc src >>= \case
    (Left err) -> putStrLn err
    (Right ch) -> initAndRunVm ch