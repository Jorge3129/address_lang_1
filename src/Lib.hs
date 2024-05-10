{-# LANGUAGE LambdaCase #-}

module Lib (execFile) where

import Compiler.Core
import System.Directory
import Vm.Core

execFile :: FilePath -> IO ()
execFile filePath = do
  path <- makeAbsolute filePath
  src <- readFile path
  compileSrc src >>= \case
    (Left err) -> putStrLn err
    (Right ch) -> initAndRunVm ch