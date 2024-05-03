module Compiler.Replace where

import Compiler.ProgTreeUtils
import Compiler.State
import Data.List (find, foldl')
import Parser.AST
import Utils.Core (slice)

lineReplacements :: ProgLine -> [Replacement] -> ProgLine
lineReplacements = foldl' lineReplacement

lineReplacement :: ProgLine -> Replacement -> ProgLine
lineReplacement rLine r = rLine {stmts = map (`stmtReplacement` r) (stmts rLine)}

stmtReplacement :: Statement -> Replacement -> Statement
stmtReplacement st r@(StmtReplace {}) = replaceStmtStmt st r
stmtReplacement st r@(ExprReplace {}) = replaceExprStmt st r
stmtReplacement st r@(BinOpReplace {}) = replaceOpStmt st r

findReplaceRange :: String -> String -> CompState -> IO [ProgLine]
findReplaceRange start end cs = do
  let progLines = pLines (csProg cs)
  let startLine = find (\ln -> start `elem` labels ln) progLines
  let endLine = find (\ln -> end `elem` labels ln) progLines
  let (startLn, endLn) = case (startLine, endLine) of
        (Just s, Just e) -> (lineNum s, lineNum e)
        (_, _) -> error $ "Invalid replacement range: " ++ start ++ ", " ++ end
  return $ slice startLn endLn progLines