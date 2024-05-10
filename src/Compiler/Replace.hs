module Compiler.Replace where

import Compiler.ProgTreeUtils
import Compiler.State
import Data.List (find, foldl')
import Parser.AST
import Utils.Core (bimap', slice)

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
  let progLines = csProgLines cs
  let (startLn, endLn) = case bimap' (findLn progLines) (start, end) of
        (Just s, Just e) -> bimap' lineNum (s, e)
        (_, _) -> error $ "Invalid replacement range: " ++ start ++ ", " ++ end
  let replaceLines = slice startLn endLn progLines
  let finalLine = ProgLine {lineNum = endLn, labels = [end], stmts = []}
  return $ replaceLines ++ [finalLine]

findLn :: [ProgLine] -> String -> Maybe ProgLine
findLn progLines lab = find (\ln -> lab `elem` labels ln) progLines
