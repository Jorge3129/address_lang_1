{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Core where

import ByteCode.Core
import Compiler.LoopUtils
import Compiler.ProgTreeUtils (replaceExprStmt, replaceOpStmt, replaceStmtStmt)
import Compiler.State
import Compiler.Vars
import Control.Monad (foldM)
import Data.List (find, foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Grammar
import MyUtils
import Value.Core

compileProg :: Program -> IO Chunk
compileProg pg1 = do
  let pg@(Program {pLines}) = numerateLines pg1
      fnVars = collectProgVars pg
  let fnMap = collectProgFns pg
  inCs <- initCs pg
  let cs = inCs {csFnVars = fnVars, csFnMap = fnMap}
  csv <- compileVars (fnVars Map.! "") cs
  cs1 <- compileLines pLines csv
  cs2 <- patchLabelJumps cs1
  ch <- getCurChunk cs2
  let ch1 = ch {chLabelMap = labelOffsetMap cs2}
  -- print $ labelOffsetMap cs2
  return $ writeChunk (fromEnum OP_RETURN) (length pLines) ch1

numerateLines :: Program -> Program
numerateLines pg@(Program {pLines}) =
  let numLines = zipWith (\pl i -> pl {lineNum = i}) pLines [0 :: Int ..]
   in pg {pLines = numLines}

compileLines :: [ProgLine] -> CompState -> IO CompState
compileLines (l : ls) cs = compileLine l cs >>= compileLines ls
compileLines [] cs = return cs

compileLine :: ProgLine -> CompState -> IO CompState
compileLine (ProgLine lbls@(fnName : _) args@(Send Nil (Var _) : _) lineNum) cs = do
  setCurLine lineNum cs
  cs1 <- compileLineLabels lbls cs
  cs2 <- compileVars (csFnVars cs Map.! fnName) cs1
  compileStmts (reverse args) cs2
--
compileLine pl@(ProgLine {labels, stmts, lineNum}) cs = do
  setCurLine lineNum cs
  cs1 <- compileLineLabels labels cs
  cs2 <- patchLoops (loopPatches cs1) pl cs1
  compileStmts stmts cs2

patchLoops :: [LoopPatch] -> ProgLine -> CompState -> IO CompState
patchLoops (lp : lps) pl cs = do
  scLbl <- toScopedLabel (fromMaybe "" (scopeLabel lp)) cs
  lnScLbls <- mapM (`toScopedLabel` cs) (labels pl)
  let shouldPatch = scLbl `elem` lnScLbls
  if shouldPatch
    then do
      cs1 <- patchLoop lp (cs {loopPatches = lps})
      patchLoops lps pl cs1
    else return cs
patchLoops [] _ cs = return cs

compileLineLabels :: [String] -> CompState -> IO CompState
compileLineLabels lbls cs@(CompState {labelOffsetMap}) = do
  offset <- curChunkCount cs
  newLblMap <-
    foldM
      ( \lblMap lbl ->
          do
            scLbl <- toScopedLabel lbl cs
            return $ Map.insert scLbl offset lblMap
      )
      labelOffsetMap
      lbls
  return $ cs {labelOffsetMap = newLblMap}

compileStmts :: [Statement] -> CompState -> IO CompState
compileStmts (st : stmts) cs = compileStmt st cs >>= compileStmts stmts
compileStmts [] cs = return cs

compileStmt :: Statement -> CompState -> IO CompState
compileStmt Stop cs = do
  emitOpCode OP_RETURN cs
  return cs
--
compileStmt (BuiltinProc "print" [ex]) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_PRINT cs1
  return cs
--
compileStmt (BuiltinProc "printList" [ex]) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_PRINT_LIST cs1
  return cs
--
compileStmt (BuiltinProc "printRefs" [ex]) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_PRINT_REFS cs1
  return cs
--
compileStmt (ExpSt ex) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_POP cs1
  return cs
--
compileStmt (Send valEx (Var name)) cs = do
  cs1 <- compileExpr valEx cs
  cs2 <- compileExpr (Var name) cs1
  emitOpCode OP_SEND cs2
  (cs4, arg) <- addConstantToCs (StringVal name) cs2
  emitOpCode OP_MAKE_VAR_POINTER cs4
  emitByte arg cs4
  return cs4
--
-- compileStmt (Send valEx (Deref innerExpr)) cs = do
--   cs1 <- compileExpr valEx cs
--   cs2 <- compileExpr innerExpr cs1
--   let cs3 = emitOpCode OP_MAKE_POINTER cs2
--   let cs4 = emitOpCode OP_DEREF cs3
--   return $ emitOpCode OP_SEND cs4
--
compileStmt (Send valEx addrEx) cs = do
  cs1 <- compileExpr valEx cs
  cs2 <- compileExpr addrEx cs1
  emitOpCode OP_SEND cs2
  return cs2
--
compileStmt (Exchange a b) cs = do
  cs1 <- compileExpr a cs
  cs2 <- compileExpr b cs1
  emitOpCode OP_EXCHANGE cs2
  return cs2
--
compileStmt (Jump lbl) cs = do
  scLbl <- toScopedLabel lbl cs
  chCnt <- curChunkCount cs
  let cs1 = addLabelPatch chCnt scLbl cs
  emitOpCode OP_JUMP cs1
  emitByte 0 cs1
  return cs1
--
compileStmt (Predicate ifExp thenStmts elseStmts) cs = do
  -- condition
  cs1 <- compileExpr ifExp cs
  (cs2, toElseJump) <- emitJump OP_JUMP_IF_FALSE cs1
  -- then clause
  emitOpCode OP_POP cs2
  cs4 <- compileStmts thenStmts cs2
  (cs5, toEndJump) <- emitJump OP_JUMP cs4
  -- else clause
  cs6 <- patchJump toElseJump cs5
  emitOpCode OP_POP cs6
  cs8 <- compileStmts elseStmts cs6
  -- end
  patchJump toEndJump cs8
--
compileStmt st@(LoopSimple {}) cs = compileStmt (desugarStmt st) cs
--
compileStmt st@(LoopComplex {}) cs = compileStmt (desugarStmt st) cs
--
compileStmt (LoopCommon initStmt stepStmt endCondition _ scope next) cs = do
  cs1 <- compileStmt initStmt cs
  loopStart <- curChunkCount cs1
  cs2 <- compileExpr endCondition cs1
  (cs3, exitJump) <- emitJump OP_JUMP_IF_FALSE cs2
  emitOpCode OP_POP cs3
  (cs5, bodyJump) <- emitJump OP_JUMP cs3
  stepStart <- curChunkCount cs5
  cs6 <- compileStmt stepStmt cs5
  cs7 <- emitLoop loopStart cs6
  cs8 <- patchJump bodyJump cs7
  curLn <- getCurLine cs8
  return $
    addLoopPatch
      ( LoopPatch
          { scopeLabel = scope,
            nextLabel = next,
            stepStart = stepStart,
            exitJump = exitJump,
            loopLine = curLn
          }
      )
      cs8
--
compileStmt (Assignment (Var name) lhs) cs = do
  csEx <- compileExpr lhs cs
  (cs1, arg) <- addConstantToCs (StringVal name) csEx
  emitOpCode OP_SET_VAR cs1
  emitByte arg cs1
  return cs1
--
compileStmt (SubprogramCall name args _) cs = do
  cs1 <- compileExprs args cs
  (cs2, constant) <- addConstantToCs (StringVal name) cs1
  emitOpCode OP_CALL cs2
  emitByte constant cs2
  return cs2
--
compileStmt (Replace repls start end) cs = do
  repLines <- findReplaceRange start end cs
  let newRLines = map (`lineReplacements` repls) repLines
  curLn <- getCurLine cs
  let cs1 = cs {csRepls = curLn : csRepls cs}
  cs2 <- compileLines newRLines cs1
  return $ cs2 {csRepls = tail (csRepls cs2)}
--
compileStmt Ret cs = do
  emitOpCode OP_RETURN cs
  return cs
--
compileStmt st _ = error $ "cannot compile statement `" ++ show st ++ "` yet"

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

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start) (drop start xs)

compileExprs :: [Expr] -> CompState -> IO CompState
compileExprs (ex : exs) cs = compileExpr ex cs >>= compileExprs exs
compileExprs [] cs = return cs

compileExpr :: Expr -> CompState -> IO CompState
compileExpr (Lit val) cs = do
  (cs1, constant) <- addConstantToCs val cs
  emitOpCode OP_CONSTANT cs1
  emitByte constant cs1
  return cs1
--
compileExpr (BinOpApp op a b) cs = do
  cs1 <- compileExpr a cs
  cs2 <- compileExpr b cs1
  emitOpCodes (binOpToOpCode op) cs2
  return cs2
--
compileExpr (Negate ex) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_NEGATE cs1
  return cs1
--
compileExpr (Deref ex) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_DEREF cs1
  return cs1
--
compileExpr (MulDeref countEx innerEx) cs = do
  cs1 <- compileExpr countEx cs
  cs2 <- compileExpr innerEx cs1
  emitOpCode OP_MUL_DEREF cs2
  return cs2
--
compileExpr (Var name) cs = do
  (cs1, constant) <- addConstantToCs (StringVal name) cs
  emitOpCode OP_GET_VAR cs1
  emitByte constant cs1
  return cs1
--
compileExpr Nil cs = return cs
--
compileExpr (BuiltinFn "alloc" [ex]) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_ALLOC_N cs1
  return cs1
--
compileExpr (BuiltinFn "getRefs" [ex]) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_GET_REFS cs1
  return cs1
--
compileExpr (BuiltinFn "constrList" args) cs = do
  cs1 <- compileExprs args cs
  cs2 <- compileExpr (Lit (IntVal (length args))) cs1
  emitOpCode OP_CONSTR_LIST cs2
  return cs2
--
compileExpr (BuiltinFn "ptr" [ex]) cs = do
  cs1 <- compileExpr ex cs
  emitOpCode OP_CAST_AS_PTR cs1
  return cs1
--
compileExpr ex _ = error $ "cannot compile expression `" ++ show ex ++ "` yet"

emitJump :: OpCode -> CompState -> IO (CompState, Int)
emitJump op cs = do
  emitOpCode op cs
  emitByte 0 cs
  chCnt <- curChunkCount cs
  return (cs, chCnt - 1)

emitLoop :: Int -> CompState -> IO CompState
emitLoop jumpToInstr cs = do
  chCnt <- curChunkCount cs
  let jumpOffset = jumpToInstr - chCnt - 2
  emitOpCode OP_JUMP cs
  emitByte jumpOffset cs
  return cs

patchJump :: Int -> CompState -> IO CompState
patchJump offset cs = do
  chCnt <- curChunkCount cs
  let jump = chCnt - offset - 1
  updateChunk (\ch -> ch {code = replace offset jump (code ch)}) cs
  return cs

patchLoop :: LoopPatch -> CompState -> IO CompState
patchLoop (LoopPatch {stepStart, exitJump}) cs = do
  cs1 <- emitLoop stepStart cs
  cs2 <- patchJump exitJump cs1
  emitOpCode OP_POP cs2
  return cs2

addLoopPatch :: LoopPatch -> CompState -> CompState
addLoopPatch p cs = cs {loopPatches = p : loopPatches cs}

addLabelPatch :: Int -> String -> CompState -> CompState
addLabelPatch curOffset lbl cs@(CompState {labelJumpsToPatch}) =
  cs
    { labelJumpsToPatch = labelJumpsToPatch ++ [(curOffset, lbl)]
    }

patchLabelJumps :: CompState -> IO CompState
patchLabelJumps cs@(CompState {labelJumpsToPatch, labelOffsetMap}) = do
  updateChunk
    ( \curCh ->
        foldl'
          ( \ch@(Chunk {code}) (curOffset, lbl) ->
              let jumpToInstr = labelOffsetMap Map.! lbl
                  jumpOffset = jumpToInstr - curOffset - 2
               in ch {code = replace (curOffset + 1) jumpOffset code}
          )
          curCh
          labelJumpsToPatch
    )
    cs
  return cs

binOpToOpCode :: BinOp -> [OpCode]
binOpToOpCode Add = [OP_ADD]
binOpToOpCode Sub = [OP_SUB]
binOpToOpCode Mul = [OP_MUL]
binOpToOpCode Div = [OP_DIV]
binOpToOpCode Mod = [OP_MOD]
binOpToOpCode Equal = [OP_EQUAL]
binOpToOpCode Greater = [OP_GREATER]
binOpToOpCode Less = [OP_LESS]
binOpToOpCode NotEqual = [OP_EQUAL, OP_NOT]
binOpToOpCode GreaterEqual = [OP_LESS, OP_NOT]
binOpToOpCode LessEqual = [OP_GREATER, OP_NOT]
binOpToOpCode And = [OP_AND]
binOpToOpCode Or = [OP_OR]
binOpToOpCode _ = undefined