{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Core where

import ByteCode.Core
import Compiler.ProgTreeUtils (desugarStmt, replaceExprStmt, replaceOpStmt, replaceStmtStmt)
import Compiler.State
import Compiler.Vars
import Control.Monad (foldM, forM_, when)
import Data.List (find, foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Grammar
import Utils.Core (slice)
import Value.Core

compileProg :: Program -> IO Chunk
compileProg pg1 = do
  let pg@(Program {pLines}) = numerateLines pg1
      fnVars = collectProgVars pg
  let fnMap = collectProgFns pg
  inCs <- initCs pg
  let cs = inCs {csFnVars = fnVars, csFnMap = fnMap}
  compileVars (fnVars Map.! "") cs
  compileLines pLines cs
  patchLabelJumps cs
  ch <- getCurChunk cs
  curLblMap <- getLabelOffsetMap cs
  let ch1 = ch {chLabelMap = curLblMap}
  return $ writeChunk (fromEnum OP_RETURN) (length pLines) ch1

numerateLines :: Program -> Program
numerateLines pg@(Program {pLines}) =
  let numLines = zipWith (\pl i -> pl {lineNum = i}) pLines [0 :: Int ..]
   in pg {pLines = numLines}

compileLines :: [ProgLine] -> CompState -> IO ()
compileLines (l : ls) cs = compileLine l cs >> compileLines ls cs
compileLines [] _ = return ()

compileLine :: ProgLine -> CompState -> IO ()
compileLine (ProgLine lbls@(fnName : _) args@(Send Nil (Var _) : _) lineNum) cs = do
  setCurLine lineNum cs
  compileLineLabels lbls cs
  compileVars (csFnVars cs Map.! fnName) cs
  compileStmts (reverse args) cs
--
compileLine pl@(ProgLine {labels, stmts, lineNum}) cs = do
  setCurLine lineNum cs
  compileLineLabels labels cs
  lps <- getLoopPatches cs
  patchLoops lps pl cs
  compileStmts stmts cs

patchLoops :: [LoopPatch] -> ProgLine -> CompState -> IO ()
patchLoops (lp : lps) pl cs = do
  scopedLabel <- toScopedLabel (fromMaybe "" (endLabel lp)) cs
  lineScopedLabels <- mapM (`toScopedLabel` cs) (labels pl)
  let shouldPatch = scopedLabel `elem` lineScopedLabels
  Control.Monad.when shouldPatch $ do
    setLoopPatches lps cs
    patchLoop lp cs
    patchLoops lps pl cs
patchLoops [] _ _ = return ()

compileLineLabels :: [String] -> CompState -> IO ()
compileLineLabels lbls cs = do
  offset <- curChunkCount cs
  curLblMap <- getLabelOffsetMap cs
  newLblMap <-
    foldM
      ( \lblMap lbl ->
          do
            scopedLabel <- toScopedLabel lbl cs
            return $ Map.insert scopedLabel offset lblMap
      )
      curLblMap
      lbls
  setLabelOffsetMap newLblMap cs

compileStmts :: [Statement] -> CompState -> IO ()
compileStmts (st : stmts) cs = compileStmt st cs >> compileStmts stmts cs
compileStmts [] _ = return ()

compileStmt :: Statement -> CompState -> IO ()
compileStmt Stop cs = do
  emitOpCode OP_RETURN cs
--
compileStmt (BuiltinProc "print" [ex]) cs = do
  compileExpr ex cs
  emitOpCode OP_PRINT cs
--
compileStmt (BuiltinProc "printList" [ex]) cs = do
  compileExpr ex cs
  emitOpCode OP_PRINT_LIST cs
--
compileStmt (BuiltinProc "printRefs" [ex]) cs = do
  compileExpr ex cs
  emitOpCode OP_PRINT_REFS cs
--
compileStmt (ExpSt ex) cs = do
  compileExpr ex cs
  emitOpCode OP_POP cs
--
-- compileStmt (Send valEx (Var name)) cs = do
--   compileExpr valEx cs
--   compileExpr (Var name) cs
--   emitOpCode OP_SEND cs
--   arg <- addConstantToCs (StringVal name) cs
--   emitOpCode OP_MAKE_VAR_POINTER cs
--   emitByte arg cs
--
-- compileStmt (Send valEx (Deref innerExpr)) cs = do
--   compileExpr valEx cs
--   compileExpr innerExpr cs
--   emitOpCode OP_MAKE_POINTER cs
--   emitOpCode OP_DEREF cs
--   emitOpCode OP_SEND cs
--
compileStmt (Send valEx addrEx) cs = do
  compileExpr valEx cs
  compileExpr addrEx cs
  emitOpCode OP_SEND cs
--
compileStmt (Exchange a b) cs = do
  compileExpr a cs
  compileExpr b cs
  emitOpCode OP_EXCHANGE cs
--
compileStmt (Jump lbl) cs = do
  scopedLabel <- toScopedLabel lbl cs
  chunkCount <- curChunkCount cs
  addJumpPatch chunkCount scopedLabel cs
  emitOpCode OP_JUMP cs
  emitByte 0 cs
--
compileStmt (Predicate ifExp thenStmts elseStmts) cs = do
  -- condition
  compileExpr ifExp cs
  toElseJump <- emitJump OP_JUMP_IF_FALSE cs
  -- then clause
  emitOpCode OP_POP cs
  compileStmts thenStmts cs
  toEndJump <- emitJump OP_JUMP cs
  -- else clause
  patchJump toElseJump cs
  emitOpCode OP_POP cs
  compileStmts elseStmts cs
  -- end
  patchJump toEndJump cs
--
compileStmt st@(LoopSimple {}) cs = compileStmt (desugarStmt st) cs
--
compileStmt st@(LoopComplex {}) cs = compileStmt (desugarStmt st) cs
--
compileStmt (LoopCommon initStmt stepStmt endCondition _ scope next) cs = do
  -- initialize
  compileStmt initStmt cs
  -- condition
  loopStart <- curChunkCount cs
  compileExpr endCondition cs
  exitJump <- emitJump OP_JUMP_IF_FALSE cs
  -- body
  emitOpCode OP_POP cs
  bodyJump <- emitJump OP_JUMP cs
  -- step
  stepStart <- curChunkCount cs
  compileStmt stepStmt cs
  emitLoop loopStart cs
  --
  patchJump bodyJump cs
  curLn <- getCurLine cs
  addLoopPatch
    ( LoopPatch
        { endLabel = scope,
          nextLabel = next,
          stepStart = stepStart,
          exitJump = exitJump,
          loopLine = curLn
        }
    )
    cs
--
compileStmt (Assignment (Var name) lhs) cs = do
  compileExpr lhs cs
  arg <- addConstantToCs (StringVal name) cs
  emitOpCode OP_SET_VAR cs
  emitByte arg cs
--
compileStmt (SubprogramCall name args _) cs = do
  compileExprs args cs
  constant <- addConstantToCs (StringVal name) cs
  emitOpCode OP_CALL cs
  emitByte constant cs
--
compileStmt (Replace repls start end) cs = do
  srcLines <- findReplaceRange start end cs
  let newLines = map (`lineReplacements` repls) srcLines
  curLn <- getCurLine cs
  pushReplacement curLn cs
  compileLines newLines cs
  popReplacement cs
--
compileStmt Ret cs = do
  emitOpCode OP_RETURN cs
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

compileExprs :: [Expr] -> CompState -> IO ()
compileExprs (ex : exs) cs = compileExpr ex cs >> compileExprs exs cs
compileExprs [] _ = return ()

compileExpr :: Expr -> CompState -> IO ()
compileExpr (Lit val) cs = do
  constant <- addConstantToCs val cs
  emitOpCode OP_CONSTANT cs
  emitByte constant cs
--
compileExpr (BinOpApp op a b) cs = do
  compileExpr a cs
  compileExpr b cs
  emitOpCodes (binOpToOpCode op) cs
--
compileExpr (Negate ex) cs = do
  compileExpr ex cs
  emitOpCode OP_NEGATE cs
--
compileExpr (Deref ex) cs = do
  compileExpr ex cs
  emitOpCode OP_DEREF cs
--
compileExpr (MulDeref countEx innerEx) cs = do
  compileExpr countEx cs
  compileExpr innerEx cs
  emitOpCode OP_MUL_DEREF cs
--
compileExpr (Var name) cs = do
  constant <- addConstantToCs (StringVal name) cs
  emitOpCode OP_GET_VAR cs
  emitByte constant cs
--
compileExpr Nil _ = return ()
--
compileExpr (BuiltinFn "alloc" [ex]) cs = do
  compileExpr ex cs
  emitOpCode OP_ALLOC_N cs
--
compileExpr (BuiltinFn "getRefs" [ex]) cs = do
  compileExpr ex cs
  emitOpCode OP_GET_REFS cs
--
compileExpr (BuiltinFn "constrList" args) cs = do
  compileExprs args cs
  compileExpr (Lit (IntVal (length args))) cs
  emitOpCode OP_CONSTR_LIST cs
--
compileExpr (BuiltinFn "ptr" [ex]) cs = do
  compileExpr ex cs
  emitOpCode OP_CAST_AS_PTR cs
--
compileExpr ex _ = error $ "cannot compile expression `" ++ show ex ++ "` yet"

emitJump :: OpCode -> CompState -> IO Int
emitJump op cs = do
  emitOpCode op cs
  emitByte 0 cs
  chunkCount <- curChunkCount cs
  return $ chunkCount - 1

emitLoop :: Int -> CompState -> IO ()
emitLoop jumpToInstr cs = do
  chunkCount <- curChunkCount cs
  let jumpOffset = jumpToInstr - chunkCount - 2
  emitOpCode OP_JUMP cs
  emitByte jumpOffset cs

patchJump :: Int -> CompState -> IO ()
patchJump offset cs = do
  chunkCount <- curChunkCount cs
  let jump = chunkCount - offset - 1
  patchChunkCode offset jump cs

patchLoop :: LoopPatch -> CompState -> IO ()
patchLoop (LoopPatch {stepStart, exitJump}) cs = do
  emitLoop stepStart cs
  patchJump exitJump cs
  emitOpCode OP_POP cs

patchLabelJumps :: CompState -> IO ()
patchLabelJumps cs = do
  curLblMap <- getLabelOffsetMap cs
  jumps <- getJumpPatches cs
  forM_ jumps $ \(curOffset, lbl) -> do
    let jumpToInstr = curLblMap Map.! lbl
        jumpOffset = jumpToInstr - curOffset - 2
    patchChunkCode (curOffset + 1) jumpOffset cs

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