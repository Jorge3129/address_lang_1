{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Core where

import ByteCode.Core
import Compiler.ProgTreeUtils (getLoopRange, isSubprogramHead, numerateLines)
import Compiler.Replace
import Compiler.State
import Compiler.Utils
import Compiler.Vars
import Control.Exception
import Control.Monad (foldM, forM_, when)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Lexer.Rules (scanTokens)
import Parser.AST
import Parser.Rules
import Utils.Core (readMap)
import Value.Core

compileSrc :: String -> IO (Either String Chunk)
compileSrc src = (Right <$> compileProg ((parseProg . scanTokens) src)) `catch` handler
  where
    handler (ErrorCallWithLocation msg _) = return $ Left $ "Compilation error: \n" ++ msg

compileProg :: Program -> IO Chunk
compileProg (Program {pLines = pLines1}) = do
  let pLines = numerateLines pLines1
  cs <- initCs pLines (collectProgVars pLines) (collectProgFns pLines)
  compileGlobalVars cs
  compileLines pLines cs
  patchJumps cs
  patchLabelRefs cs
  compileEof (length pLines) cs
  getCurChunk cs

compileEof :: Int -> CompState -> IO ()
compileEof lineCount cs = do
  updateChunk (writeChunk (fromEnum OP_RETURN) lineCount) cs

compileLines :: [ProgLine] -> CompState -> IO ()
compileLines ls cs = forM_ ls (`compileLine` cs)

compileLine :: ProgLine -> CompState -> IO ()
compileLine pl@(ProgLine labs _ lineNum) cs = do
  setCurLine lineNum cs
  compileLineLabels labs cs
  if isSubprogramHead pl
    then compileSubprogramHead pl cs
    else do
      lps <- getLoopPatches cs
      patchLoops lps pl cs
      compileStmts (stmts pl) cs

compileSubprogramHead :: ProgLine -> CompState -> IO ()
compileSubprogramHead (ProgLine labs args _) cs = do
  let fnName = head labs
  compileVars (csFnVars cs `readMap` fnName) cs
  compileStmts (reverse args) cs
  emitOpCode OP_POP cs

compileLineLabels :: [String] -> CompState -> IO ()
compileLineLabels labs cs = do
  offset <- curChunkCount cs
  curLblMap <- getLabelOffsetMap cs
  newLblMap <- foldM (step offset) curLblMap labs
  setLabelOffsetMap newLblMap cs
  where
    step offset lblMap lbl = do
      scopedLabel <- toScopedLabel lbl cs
      return $ Map.insert scopedLabel offset lblMap

compileStmts :: [Statement] -> CompState -> IO ()
compileStmts stmts cs = forM_ stmts (`compileStmt` cs)

compileStmt :: Statement -> CompState -> IO ()
compileStmt (Assign (Var name) lhs) cs = do
  compileExpr lhs cs
  arg <- addConstantToCs (StringVal name) cs
  emitOpCode OP_SET_VAR cs
  emitByte arg cs
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
compileStmt (Jump label) cs = do
  scopedLabel <- toScopedLabel label cs
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
compileStmt st@(Loop _ _ _ _ scope next) cs = do
  let (initStmt, stepStmt, endCondition) = getLoopRange st
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
  addLoopPatch (LoopPatch scope next stepStart exitJump) cs
--
compileStmt (SubprogramCall callValue args _) cs = do
  let argCount = length args
  compileExpr callValue cs
  compileExprs args cs
  emitOpCode OP_CALL cs
  emitByte argCount cs
--
compileStmt (BuiltinProc name args) cs = do
  compileExprs args cs
  constant <- addConstantToCs (StringVal name) cs
  emitOpCode OP_CALL_PROC cs
  emitByte constant cs
--
compileStmt (Replace reps start end) cs = do
  srcLines <- findReplaceRange start end cs
  let newLines = map (`lineReplacements` reps) srcLines
  curLn <- getCurLine cs
  pushReplacement curLn cs
  compileLines newLines cs
  popReplacement cs
--
compileStmt Ret cs = emitOpCode OP_RETURN cs
compileStmt Stop cs = emitOpCode OP_RETURN cs
--
compileStmt st _ = error $ "cannot compile statement `" ++ show st ++ "` yet"

compileExprs :: [Expr] -> CompState -> IO ()
compileExprs exprs cs = forM_ exprs (`compileExpr` cs)

compileExpr :: Expr -> CompState -> IO ()
compileExpr (Lit val) cs = do
  constant <- addConstantToCs val cs
  emitOpCode OP_CONSTANT cs
  emitByte constant cs
--
compileExpr (UnOpApp op a) cs = do
  compileExpr a cs
  emitOpCodes (unOpToOpcode op) cs
--
compileExpr (BinOpApp op a b) cs = do
  compileExpr a cs
  compileExpr b cs
  emitOpCodes (binOpToOpCode op) cs
--
compileExpr (Var name) cs = do
  constant <- addConstantToCs (StringVal name) cs
  emitOpCode OP_GET_VAR cs
  emitByte constant cs
--
compileExpr Nil _ = return ()
--
compileExpr (BuiltinFn name args) cs = do
  compileExprs args cs
  compileExpr (Lit (IntVal (length args))) cs
  constant <- addConstantToCs (StringVal name) cs
  emitOpCode OP_CALL_FN cs
  emitByte constant cs
--
compileExpr (LabelRef lblName scoped) cs = do
  scopedLabel <- if scoped then toScopedLabel lblName cs else return lblName
  constant <- addConstantToCs (IntVal 0) cs
  emitOpCode OP_CONSTANT cs
  chunkCount <- curChunkCount cs
  addLabelRefPatch chunkCount scopedLabel cs
  emitByte constant cs
--
compileExpr ex _ = error $ "cannot compile expression `" ++ show ex ++ "` yet"

-- Patches
patchJump :: Int -> CompState -> IO ()
patchJump offset cs = do
  chunkCount <- curChunkCount cs
  let jump = chunkCount - offset - 1
  patchChunkCode offset jump cs

patchJumps :: CompState -> IO ()
patchJumps cs = do
  labelMap <- getLabelOffsetMap cs
  jumps <- getJumpPatches cs
  forM_ jumps $ \(curOffset, label) -> do
    let jumpToInstr = labelMap `readMap` label
    let jumpOffset = jumpToInstr - curOffset - 2
    jumpOffset `seq`
      patchChunkCode (curOffset + 1) jumpOffset cs

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

patchLoop :: LoopPatch -> CompState -> IO ()
patchLoop (LoopPatch {stepStart, exitJump, nextLabel}) cs = do
  emitLoop stepStart cs
  patchJump exitJump cs
  emitOpCode OP_POP cs
  case nextLabel of
    Just next -> compileStmt (Jump next) cs
    Nothing -> return ()

patchLabelRefs :: CompState -> IO ()
patchLabelRefs cs = do
  labelMap <- getLabelOffsetMap cs
  labelRefs <- getLabelRefPatches cs
  forM_ labelRefs $ \(curOffset, label) -> do
    let labelOffset = labelMap `readMap` label
    labelOffset `seq`
      patchChunkConstant curOffset (IntVal labelOffset) cs
