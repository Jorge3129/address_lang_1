{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import ByteCode
import CompilerUtils
import Control.Arrow ((>>>))
import Data.List (foldl')
import Data.Map (Map, empty, insert, (!))
import Data.Maybe (fromMaybe)
import Grammar
import MyUtils
import Value

type LabelOffsetMap = Map String Int

data LoopPatch = LoopPatch
  { scopeLabel :: Maybe String,
    nextLabel :: Maybe String,
    loopLine :: Int,
    stepStart :: Int,
    exitJump :: Int
  }
  deriving (Eq, Show)

data CompState = CompState
  { curChunk :: Chunk,
    curLine :: Int,
    labelOffsetMap :: LabelOffsetMap,
    labelJumpsToPatch :: [(Int, String)],
    loopPatches :: [LoopPatch]
  }
  deriving (Eq, Show)

initCs :: CompState
initCs =
  CompState
    { curChunk = initChunk,
      curLine = 0,
      labelOffsetMap = Data.Map.empty,
      labelJumpsToPatch = [],
      loopPatches = []
    }

compileProg :: Program -> IO Chunk
compileProg (Program {pLines}) = do
  let numLines = zip pLines [0 :: Int ..]
      cs = initCs
  cs1 <- compileLines numLines cs
  let cs2 = patchLabelJumps cs1
  return $ writeChunk (fromEnum OP_RETURN) (length pLines) (curChunk cs2)

compileLines :: [(ProgLine, Int)] -> CompState -> IO CompState
compileLines (l : ls) cs = compileLine l cs >>= compileLines ls
compileLines [] cs = return cs

compileLine :: (ProgLine, Int) -> CompState -> IO CompState
compileLine pl@(ProgLine {labels, stmts}, lineNum) cs = do
  let cs1 = compileLineLabels labels (cs {curLine = lineNum})
      cs2 = patchLoops (loopPatches cs1) pl cs1
  compileStmts stmts cs2

patchLoops :: [LoopPatch] -> (ProgLine, Int) -> CompState -> CompState
patchLoops (lp : lps) pl@(ProgLine {labels}, _) cs =
  let shouldPatch = fromMaybe "" (scopeLabel lp) `elem` labels
   in if shouldPatch
        then patchLoops lps pl (patchLoop lp (cs {loopPatches = lps}))
        else cs
patchLoops [] _ cs = cs

compileLineLabels :: [String] -> CompState -> CompState
compileLineLabels lbls cs@(CompState {labelOffsetMap, curChunk}) =
  let offset = length $ code $ curChunk
      newLblMap = foldl' (\lblMap lbl -> insert lbl offset lblMap) labelOffsetMap lbls
   in cs {labelOffsetMap = newLblMap}

compileStmts :: [Statement] -> CompState -> IO CompState
compileStmts (st : stmts) cs = compileStmt st cs >>= compileStmts stmts
compileStmts [] cs = return cs

compileStmt :: Statement -> CompState -> IO CompState
compileStmt Stop cs = return $ emitOpCode OP_RETURN cs
--
compileStmt (BuiltinFunc "print" [ex]) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_PRINT cs1
--
compileStmt (ExpSt ex) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_POP cs1
--
compileStmt (Send valEx addrEx) cs = do
  cs1 <- compileExpr valEx cs
  cs2 <- compileExpr addrEx cs1
  return $ emitOpCode OP_SEND cs2
--
compileStmt (Jump lbl) cs = do
  let cs1 = addLabelPatch (curChunkCount cs) lbl cs
      cs2 = emitOpCode OP_JUMP cs1
  return $ emitByte 0 cs2
--
compileStmt (Predicate ifExp thenStmts elseStmts) cs = do
  -- condition
  cs1 <- compileExpr ifExp cs
  let (cs2, toElseJump) = emitJump OP_JUMP_IF_FALSE cs1
  -- then clause
  let cs3 = emitOpCode OP_POP cs2
  cs4 <- compileStmts thenStmts cs3
  let (cs5, toEndJump) = emitJump OP_JUMP cs4
  -- else clause
  let cs6 = patchJump toElseJump cs5
      cs7 = emitOpCode OP_POP cs6
  cs8 <- compileStmts elseStmts cs7
  -- end
  return $ patchJump toEndJump cs8
--
compileStmt (LoopSimple startVal step end counter scope next) cs = do
  cs1 <- compileStmt (Send startVal counter) cs
  let loopStart = curChunkCount cs1
  cs2 <- compileExpr (getLoopEndExpr end counter) cs1
  let (cs3, exitJump) = emitJump OP_JUMP_IF_FALSE cs2
      cs4 = emitOpCode OP_POP cs3
      (cs5, bodyJump) = emitJump OP_JUMP cs4
      stepStart = curChunkCount cs5
  cs6 <- compileStmt (getLoopStepStmt step counter) cs5
  let cs7 = emitLoop loopStart cs6
      cs8 = patchJump bodyJump cs7
  return $
    addLoopPatch
      ( LoopPatch
          { scopeLabel = scope,
            nextLabel = next,
            stepStart = stepStart,
            exitJump = exitJump,
            loopLine = curLine cs8
          }
      )
      cs8
--
compileStmt st _ = error $ "cannot compile statement `" ++ show st ++ "` yet"

patchLoop :: LoopPatch -> CompState -> CompState
patchLoop (LoopPatch {stepStart, exitJump}) =
  emitLoop stepStart
    >>> patchJump exitJump
    >>> emitOpCode OP_POP

addLoopPatch :: LoopPatch -> CompState -> CompState
addLoopPatch p cs = cs {loopPatches = p : loopPatches cs}

compileExpr :: Expr -> CompState -> IO CompState
compileExpr (Lit val) cs = do
  let (cs1, constant) = addConstantToCs (IntVal val) cs
      cs2 = emitOpCode OP_CONSTANT cs1
  return $ emitByte constant cs2
--
compileExpr (BinOpApp op a b) cs = do
  cs1 <- compileExpr a cs
  cs2 <- compileExpr b cs1
  return $ emitOpCodes (binOpToOpCode op) cs2
--
compileExpr (Deref ex) cs = do
  cs1 <- compileExpr ex cs
  return $ emitOpCode OP_DEREF cs1
compileExpr ex _ = error $ "cannot compile expression `" ++ show ex ++ "` yet"

emitByte :: Int -> CompState -> CompState
emitByte byte cs@(CompState {curChunk, curLine}) =
  cs
    { curChunk = writeChunk (fromEnum byte) curLine curChunk
    }

emitOpCode :: OpCode -> CompState -> CompState
emitOpCode op = emitByte (fromEnum op)

emitOpCodes :: [OpCode] -> CompState -> CompState
emitOpCodes ops cs = foldl' (flip emitOpCode) cs ops

emitJump :: OpCode -> CompState -> (CompState, Int)
emitJump op cs =
  let cs1 = (emitOpCode op >>> emitByte 0) cs
   in (cs1, curChunkCount cs1 - 1)

emitLoop :: Int -> CompState -> CompState
emitLoop jumpToInstr cs =
  let jumpOffset = jumpToInstr - curChunkCount cs - 2
   in (emitOpCode OP_JUMP >>> emitByte jumpOffset) cs

patchJump :: Int -> CompState -> CompState
patchJump offset cs =
  let ch = curChunk cs
      jump = curChunkCount cs - offset - 1
      newCh = ch {code = replace offset jump (code ch)}
   in cs {curChunk = newCh}

addConstantToCs :: Value -> CompState -> (CompState, Int)
addConstantToCs val cs@(CompState {curChunk}) =
  let (newCh, constant) = addConstant val curChunk
   in (cs {curChunk = newCh}, constant)

addLabelPatch :: Int -> String -> CompState -> CompState
addLabelPatch curOffset lbl cs@(CompState {labelJumpsToPatch}) =
  cs
    { labelJumpsToPatch = labelJumpsToPatch ++ [(curOffset, lbl)]
    }

patchLabelJumps :: CompState -> CompState
patchLabelJumps cs@(CompState {labelJumpsToPatch, labelOffsetMap, curChunk}) =
  let newCh =
        foldl'
          ( \ch@(Chunk {code}) (curOffset, lbl) ->
              let jumpToInstr = labelOffsetMap ! lbl
                  jumpOffset = jumpToInstr - curOffset - 2
               in ch {code = replace (curOffset + 1) jumpOffset code}
          )
          curChunk
          labelJumpsToPatch
   in cs {curChunk = newCh}

curChunkCount :: CompState -> Int
curChunkCount = length . code . curChunk

binOpToOpCode :: BinOp -> [OpCode]
binOpToOpCode Add = [OP_ADD]
binOpToOpCode Sub = [OP_SUB]
binOpToOpCode Mul = [OP_MUL]
binOpToOpCode Div = [OP_DIV]
binOpToOpCode Equal = [OP_EQUAL]
binOpToOpCode Greater = [OP_GREATER]
binOpToOpCode Less = [OP_LESS]
binOpToOpCode NotEqual = [OP_EQUAL, OP_NOT]
binOpToOpCode GreaterEqual = [OP_LESS, OP_NOT]
binOpToOpCode LessEqual = [OP_GREATER, OP_NOT]
binOpToOpCode _ = undefined