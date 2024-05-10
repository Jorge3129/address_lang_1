module Compiler.Utils where

import ByteCode.Core
import Parser.AST

-- Binary ops
binOpToOpCode :: BinOp -> [OpCode]
binOpToOpCode Add = [OP_ADD]
binOpToOpCode Sub = [OP_SUB]
binOpToOpCode Mul = [OP_MUL]
binOpToOpCode Div = [OP_DIV]
binOpToOpCode Mod = [OP_MOD]
binOpToOpCode PtrAdd = [OP_PTR_ADD]
--
binOpToOpCode Equal = [OP_EQUAL]
binOpToOpCode Greater = [OP_GREATER]
binOpToOpCode Less = [OP_LESS]
binOpToOpCode NotEqual = [OP_EQUAL, OP_NOT]
binOpToOpCode GreaterEqual = [OP_LESS, OP_NOT]
binOpToOpCode LessEqual = [OP_GREATER, OP_NOT]
--
binOpToOpCode And = [OP_AND]
binOpToOpCode Or = [OP_OR]
binOpToOpCode MulDeref = [OP_MUL_DEREF]
binOpToOpCode MinDeref = [OP_MIN_DEREF]

unOpToOpcode :: UnOp -> [OpCode]
unOpToOpcode Negate = [OP_NEGATE]
unOpToOpcode Deref = [OP_DEREF]
unOpToOpcode Not = [OP_NOT]