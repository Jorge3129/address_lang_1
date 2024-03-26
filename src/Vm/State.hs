{-# LANGUAGE NamedFieldPuns #-}

module Vm.State where

import ByteCode.Core
import Data.Map (Map, empty)
import Value.Core

data VM = VM
  { chunk :: !Chunk,
    ip :: !Int,
    stack :: ![Value],
    memory :: ![Value],
    varsMap :: !(Map String Int),
    vmCalls :: ![(String, Int)]
  }
  deriving (Eq, Show)

-- data VM = VM
--   { chunk :: Chunk,
--     ip :: Int,
--     stack :: [Value],
--     memory :: [Value],
--     varsMap :: Map String Int,
--     vmCalls :: [(String, Int)]
--   }
--   deriving (Eq, Show)

memMax :: Int
memMax = 2000

initVM :: Chunk -> VM
initVM ch =
  VM
    { chunk = ch,
      ip = 0,
      stack = [],
      memory = replicate memMax NilVal,
      varsMap = Data.Map.empty,
      vmCalls = []
    }

-- TODO change arg order
push :: VM -> Value -> VM
push vm@(VM {stack}) val =
  vm {stack = val : stack}

pop :: VM -> (Value, VM)
pop vm@(VM {stack}) =
  ( head stack,
    vm {stack = tail stack}
  )

peek :: Int -> VM -> Value
peek offset (VM {stack}) = stack !! offset

addIp :: Int -> VM -> VM
addIp ipOffset vm@(VM {ip}) = vm {ip = ip + ipOffset}

readByte :: VM -> (Int, VM)
readByte vm@(VM {ip, chunk}) =
  let (Chunk {code}) = chunk
      instruction = code !! ip
      newVm = vm {ip = ip + 1}
   in (instruction, newVm)

readConst :: VM -> (Value, VM)
readConst vm@(VM {chunk}) =
  let (Chunk {constants}) = chunk
      (constPos, newVm) = readByte vm
   in (constants !! constPos, newVm)