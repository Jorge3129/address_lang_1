module Value.WordUtils where

import Data.Bits
import Data.Word
import Value.Core

data ValueTag = IntTag | PointerTag | NilTag
  deriving (Eq, Enum, Show)

tagBits :: Int
tagBits = 3

valueBits :: Int
valueBits = 64 - tagBits

valueToWord64 :: Value -> Word64
valueToWord64 (IntVal n) = encodeValue IntTag (fromIntegral n)
valueToWord64 (PointerVal n) = encodeValue PointerTag (fromIntegral n)
valueToWord64 NilVal = encodeValue NilTag 0
valueToWord64 _ = error "Conversion to Word64 not implemented for this type"

word64ToValue :: Word64 -> Value
word64ToValue word
  | tag == IntTag = IntVal (fromIntegral value)
  | tag == PointerTag = PointerVal (fromIntegral value)
  | tag == NilTag = NilVal
  | otherwise = error "Conversion from Word64 not implemented for this type"
  where
    tag = toEnum (fromIntegral $ word .&. mask tagBits) :: ValueTag
    value = word `shiftR` tagBits

encodeValue :: ValueTag -> Word64 -> Word64
encodeValue tag value = (value `shiftL` tagBits) .|. fromIntegral (fromEnum tag)

mask :: Int -> Word64
mask n = complement (complement 0 `shiftL` n)