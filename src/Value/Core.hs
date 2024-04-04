{-# LANGUAGE InstanceSigs #-}

module Value.Core where

data Value
  = IntVal !Int
  | PointerVal !Int
  | DoubleVal !Double
  | StringVal !String
  | NilVal

instance Show Value where
  show (IntVal v) = show v
  show (PointerVal v) = "(Pointer " ++ show v ++ ")"
  show (DoubleVal v) = show v
  show (StringVal s) = s
  show NilVal = "N"

instance Eq Value where
  (==) :: Value -> Value -> Bool
  -- a + a
  (==) (IntVal a) (IntVal b) = a == b
  (==) (DoubleVal a) (DoubleVal b) = a == b
  (==) (PointerVal a) (PointerVal b) = a == b
  (==) (StringVal a) (StringVal b) = a == b
  (==) NilVal NilVal = True
  -- Int + Double
  (==) (IntVal a) (DoubleVal b) = fromIntegral a == b
  (==) (DoubleVal a) (IntVal b) = a == fromIntegral b
  -- Int + Pointer
  (==) (IntVal a) (PointerVal b) = a == b
  (==) (PointerVal a) (IntVal b) = a == b
  -- Pointer + Double
  (==) (PointerVal a) (DoubleVal b) = fromIntegral a == b
  (==) (DoubleVal a) (PointerVal b) = a == fromIntegral b
  (==) _ _ = False

-- (==) a b = error $ "cannot check equality for " ++ show a ++ " and " ++ show b

instance Ord Value where
  compare :: Value -> Value -> Ordering
  -- a + a
  compare (IntVal a) (IntVal b) = compare a b
  compare (DoubleVal a) (DoubleVal b) = compare a b
  compare (PointerVal a) (PointerVal b) = compare a b
  -- Int + Double
  compare (IntVal a) (DoubleVal b) = compare (fromIntegral a) b
  compare (DoubleVal a) (IntVal b) = compare a (fromIntegral b)
  -- Int + Pointer
  compare (IntVal a) (PointerVal b) = compare a b
  compare (PointerVal a) (IntVal b) = compare a b
  -- Pointer + Double
  compare (PointerVal a) (DoubleVal b) = compare (fromIntegral a) b
  compare (DoubleVal a) (PointerVal b) = compare a (fromIntegral b)
  compare a b = error $ "cannot compare " ++ show a ++ " and " ++ show b

instance Num Value where
  (+) :: Value -> Value -> Value
  (+) = addV
  (*) :: Value -> Value -> Value
  (*) = mulV
  abs :: Value -> Value
  abs (IntVal v) = IntVal $ abs v
  abs (PointerVal v) = PointerVal $ abs v
  abs (DoubleVal v) = DoubleVal $ abs v
  abs _ = undefined
  signum :: Value -> Value
  signum (IntVal v) = IntVal $ signum v
  signum (PointerVal v) = PointerVal $ signum v
  signum (DoubleVal v) = DoubleVal $ signum v
  signum _ = undefined
  fromInteger :: Integer -> Value
  fromInteger v = IntVal $ fromInteger v
  negate :: Value -> Value
  negate (IntVal v) = IntVal $ -1 * v
  negate (PointerVal v) = PointerVal $ -1 * v
  negate (DoubleVal v) = DoubleVal $ -1 * v
  negate _ = undefined

instance Fractional Value where
  fromRational :: Rational -> Value
  fromRational v = DoubleVal $ fromRational v
  (/) :: Value -> Value -> Value
  (/) = divV

asInt :: Value -> Int
asInt (IntVal v) = v
asInt (PointerVal v) = v
asInt v = error $ "the value " ++ show v ++ " is not an integer"

isPointer :: Value -> Bool
isPointer (PointerVal _) = True
isPointer _ = False

asPointer :: Value -> Value
asPointer (PointerVal v) = PointerVal v
asPointer (IntVal v) = PointerVal v
asPointer v = error $ "cannot convert value " ++ show v ++ " to pointer"

asStr :: Value -> String
asStr (StringVal v) = v
asStr v = error $ "the value " ++ show v ++ " is not a string"

isFalsy :: Value -> Bool
isFalsy (IntVal v) = v == 0
isFalsy (PointerVal v) = v == 0
isFalsy (DoubleVal v) = v == 0
isFalsy _ = error "isFalsy not implemented"

addV :: Value -> Value -> Value
addV (IntVal a) (IntVal b) = IntVal $ a + b
addV (DoubleVal a) (DoubleVal b) = DoubleVal $ a + b
addV (PointerVal a) (PointerVal b) = IntVal $ a + b
--
addV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a + b
addV (DoubleVal a) (IntVal b) = DoubleVal $ a + fromIntegral b
--
addV (IntVal a) (PointerVal b) = IntVal $ a + b
addV (PointerVal a) (IntVal b) = IntVal $ a + b
--
addV a b = error $ "cannot add " ++ show a ++ " and " ++ show b

subV :: Value -> Value -> Value
subV (IntVal a) (IntVal b) = IntVal $ a - b
subV (DoubleVal a) (DoubleVal b) = DoubleVal $ a - b
--
subV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a - b
subV (DoubleVal a) (IntVal b) = DoubleVal $ a - fromIntegral b
subV a b = error $ "cannot subtract " ++ show a ++ " and " ++ show b

mulV :: Value -> Value -> Value
mulV (IntVal a) (IntVal b) = IntVal $ a * b
mulV (DoubleVal a) (DoubleVal b) = DoubleVal $ a * b
--
mulV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a * b
mulV (DoubleVal a) (IntVal b) = DoubleVal $ a * fromIntegral b
mulV a b = error $ "cannot multiply " ++ show a ++ " and " ++ show b

divV :: Value -> Value -> Value
divV (IntVal a) (IntVal b) = DoubleVal $ fromIntegral a / fromIntegral b
-- divV (IntVal a) (IntVal b) = IntVal $ a `div` b
divV (DoubleVal a) (DoubleVal b) = DoubleVal $ a / b
--
divV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a / b
divV (DoubleVal a) (IntVal b) = DoubleVal $ a / fromIntegral b
divV a b = error $ "cannot divide " ++ show a ++ " and " ++ show b
