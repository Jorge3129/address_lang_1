{-# LANGUAGE InstanceSigs #-}

module Value where

data Value
  = IntVal Int
  | DoubleVal Double
  | StringVal String
  | NilVal
  deriving (Eq)

instance Show Value where
  show (IntVal v) = show v
  show (DoubleVal v) = show v
  show (StringVal s) = s
  show NilVal = "N"

instance Ord Value where
  compare :: Value -> Value -> Ordering
  compare (IntVal a) (IntVal b) = compare a b
  compare (IntVal a) (DoubleVal b) = compare (fromIntegral a) b
  compare (DoubleVal a) (IntVal b) = compare a (fromIntegral b)
  compare (DoubleVal a) (DoubleVal b) = compare a b
  compare a b = error $ "cannot compare " ++ show a ++ " and " ++ show b

instance Num Value where
  (+) :: Value -> Value -> Value
  (+) = addV
  (*) :: Value -> Value -> Value
  (*) = mulV
  abs :: Value -> Value
  abs (IntVal v) = IntVal $ abs v
  abs (DoubleVal v) = DoubleVal $ abs v
  abs _ = undefined
  signum :: Value -> Value
  signum (IntVal v) = IntVal $ signum v
  signum (DoubleVal v) = DoubleVal $ signum v
  signum _ = undefined
  fromInteger :: Integer -> Value
  fromInteger v = IntVal $ fromInteger v
  negate :: Value -> Value
  negate (IntVal v) = IntVal $ -1 * v
  negate (DoubleVal v) = DoubleVal $ -1 * v
  negate _ = undefined

instance Fractional Value where
  fromRational :: Rational -> Value
  fromRational v = DoubleVal $ fromRational v
  (/) :: Value -> Value -> Value
  (/) = divV

asInt :: Value -> Int
asInt (IntVal v) = v
asInt v = error $ "the value " ++ show v ++ " is not an integer"

isFalsy :: Value -> Bool
isFalsy (IntVal v) = v == 0
isFalsy (DoubleVal v) = v == 0
isFalsy _ = error "isFalsy not implemented"

asNum :: Value -> Double
asNum (IntVal v) = fromIntegral v :: Double
asNum (DoubleVal v) = v
asNum _ = error "cannot convert value to number"

addV :: Value -> Value -> Value
addV (IntVal a) (IntVal b) = IntVal $ a + b
addV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a + b
addV (DoubleVal a) (IntVal b) = DoubleVal $ a + fromIntegral b
addV (DoubleVal a) (DoubleVal b) = DoubleVal $ a + b
addV _ _ = error "cannot add"

subV :: Value -> Value -> Value
subV (IntVal a) (IntVal b) = IntVal $ a - b
subV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a - b
subV (DoubleVal a) (IntVal b) = DoubleVal $ a - fromIntegral b
subV (DoubleVal a) (DoubleVal b) = DoubleVal $ a - b
subV _ _ = error "cannot sub"

mulV :: Value -> Value -> Value
mulV (IntVal a) (IntVal b) = IntVal $ a * b
mulV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a * b
mulV (DoubleVal a) (IntVal b) = DoubleVal $ a * fromIntegral b
mulV (DoubleVal a) (DoubleVal b) = DoubleVal $ a * b
mulV _ _ = error "cannot mul"

divV :: Value -> Value -> Value
divV (IntVal a) (IntVal b) = IntVal $ a `div` b
divV (IntVal a) (DoubleVal b) = DoubleVal $ fromIntegral a / b
divV (DoubleVal a) (IntVal b) = DoubleVal $ a / fromIntegral b
divV (DoubleVal a) (DoubleVal b) = DoubleVal $ a / b
divV _ _ = error "cannot div"
