module Value where

data Value
  = IntVal Int
  | FloatVal Float
  | NilVal
  deriving (Eq)

addVals :: Value -> Value -> Value
addVals (IntVal a) (IntVal b) = IntVal $ a + b
addVals (IntVal a) (FloatVal b) = FloatVal $ (fromIntegral a :: Float) + b
addVals (FloatVal a) (IntVal b) = FloatVal $ a + (fromIntegral b :: Float)
addVals (FloatVal a) (FloatVal b) = FloatVal $ a + b
addVals NilVal _ = error "cannot add nil value"
addVals _ NilVal = error "cannot add nil value"

instance Show Value where
  show (IntVal v) = show v
  show (FloatVal v) = show v
  show NilVal = "Nil"