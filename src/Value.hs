module Value where

data Value
  = IntVal Int
  | FloatVal Float
  | NilVal
  deriving (Eq)

asNum :: Value -> Float
asNum (IntVal v) = fromIntegral v :: Float
asNum (FloatVal v) = v
asNum _ = error "cannot convert value to number"

instance Show Value where
  show (IntVal v) = show v
  show (FloatVal v) = show v
  show NilVal = "Nil"