module ArithmeticExpression (Expression (..)) where

data Expression
  = Val Int
  | Sum Expression Expression
  | Mult Expression Expression
  | Pow Expression Expression
  | Div Expression Expression
  | Sub Expression Expression
  deriving (Show)
