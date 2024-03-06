module Lib
  ( someFunc,
  )
where

import ArithmeticExpression
import ExpressionEvaluator

someFunc :: IO ()
someFunc = print (evalExpression (Div (Val 8) (Val 2)))
