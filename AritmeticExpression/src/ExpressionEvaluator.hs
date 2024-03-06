module ExpressionEvaluator (evalExpression) where

import ArithmeticExpression

evalExpression :: Expression -> Int
evalExpression (Val n) = n
evalExpression (Sum x y) = evalExpression x + evalExpression y
evalExpression (Mult x y) = evalExpression x * evalExpression y
evalExpression (Pow x y) = evalExpression x ^ evalExpression y
evalExpression (Div x y) = evalExpression x `div` evalExpression y
evalExpression (Sub x y) = evalExpression x - evalExpression y
