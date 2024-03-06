module Parser
  ( parseExpression,
  )
where

import ArithmeticExpression
import Control.Monad (void)
import Text.Parsec (ParseError, between, chainl1, chainr1, char, choice, digit, eof, many, many1, oneOf, parse, (<|>))
import Text.Parsec.String (Parser)

-- Definir el analizador para las expresiones matemáticas
parseExpression :: String -> Either ParseError Expression
parseExpression input = parse expression "" input

-- Gramática para las expresiones matemáticas
expression :: Parser Expression
expression = do
  Parser.spaces
  expr <- sumExpr
  eof
  return expr

-- Analizar una expresión de suma
sumExpr :: Parser Expression
sumExpr = chainl1 multExpr addOp

-- Analizar una expresión de multiplicación
multExpr :: Parser Expression
multExpr = chainl1 powExpr multOp

-- Analizar una expresión de potencia
powExpr :: Parser Expression
powExpr = chainr1 factorExpr powOp

-- Analizar un factor
factorExpr :: Parser Expression
factorExpr = parensExpr <|> valExpr

parensExpr :: Parser Expression
parensExpr = between (char '(') (char ')') sumExpr

-- Analizar un valor
valExpr :: Parser Expression
valExpr = Val <$> integer

-- Analizar un entero
integer :: Parser Int
integer = read <$> many1 digit

-- Analizar operadores de suma
addOp :: Parser (Expression -> Expression -> Expression)
addOp = choice [char '+' >> return Sum, char '-' >> return Sub]

-- Analizar operadores de multiplicación
multOp :: Parser (Expression -> Expression -> Expression)
multOp = char '*' >> return Mult

-- Analizar operadores de potencia
powOp :: Parser (Expression -> Expression -> Expression)
powOp = char '^' >> return Pow

spaces :: Parser ()
spaces = void $ many $ oneOf " \t"
