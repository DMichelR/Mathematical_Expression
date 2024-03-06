module Main (main) where

import ExpressionEvaluator
import Parser
import Text.Parsec ()

parseAndEval :: String -> Either String Int
parseAndEval input =
  case parseExpression input of
    Left err -> Left $ show err
    Right expr -> Right $ evalExpression expr

main :: IO ()
main = do
  putStrLn "Ingrese una expresión matemática:"
  input <- getLine
  case parseAndEval input of
    Left err -> putStrLn $ "Error: " ++ err
    Right result -> putStrLn $ "Resultado: " ++ show result
