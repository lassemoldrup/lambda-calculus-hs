module Main where

import Eval (eval)
import Lexer (tokenize)
import Parser (parse)

main :: IO ()
main = do
  input <- getContents
  let tokens = tokenize input
  putStrLn $ "Lexing: " ++ show tokens
  let parsed = parse tokens
  putStrLn $ "Parsed: " ++ show parsed
  let result = eval parsed
  putStrLn $ "Result: " ++ show result
