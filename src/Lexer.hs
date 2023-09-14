module Lexer (tokenize, Token (..), Ident) where

import Data.Char (isAlpha, isAlphaNum, isSpace)

type Ident = String

data Token = Id Ident | Lambda | Dot | LParen | RParen deriving (Show)

tokenize :: String -> [Token]
tokenize ('\\' : tail) = Lambda : tokenize tail
tokenize ('λ' : tail) = Lambda : tokenize tail
tokenize ('.' : tail) = Dot : tokenize tail
tokenize ('(' : tail) = LParen : tokenize tail
tokenize (')' : tail) = RParen : tokenize tail
tokenize (ch : tail)
  | isSpace ch = tokenize tail
  | isAlpha ch =
      let (ident, identTail) = tokenizeId [ch] tail
       in Id ident : tokenize identTail
  | otherwise = error $ "Lexer error: Unrecognized character '" ++ ch : "'."
tokenize [] = []

tokenizeId :: Ident -> String -> (Ident, String)
tokenizeId ident (ch : tail)
  | isIdentEnd ch = (ident, ch : tail)
  | isAlphaNum ch = tokenizeId (ident ++ [ch]) tail
  | otherwise = error $ "Lexer error: Invalid character '" ++ ch : "' in identifier."
tokenizeId ident [] = (ident, [])

isIdentEnd :: Char -> Bool
isIdentEnd ch = ch == '\\' || ch == 'λ' || ch == '.' || ch == '(' || ch == ')' || isSpace ch