module Lexer (tokenize, Token (..), Ident) where

import Data.Char (isAlpha, isAlphaNum, isSpace)

type Ident = String

data Token = Id Ident | Lambda | Dot | LParen | RParen deriving (Show)

tokenize :: String -> [Token]
tokenize = tokenizeInner []

tokenizeInner :: [Token] -> String -> [Token]
tokenizeInner acc [] = acc
tokenizeInner acc ('\\' : tail) = tokenizeInner (Lambda : acc) tail
tokenizeInner acc ('λ' : tail) = tokenizeInner (Lambda : acc) tail
tokenizeInner acc ('.' : tail) = tokenizeInner (Dot : acc) tail
tokenizeInner acc ('(' : tail) = tokenizeInner (LParen : acc) tail
tokenizeInner acc (')' : tail) = tokenizeInner (RParen : acc) tail
tokenizeInner acc (ch : tail)
  | isSpace ch = tokenizeInner acc tail
  | isAlpha ch =
      let (ident, identTail) = tokenizeId [ch] tail
       in tokenizeInner (Id ident : acc) identTail
  | otherwise = error $ "Lexer error: Unrecognized character '" ++ ch : "'."

tokenizeId :: Ident -> String -> (Ident, String)
tokenizeId ident (ch : tail)
  | isIdentEnd ch = (reverse ident, ch : tail)
  | isAlphaNum ch = tokenizeId (ch : ident) tail
  | otherwise = error $ "Lexer error: Invalid character '" ++ ch : "' in identifier."
tokenizeId ident [] = (reverse ident, [])

isIdentEnd :: Char -> Bool
isIdentEnd ch = ch == '\\' || ch == 'λ' || ch == '.' || ch == '(' || ch == ')' || isSpace ch