module Parser (parse, Term (..)) where

import Lexer (Ident, Token)
import qualified Lexer (Token (..))

data Term = Var Ident | Abs Ident Term | App Term Term deriving (Show)

data TokenTree = Tree [TokenTree] | Id Ident | Lambda | Dot deriving (Show)

parse :: [Token] -> Term
parse = parseTokenTrees . intoTokenTrees

parseTokenTrees :: [TokenTree] -> Term
parseTokenTrees [] = error "Parser error: Empty term."
parseTokenTrees (Lambda : Id ident : Dot : tts) = Abs ident $ parseTokenTrees tts
parseTokenTrees (tt : tts) = parseApp (parseTokenTree tt) tts

parseTokenTree :: TokenTree -> Term
parseTokenTree (Tree tt) = parseTokenTrees tt
parseTokenTree (Id ident) = Var ident
parseTokenTree Lambda = error "Parser error: Unexpected '\\'."
parseTokenTree Dot = error "Parser error: Unexpected '.'."

parseApp :: Term -> [TokenTree] -> Term
parseApp acc [] = acc
parseApp acc (Lambda : Id ident : Dot : tts) = App acc $ Abs ident $ parseTokenTrees tts
parseApp acc (tt : tts) = parseApp (App acc $ parseTokenTree tt) tts

intoTokenTrees :: [Token] -> [TokenTree]
intoTokenTrees tks = case intoTokenTreesInner [] 0 tks of
  (tts, 0, []) -> tts
  (_, _, []) -> error "Parser error: Missing ')'."
  (_, _, _ : _) -> error "Parser error: Unexpected ')'."

intoTokenTreesInner :: [TokenTree] -> Int -> [Token] -> ([TokenTree], Int, [Token])
intoTokenTreesInner acc depth [] = (reverse acc, depth, [])
intoTokenTreesInner acc depth (Lexer.Id ident : tks) = intoTokenTreesInner (Id ident : acc) depth tks
intoTokenTreesInner acc depth (Lexer.Lambda : tks) = intoTokenTreesInner (Lambda : acc) depth tks
intoTokenTreesInner acc depth (Lexer.Dot : tks) = intoTokenTreesInner (Dot : acc) depth tks
intoTokenTreesInner acc depth (Lexer.LParen : tks) =
  let (tts, newDepth, rest) = intoTokenTreesInner [] (depth + 1) tks
   in intoTokenTreesInner (Tree tts : acc) newDepth rest
intoTokenTreesInner acc depth (Lexer.RParen : tks) = (reverse acc, depth - 1, tks)
