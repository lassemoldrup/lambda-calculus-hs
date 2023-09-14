module Eval (eval) where

import Data.Set (Set, delete, member, singleton, union)
import Lexer (Ident)
import Parser (Term (Abs, App, Var))

-- Normal order beta reduction
eval :: Term -> Term
eval (Var var) = Var var
eval (Abs var body) = Abs var $ eval body
eval (App fun arg) = case evalToAbs fun of
  Abs var body -> eval $ replace body var arg
  term -> App term $ eval arg

evalToAbs :: Term -> Term
evalToAbs (Var var) = Var var
evalToAbs (Abs var body) = Abs var body
evalToAbs (App fun arg) = case evalToAbs fun of
  Abs var body -> evalToAbs $ replace body var arg
  term -> App term $ eval arg

replace :: Term -> Ident -> Term -> Term
replace (Var var') var term
  | var' == var = term
  | otherwise = Var var'
replace (Abs var' body) var term
  | var' == var = Abs var' body
  | var' `member` freeVars term = Abs (prime var') $ replace (rename body var') var term
  | otherwise = Abs var' $ replace body var term
replace (App fun arg) var term = App (replace fun var term) (replace arg var term)

-- Replace all free occurences of var in term with primed var
rename :: Term -> Ident -> Term
rename term var = replace term var $ Var $ prime var

prime :: Ident -> Ident
prime var = var ++ "'"

freeVars :: Term -> Set Ident
freeVars (Var var) = singleton var
freeVars (Abs var body) = delete var $ freeVars body
freeVars (App fun arg) = freeVars fun `union` freeVars arg