{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts #-}
module VanillaParser where

import VanillaCore hiding (main)
import Test.QuickCheck
import Text.Parsec hiding (token)
import Control.Monad
import Data.Functor.Identity

{-

program := def ; ...
def := id = expr

expr :=
literal
iden
( op )                -- like iden
expr ( expr , ... )   -- function call
( expr op ... )       -- also a function call

( iden-or-op , ...) -> expr  -- function literal

let iden = expr0 in expr1  -- sugar for ((iden) -> expr1)(expr0)



-}

type Parser = ParsecT [Char] () Identity

{-
TODO distinguish ast parsers from token parsers:
don't have to use Parsec's GenTokenParser thing,
but instead just define tok_id etc.
-}

token :: Parser a -> Parser a
token p = do
  v <- p
  spaces
  return v

tok_equals = token $ char '='
tok_id = token $ many1 letter -- TODO digit, underscore...
tok_op = token $ many1 (oneOf "~!@#$%^&*-=+|\\:<>/?")
tok_semicolon = token $ char ';'
tok_openParen = token $ char '('
tok_closeParen = token $ char ')'
tok_comma = token $ char ','
tok_arrow = token $ string "->"
parens = between tok_openParen tok_closeParen

program :: Parser [Def]
program = do
  spaces
  sepBy def tok_semicolon

def :: Parser Def
def = do
  lhs <- tok_id
  tok_equals
  rhs <- expr
  return $ Def lhs rhs

expr :: Parser Expr
expr = do
  primary <- (literal
              <|> Var `liftM` tok_id
              <|> lambda
              <|> try (Var `liftM` parens tok_op)
              <|> prefixOp
              <|> parens expr)
  call primary <|> infixOp primary <|> return primary

literal :: Parser Expr
literal = token $ (Lit . Integer . read) `liftM` many1 digit

lambda :: Parser Expr
lambda = do p <- try $ do p <- params
                          tok_arrow
                          return p
            e <- expr
            return $ Func p e
    where params = parens (param `sepBy` tok_comma)
          param = tok_id <|> tok_op

call :: Expr -> Parser Expr
call callee = do args <- parens (expr `sepBy` tok_comma)
                 return $ App callee (foldr Cons (Lit Null) args)

prefixOp :: Parser Expr
prefixOp = do op <- tok_op
              arg <- expr
              return $ App (Var op) [arg]

infixOp arg0 = do op <- tok_op
                  arg1 <- expr
                  return $ App (Var op) [arg0, arg1]


pp s = case parse program "<in>" s of
  Left err -> error $ show err
  Right v -> v

prop_empty =
  pp "" == []
  
prop_example =
  pp " x = 1 ; y = 2 "
   == [ Def "x" (Lit $ Integer 1)
      , Def "y" (Lit $ Integer 2)
      ]

prop_func =
  pp "f = () -> 1"
  == [ Def "f" (Func [] (Lit $ Integer 1)) ]

prop_params =
  pp "f = (x, y, ++) -> 1"
  == [ Def "f" (Func ["x", "y", "++"] (Lit $ Integer 1)) ]

prop_call =
  pp "v = f(x, 1)"
  == [ Def "v" (App (Var "f") [(Var "x"), (Lit $ Integer 1)]) ]

prop_prefix =
  pp "f = () -> (- x)"
  == [  Def "f" (Func []
                 (App (Var "-") [Var "x"]))
     ]

prop_ops =
  pp "f = (++, <|>) -> (<|> (1 ++ 2))"
  == [ Def "f" (Func ["++", "<|>"]
                (App (Var "<|>")
                 [(App (Var "++")
                   [(Lit $ Integer 1), (Lit $ Integer 2)])]))
     ]


-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
main :: IO ()
main = $quickCheckAll >> return ()
