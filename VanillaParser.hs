{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts #-}
module VanillaParser where

import VanillaCore hiding (main)
import Test.QuickCheck
import Text.Parsec
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

program :: Parser [Def]
program = sepBy def (char ';')

def :: Parser Def
def = do
  lhs <- iden
  char '='
  rhs <- expr
  return $ Def lhs rhs

iden :: Parser String
iden = do
  hd <- letter
  tl <- many (letter <|> digit)
  return $ hd:tl

expr :: Parser Expr
expr = (Var `liftM` iden)
       <|> literal

literal :: Parser Expr
literal = (Lit . Integer . read) `liftM` many1 digit



prop_empty =
  parse program "<in>" "" == Right []
  
prop_example =
  parse program "<in>" "x = 1 ; y = 2"
   == Right [ Def "x" (Lit $ Integer 1)
            , Def "y" (Lit $ Integer 2)
            ]

prop_ops =
  parse program "<in>" "f = (++, <|>) -> (<|> (1 ++ 2))"
  == Right [ Def "f" (Func ["++", "<|>"]
                      (App (Var "<|>")
                       [(App (Var "++")
                         [(Lit $ Integer 1), (Lit $ Integer 2)])]))
           ]


-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
main :: IO ()
main = $quickCheckAll >> return ()
