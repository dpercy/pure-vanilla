{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts #-}
module VanillaParser where

import VanillaCore hiding (main)

import Test.QuickCheck
import Text.Parsec hiding (token, space, spaces, newline)
import Control.Monad
import Data.Functor.Identity
import Data.Char

{-

Design principles for syntax:
- each punctuation should have 1 meaning
- use conventional notation where possible
- support auto-indentation
  - consequence: indentation must not be significant, but newlines can be.

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

special functions:
- perform
- tag
- cons


-}

type Parser = ParsecT [Char] () Identity

space :: Parser Char
space = satisfy (\c -> isSpace c && c /= '\n')

spaces :: Parser ()
spaces = skipMany space

(&) = flip (<?>)

token :: Parser a -> Parser a
token p = do
  v <- p
  spaces
  return v

tok_equals = token $ char '='
tok_id = token $ many1 letter -- TODO digit, underscore...
tok_sym = token $ char ':' >> many1 letter
tok_op = token $ many1 (oneOf "~!@#$%^&*-=+|\\<>/?")
tok_semicolon = token $ char ';'
tok_newline = (many1 $ token $ char '\n') >> return '\n'
tok_openParen = token $ char '('
tok_closeParen = token $ char ')'
tok_comma = token $ char ','
tok_arrow = token $ string "->"
tok_int = token $ read `liftM` many1 digit
parens = between tok_openParen tok_closeParen

program :: Parser [Def]
program = "program" & do
  many (space <|> tok_newline)
  defs <- def `sepEndBy` (tok_semicolon <|> tok_newline)
  eof
  return defs

def :: Parser Def
def = "definition" & do
  lhs <- variable
  tok_equals
  rhs <- expr
  return $ Def lhs rhs

variable :: Parser String
variable = try $ do
  v <- tok_id
  guard (not $ v `elem` keywords)
  return v

keyword :: String -> Parser String
keyword kw = try $ do
  v <- tok_id
  guard (v == kw)
  return v

keywords :: [String]
keywords = [ "let", "in", "if", "then", "else" ]


expr :: Parser Expr
expr = "expression" & do
  primary <- (literal
              <|> Var `liftM` variable
              <|> lambda
              <|> try (Var `liftM` parens tok_op)
              <|> prefixOp
              <|> parens expr
              <|> letExpr
              <|> ifExpr)
  call primary <|> infixOp primary <|> return primary
-- TODO resolve ambiguities like f = - 1 + 2

literal :: Parser Expr
literal = "literal" & (num <|> sym)
  where num = do n <- tok_int
                 return (Lit $ Integer n)
        sym = do s <- tok_sym
                 return (Lit $ Symbol s)
          

lambda :: Parser Expr
lambda = do p <- try $ do p <- params
                          tok_arrow
                          optional tok_newline
                          return p
            e <- expr
            -- Use the 'func' smart constructor to convert
            -- Vars to Uprefs (de bruijn indices)
            return $ func p e
    where params = parens (param `sepEndBy` tok_comma)
          param = variable <|> tok_op

call :: Expr -> Parser Expr
call callee = do args <- parens (expr `sepEndBy` tok_comma)
                 return $ case callee of
                           Var op -> app op args
                           _ -> App callee (foldr Cons (Lit Null) args)

prefixOp :: Parser Expr
prefixOp = do op <- tok_op
              arg <- expr
              return $ app op [arg]

infixOp arg0 = do op <- tok_op
                  arg1 <- expr
                  return $ app op [arg0, arg1]

letExpr :: Parser Expr
letExpr = do keyword "let"
             v <- variable
             tok_equals
             e <- expr
             keyword "in" ; optional tok_newline
             b <- expr
             return $ (App (func [v] b) [e])

-- since the else part is mandatory,
-- there should be no ambiguity when nesting ifs.
ifExpr :: Parser Expr
ifExpr = do keyword "if" ; t <- expr ; keyword "then" ; c <- expr ; optional tok_newline
            keyword "else" ; e <- expr ; optional tok_newline
            return $ If t c e

app :: String -> [Expr] -> Expr
app "perform" [e] = Perform e
app "cons" [x, y] = Cons x y
app "tag" [x, y] = Tag x y
app "+" [x, y] = App (Prim OpPlus) [x, y]
app "<" [x, y] = App (Prim OpLessThan) [x, y]
-- TODO more cases for more ops
app f a = App (Var f) (foldr Cons (Lit Null) a)

pp :: String -> [Def]
pp s = case parse program "<in>" s of
  Left err -> error $ show err
  Right v -> v

parseProgram = pp


prop_empty =
  pp "" == []
  
prop_example =
  pp " x = 1 ; y = :wut "
   == [ Def "x" 1
      , Def "y" (Lit $ Symbol "wut")
      ]

prop_newline =
  pp "x=1\ny=2"
  == [ Def "x" 1, Def "y" 2 ]
prop_newlines =
  pp "x=1\n\n\ny=2"
  == [ Def "x" 1, Def "y" 2 ]
prop_start_newlines =
  pp "  \n   \n\n \n  x = 1"
  == [Def "x" 1]
prop_end_newlines =
  pp "x=1     \n  \n  \n\n"
  == [Def "x" 1]
prop_only_newlines =
  pp "   \n \n \n\n  "
  == []

prop_func =
  pp "v = (x) -> cons(x, y)"
  == [ Def "v" (func ["x"] (Cons (Var "x") (Var "y"))) ]

prop_params =
  pp "f = (x, y, ++) -> 1"
  == [ Def "f" (func ["x", "y", "++"] 1) ]

prop_call =
  pp "v = f(x, 1)"
  == [ Def "v" (App (Var "f") [(Var "x"), 1]) ]

prop_prefix =
  pp "f = () -> (- x)"
  == [  Def "f" (func []
                 (App (Var "-") [Var "x"]))
     ]

prop_ops =
  pp "f = (++, <|>) -> (<|> (1 ++ 2))"
  == [ Def "f" (func ["++", "<|>"]
                (App (Var "<|>")
                 [(App (Var "++")
                   [1, 2])]))
     ]

prop_letin =
  pp "v = let x = 1 in (x + 2)"
  == [ Def "v" (App (func ["x"] (App (Prim OpPlus)
                                 [Var "x", 2]))
                [1]) ]

prop_special_functions =
  pp "f = perform(tag(1, cons(2, 3)))"
  == [ Def "f" (Perform (Tag 1 (Cons 2 3))) ]

prop_conditionals =
  pp "v = if 1 then 2 else if 3 then 4 else 5"
  == [ Def "v" (If 1 2 (If 3 4 5)) ]

prop_prims =
  pp "v = 1 < 2"
  == [ Def "v" (App (Prim OpLessThan) [1, 2]) ]


-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
main :: IO ()
main = $quickCheckAll >> return ()
