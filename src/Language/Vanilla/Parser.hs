{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts #-}
module Language.Vanilla.Parser where

import Language.Vanilla.Core

import Test.QuickCheck
import Text.Parsec hiding (token, space, spaces, newline, Error)
import Control.Monad
import Data.Functor.Identity
import Data.Char
import Data.Ratio
import Data.Maybe
import qualified Data.Map as Map

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

-- parses either a quote-number suffix, like '0 or '123,
-- or nothing.
-- This is used in identifiers, which have an optional suffix.
maybeNumSuffix :: Parser (Maybe Integer)
maybeNumSuffix = optionMaybe $ try $ do char '\''
                                        ds <- many1 digit
                                        return (read ds :: Integer)

tok_equals = token $ char '='
tok_id = token $ do
  let starter = letter <|> oneOf "_"
  let mid = starter <|> digit
  c <- starter
  cs <- many mid
  i <- maybeNumSuffix
  -- HAX ALERT: use -1 as a sentinel everywhere to mean "unspecified numeric suffix".
  -- These variables will be fixed when we do fixScope.
  return $ Var (c:cs) (fromMaybe (-1) i)
tok_op = token $ do
  cs <- many1 (oneOf "~!@#$%^&*-=+|\\<>/?")
  i <- maybeNumSuffix
  return $ Var cs (fromMaybe (-1) i)
tok_str = token $ do char '"'
                     s <- many (digit <|> letter <|> escapes <|> oneOf " ") -- TODO actual strings
                     char '"'
                     return s
  where escapes :: Parser Char
        escapes = try $ do char '\\'
                           c <- anyChar
                           case c of
                            't' -> return '\t'
                            'n' -> return '\n'
                            _ -> fail ("bad escape character: " ++ show c)
tok_semicolon = token $ char ';'
tok_newline = (many1 $ token $ char '\n') >> return '\n'
tok_openParen = token $ char '('
tok_closeParen = token $ char ')'
tok_openBracket = token $ char '['
tok_closeBracket = token $ char ']'
tok_comma = token $ char ','
tok_arrow = token $ string "->"
tok_int :: Parser Integer
tok_int = token $ read `liftM` many1 digit
tok_ratio :: Parser Rational
tok_ratio = token $ do n <- many1 digit
                       char '/'
                       d <- many1 digit
                       return (read n % read d)
tok_decimal :: Parser Rational
tok_decimal = token $ do realPart <- many1 digit
                         char '.'
                         fracPart <- many1 digit
                         return $ (fromInteger $ read realPart) + (read fracPart % 10 ^ length fracPart)
tok_colon = token $ char ':'

parens = between tok_openParen tok_closeParen

program :: Parser [Def]
program = "program" & do
  many (space <|> tok_newline)
  defs <- def `sepEndBy` (tok_semicolon <|> tok_newline)
  eof
  return defs

def :: Parser Def
def = "definition" & do
  Var x i <- variable
  guard (i == -1)
  tok_equals
  optional tok_newline
  e <- expr
  return $ Def x e

variable :: Parser Var
variable = try $ do
  v@(Var name _) <- tok_id
  guard (not $ name `elem` keywords)
  return v

keyword :: String -> Parser String
keyword kw = try $ do
  Var x i <- tok_id
  guard (i == -1)
  guard (x == kw)
  return x

keywords :: [String]
keywords = [ "let", "in", "if", "then", "else" ]


expr :: Parser Expr
expr = "expression" & (lambda
                       <|> quoteExpr
                       <|> letExpr
                       <|> ifExpr
                       <|> arith
                      )
-- A leaf is an expression that cannot be split.

-- Variables and literals are leaves,
-- because they consist of only one token.
    
-- A parenthesized expression is also a leaf because the
-- parens hold it together: if you tried to split it, you'd
-- get a paren mismatch error instead of an incorrect parse.
    
-- A lambda cannot be a leaf expression because
--  () -> 1 + 2
-- could be read as either () -> (1 + 2) or (() -> 1) + 2.
-- The same problem affects if-expressions and let-expressions.
leaf = literal
       <|> do v <- variable
              return $ Local v
       <|> do op <- try $ parens tok_op
              return $ Local op
       <|> parens (between (optional tok_newline) (optional tok_newline) expr)
       <|> do tok_openBracket
              tok_closeBracket
              -- TODO a more-general list notation here
              return (Lit Null)
factor = do head <- leaf
            -- TODO test curried calls like f(1)(2)
            calls head
              where calls head = (call head >>= calls) <|> return head
arith = prefixOp <|> infixes
 where prefixOp = do op <- try tok_op
                     arg <- factor
                     return $ app op [arg]
       infixes = do head <- factor
                    tails <- many (do op <- tok_op
                                      e <- factor
                                      return (op, e))
                    case tails of
                     [] -> return head
                     ((op, _):_) -> do guard $ all (== op) (map fst tails)
                                       return $ app op (head:(map snd tails))
                     _ -> error "unreachable (why does GHC not see this?)"

literal :: Parser Expr
literal = "literal" & (dec <|> frac <|> int <|> str)
  where dec = "decimal" & do n <- try tok_decimal
                             return (Lit $ Num $ n)
        frac = "fraction" & do n <- try tok_ratio
                               return (Lit $ Num $ n)
        int = "integer" & do n <- try tok_int
                             return (Lit $ Num $ fromInteger n)
        str = "string" & do s <- try tok_str
                            return (Lit $ String s)
                            
          

lambda :: Parser Expr
lambda = do p <- try $ do p <- params
                          tok_arrow
                          optional tok_newline
                          return p
            e <- expr
            return $ Func p e
    where params = parens (param `sepEndBy` tok_comma)
          param :: Parser Var
          param = variable <|> tok_op

call :: Expr -> Parser Expr
call callee = do args <- parens (between (optional tok_newline) (optional tok_newline)
                                 (expr `sepEndBy` (tok_comma >> optional tok_newline)))
                 return $ case callee of
                           Local op -> app op args
                           _ -> App callee (foldr Cons (Lit Null) args)

quoteExpr :: Parser Expr
quoteExpr = do tok_colon
               stx <- leaf
               return $ Quote stx

letExpr :: Parser Expr
letExpr = do keyword "let"
             v <- variable
             tok_equals
             e <- expr
             keyword "in" ; optional tok_newline
             b <- expr
             return $ (App (Func [v] b) [e])

-- since the else part is mandatory,
-- there should be no ambiguity when nesting ifs.
ifExpr :: Parser Expr
ifExpr = do keyword "if" ; t <- expr   ; optional tok_newline
            keyword "then" ; c <- expr ; optional tok_newline
            keyword "else" ; e <- expr
            return $ If t c e

app :: Var -> [Expr] -> Expr
app (Var "perform" (-1)) [e] = Perform e
app (Var "cons" (-1)) [x, y] = Cons x y
app (Var "tag" (-1)) [x, y] = Tag x y
app f a = App (Local f) (foldr Cons (Lit Null) a)

pp :: String -> [Def]
pp s = case parse program "<in>" s of
  Left err -> error $ show err
  Right v -> fixScope v

fixScope :: [Def] -> [Def]
fixScope = map fixDef
  where fixDef (Def x e) = Def x (fixExprScope emptyScope e)

fixExprScope scope@(InScope sc) e =
  let r = fixExprScope (InScope sc) in
   case e of
    Local (Var x (-1)) -> case Map.lookup x sc of
      Nothing -> globalOrPrim x
      Just i -> Local (Var x i)
    Local (Var x i) -> Local (Var x i)
    Func p b -> let (scope', p') = renameVars scope p in
                 Func p' (fixExprScope scope' b)
    Global _ -> error "fixScope assumes all ids are Local"
    Prim _ -> e
    Lit _ -> e
    Error _ -> e
    -- TODO enforce here my assumption that quoted syntax does not contain free variables.
    --- but remember that once you take-apart a syntax, it can become open...
    Quote stx -> Quote (fixExprScope emptyScope stx)
    Perform e0 -> Perform (r e0)
    Cons e0 e1 -> Cons (r e0) (r e1)
    Tag e0 e1 -> Tag (r e0) (r e1)
    App e0 e1 -> App (r e0) (r e1)
    If e0 e1 e2 -> If (r e0) (r e1) (r e2)

parseProgram s = fixScope `fmap` parse program "<in>" s

parseExpr s = fixExprScope emptyScope `fmap` parse expr "<in>" s

globalOrPrim :: String -> Expr
globalOrPrim name = case primByName name of
  Nothing -> Global name
  Just op -> Prim op


prop_empty =
  pp "" == []
  
prop_example =
  pp " x = 1 ; y = \"wut\" ; z = 1/2 ; q = 3.5"
   == [ Def "x" 1
      , Def "y" (Lit $ String "wut")
      , Def "z" (1/2)
      , Def "q" 3.5
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
  == [ Def "v" (Func [Var "x" 0] (Cons (Local (Var "x" 0)) (Global "y"))) ]

prop_params =
  pp "f = (x, y, ++) -> 1"
  == [ Def "f" (Func [Var "x" 0, Var "y" 0, Var "++" 0] 1) ]

prop_call =
  pp "v = f(x, 1)"
  == [ Def "v" (App (Global "f") [(Global "x"), 1]) ]

prop_call_curry =
  pp "v = f(x, 1)()(y)"
  == [ Def "v" (App (App (App (Global "f") [Global "x", 1]) []) [Global "y"]) ]

prop_prefix =
  pp "f = () -> (- x)"
  == [  Def "f" (Func []
                 (App (Prim OpMinus) [Global "x"]))
     ]

prop_ops =
  pp "f = (++, <|>) -> (<|> (1 ++ 2 ++ 3))"
  == [ Def "f" (Func [Var "++" 0, Var "<|>" 0]
                (App (Local (Var "<|>" 0))
                 [(App (Local (Var "++" 0))
                   [1, 2, 3])]))
     ]

prop_letin =
  pp "v = let x = 1 in (x + 2)"
  == [ Def "v" (App (Func [Var "x" 0] (App (Prim OpPlus)
                                       [Local (Var "x" 0), 2]))
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

prop_if_has_lower_precedence_than_infix =
  pp "v = if 1 then 2 else 3 + 4"
  == [ Def "v" (If 1 2 (App (Prim OpPlus) [3, 4])) ]

prop_if_has_lower_precedence_than_apply =
  pp "v = if 1 then 2 else f(3)"
  == [ Def "v" (If 1 2 (App (Global "f") [3])) ]

prop_if_has_lower_precedence_than_apply_then_infix =
  pp "v = if 1 then 2 else 3(4) + 5"
  == [ Def "v" (If 1 2 (App (Prim OpPlus) [App 3 [4], 5])) ]

prop_shadow =
  pp "f = (x) -> (x) -> x"
  == [ Def "f" (Func [Var "x" 0] (Func [Var "x" 1] (Local (Var "x" 1)))) ]

prop_quote =
  pp "stx = :( (x) -> x )"
  == [ Def "stx" (Quote (Func [Var "x" 0] (Local (Var "x" 0)))) ]

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
