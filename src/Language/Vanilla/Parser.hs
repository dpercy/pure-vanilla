{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts #-}
module Language.Vanilla.Parser (
  parsePrelude,
  parseProgram,
  parseProgram',
  parseExpr,
  test,
  ) where

import Language.Vanilla.Core
import Language.Vanilla.Prims (prims)
import Language.Vanilla.Eval (evalDefs)

import Test.QuickCheck
import Text.Parsec hiding (token, space, spaces, newline, Error)
import Control.Monad
import Data.Functor.Identity
import Data.Char
import Data.Ratio
import Data.Maybe
import qualified Data.Map as Map
import GHC.Exts (fromList)
import System.IO (withFile, IOMode(ReadMode), hGetContents)

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
spaces = skipMany (ignore space <|> ignore commentLine)
  where ignore p = p >> return ()

commentLine :: Parser String
commentLine = "comment " & do
  char '#'
  s <- many (noneOf "\n")
  char '\n'
  return s

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
                     s <- many (noneOf ['\\', '"', '\n'] <|> escapes)
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
tok_ellipsis = token $ string "..."

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
              optional tok_newline
              parseList

parseList :: Parser Expr
parseList = emptyCase <|> splatCase <|> itemCase
  where emptyCase = do tok_closeBracket
                       return (Lit Null)
        splatCase = do tok_ellipsis
                       e <- expr
                       optional tok_newline
                       tok_closeBracket
                       optional tok_comma
                       return e
        itemCase = do e <- expr
                      endNoComma e <|> commaAndContinue e
        endNoComma e = do optional tok_newline
                          tok_closeBracket
                          return (Cons e (Lit Null))
        commaAndContinue e = do tok_comma
                                optional tok_newline
                                rest <- parseList
                                return (Cons e rest)
factor = do head <- leaf
            calls head
              where calls head = (call head >>= calls) <|> return head
arith :: Parser Expr
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
                                 (commaSep expr))
                 return $ case callee of
                           Local op -> app op args
                           _ -> App callee (foldr Cons (Lit Null) args)

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepEndBy` (tok_comma >> optional tok_newline)

quoteExpr :: Parser Expr
quoteExpr = do tok_colon
               stx <- leaf
               return $ Quote stx

letExpr :: Parser Expr
letExpr = do keyword "let"
             binds <- commaSep $ do v <- variable
                                    tok_equals
                                    e <- expr
                                    return (v, e)
             let (vs, es) = unzip binds
             keyword "in" ; optional tok_newline
             b <- expr
             return $ (App (Func vs b) (fromList es))

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

parseProgram' :: Libs -> String -> [Def]
parseProgram' libs s = case parse (spaces >> program) "<in>" s of
  Left err -> error $ show err
  Right v -> fixScope libs v
pp = parseProgram' emptyLibs

-- fixScope resolves variable references.
--  - local variables become (Local (Var x n))
--  - global variables become (Global m x)
fixScope :: Libs -> [Def] -> [Def]
fixScope libs defs = map fixDef defs
  where fixDef (Def x e) = Def x (fixExprScope globals emptyScope e)
        globals = Map.insert "" (Map.fromList (map (\(Def x e) -> (x, e)) defs)) libs

fixExprScope :: Libs -> InScope -> Expr -> Expr
fixExprScope globals scope@(InScope sc) e =
  let r = fixExprScope globals (InScope sc) in
   case e of
    Local (Var x (-1)) -> case Map.lookup x sc of
      Just i -> Local (Var x i)
      Nothing -> resolveGlobal x globals
    Local (Var x i) -> Local (Var x i)
    Func p b -> let (scope', p') = renameVars scope p in
                 Func p' (fixExprScope globals scope' b)
    Global _ _ -> error "fixScope assumes all ids are Local (TODO add literal notation for globals)"
    Lit _ -> e
    Error _ -> e
    -- TODO enforce here my assumption that quoted syntax does not contain free variables.
    --- but remember that once you take-apart a syntax, it can become open...
    Quote stx -> Quote (fixExprScope globals emptyScope stx)
    Perform e0 -> Perform (r e0)
    Cons e0 e1 -> Cons (r e0) (r e1)
    Tag e0 e1 -> Tag (r e0) (r e1)
    App e0 e1 -> App (r e0) (r e1)
    If e0 e1 e2 -> If (r e0) (r e1) (r e2)

resolveGlobal :: String -> Libs -> Expr
-- zero matches: pretend it was (Global "" _)???
-- one match: resolve it
-- multiple matches: parse error...???
resolveGlobal x libs = case resolvePrim x ++ resolveInLibs x of
                        [] -> (Global "" x)
                        [m] -> (Global m x)
                        mods -> Error ("ambiguous variable " ++ x ++ " could be " ++ show mods)
  where resolvePrim :: String -> [String]
        resolvePrim x = case Map.lookup x prims of
          Nothing -> []
          Just _ -> ["Base"]
        resolveInLibs :: String -> [String]
        resolveInLibs x = do
          (libName, defs) <- Map.toList libs
          if x `elem` (Map.keys defs)
            then [libName]
            else []

parseProgram :: Libs -> String -> Either ParseError [Def]
parseProgram libs s = fixScope libs `fmap` parse (spaces >> program) "<in>" s

parseExpr :: Libs -> String -> Either ParseError Expr
parseExpr libs s = fixExprScope libs emptyScope `fmap` parse (spaces >> expr) "<in>" s

parsePrelude :: String -> IO Libs
parsePrelude file = do
  withFile file ReadMode $ \fh -> do
    contents <- hGetContents fh
    case parseProgram emptyLibs contents of
     Left err -> error (show err)
     Right program ->
       let program' = evalDefs emptyLibs program in
       let program'' = map (\(Def x e) -> (x, e)) program' in
       let program''' = Map.fromList program'' in
       return $ Map.fromList [("Prelude", program''')]

prop_empty = once $
  pp "" == []
  
prop_example = once $
  pp " x = 1 ; y = \"wut\" ; z = 1/2 ; q = 3.5"
   == [ Def "x" 1
      , Def "y" (Lit $ String "wut")
      , Def "z" (1/2)
      , Def "q" 3.5
      ]

prop_newline = once $
  pp "x=1\ny=2"
  == [ Def "x" 1, Def "y" 2 ]
prop_newlines = once $
  pp "x=1\n\n\ny=2"
  == [ Def "x" 1, Def "y" 2 ]
prop_start_newlines = once $
  pp "  \n   \n\n \n  x = 1"
  == [Def "x" 1]
prop_end_newlines = once $
  pp "x=1     \n  \n  \n\n"
  == [Def "x" 1]
prop_only_newlines = once $
  pp "   \n \n \n\n  "
  == []

prop_func = once $
  pp "v = (x) -> cons(x, y)"
  == [ Def "v" (Func [Var "x" 0] (Cons (Local (Var "x" 0)) (Global "" "y"))) ]

prop_params = once $
  pp "f = (x, y, ++) -> 1"
  == [ Def "f" (Func [Var "x" 0, Var "y" 0, Var "++" 0] 1) ]

prop_call = once $
  pp "v = f(x, 1)"
  == [ Def "v" (App (Global "" "f") [(Global "" "x"), 1]) ]

prop_call_curry = once $
  pp "v = f(x, 1)()(y)"
  == [ Def "v" (App (App (App (Global "" "f") [Global "" "x", 1]) []) [Global "" "y"]) ]

prop_prefix = once $
  pp "f = () -> (- x)"
  == [  Def "f" (Func []
                 (App (Global "Base" "-") [Global "" "x"]))
     ]

prop_ops = once $
  pp "f = (++, <|>) -> (<|> (1 ++ 2 ++ 3))"
  == [ Def "f" (Func [Var "++" 0, Var "<|>" 0]
                (App (Local (Var "<|>" 0))
                 [(App (Local (Var "++" 0))
                   [1, 2, 3])]))
     ]

prop_letin = once $
  pp "v = let x = 1 in (x + 2)"
  == [ Def "v" (App (Func [Var "x" 0] (App (Global "Base" "+")
                                       [Local (Var "x" 0), 2]))
                [1]) ]

prop_special_functions = once $
  pp "f = perform(tag(1, cons(2, 3)))"
  == [ Def "f" (Perform (Tag 1 (Cons 2 3))) ]

prop_conditionals = once $
  pp "v = if 1 then 2 else if 3 then 4 else 5"
  == [ Def "v" (If 1 2 (If 3 4 5)) ]

prop_prims = once $
  pp "v = 1 < 2"
  == [ Def "v" (App (Global "Base" "<") [1, 2]) ]

prop_if_has_lower_precedence_than_infix = once $
  pp "v = if 1 then 2 else 3 + 4"
  == [ Def "v" (If 1 2 (App (Global "Base" "+") [3, 4])) ]

prop_if_has_lower_precedence_than_apply = once $
  pp "v = if 1 then 2 else f(3)"
  == [ Def "v" (If 1 2 (App (Global "" "f") [3])) ]

prop_if_has_lower_precedence_than_apply_then_infix = once $
  pp "v = if 1 then 2 else 3(4) + 5"
  == [ Def "v" (If 1 2 (App (Global "Base" "+") [App 3 [4], 5])) ]

prop_shadow = once $
  pp "f = (x) -> (x) -> x"
  == [ Def "f" (Func [Var "x" 0] (Func [Var "x" 1] (Local (Var "x" 1)))) ]

prop_quote = once $
  pp "stx = :( (x) -> x )"
  == [ Def "stx" (Quote (Func [Var "x" 0] (Local (Var "x" 0)))) ]

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
