{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, OverloadedStrings #-}
module VanillaPrinter where

import VanillaCore
import VanillaParser (parseProgram)
import ExampleWordCount

import Test.QuickCheck
import Text.PrettyPrint.Leijen
import Data.Char
import Data.String
import Data.List
import Data.Ratio

instance IsString Doc where
  fromString = text

{-

What context do you need while pretty printing?
- given an upref, what display name is it bound to?
- given a binder, what name can I choose to avoid collisions?
  - so: which names are in scope?

-}

data Env = Env { globals :: [String]
               , locals :: [String]
               }
toplevelEnv :: [String] -> Env
toplevelEnv lhs = Env { globals = lhs ++ map primName allPrimops
                      , locals = []
                      }

incName :: String -> String
incName word =
  let rev = reverse word in
  let num = case reverse $ takeWhile isDigit rev of
             "" -> 0
             s -> read s
  in
  let base = reverse $ dropWhile isDigit rev in
  base ++ show (num + 1)

consNoCollide :: [String] -> String -> [String] -> [String]
consNoCollide ctx x xs = if x `elem` xs || x `elem` ctx
                         then consNoCollide ctx (incName x) xs
                         else x:xs

appendNoCollide :: [String] -> [String] -> [String] -> [String]
appendNoCollide ctx left right = foldr (consNoCollide ctx) right left

prop_appendNoCollide_ex1 = appendNoCollide [] ["x", "x", "y", "z"] ["z", "x"]
                           == ["x2", "x1", "y", "z1", "z", "x"]

prop_incName_ex1 = incName "x0" == "x1"
prop_incName_ex2 = incName "x9" == "x10"
prop_incName_ex3 = incName "x" == "x1"


addParams :: [String] -> Env -> ([String], Env)
-- TODO when shadowing, you can avoid renaming if the shadowed variable
--      doesn't appear free in the body.
addParams ps env =
  let (++) = appendNoCollide (globals env) in
  let env' = env{ locals = (reverse ps) ++ locals env } in
  (reverse (take (length ps) (locals env')), env')

lookupUpref :: Int -> Env -> String
lookupUpref up env =
  if up < length (locals env)
  then (locals env)!!up
  else error "malformed core term: upref exceeds binding depth: " ++ (show up)


showDefs :: [Def] -> Doc
showDefs defs = vcat $ intersperse "" $ map (showDef env) defs
  where env = toplevelEnv (map (\(Def lhs _) -> lhs) defs)

showDef :: Env -> Def -> Doc
showDef env (Def x e) = hang 2 $ showId x <+> char '=' <+> showExpr env e

se :: Env -> Expr -> Doc
se env expr = wrap (showExpr env expr)
  where wrap = case expr of
                -- leaf expressions don't need parens
                Upref _ -> id
                Var _ -> id
                Prim _ -> id
                Lit _ -> id
                -- constructor and effect calls don't need parens
                Perform _ -> id
                Cons _ _ -> id
                Tag _ _ -> id
                Error _ -> id
               -- lambdas and ifs need parens: they're low-precedence right-associative
                Func _ _ -> parens . align
                If _ _ _ -> parens . align
                -- function calls need parens in case they are an infix op
                App _ _ -> parens . align

showExpr :: Env -> Expr -> Doc
showExpr env (Upref i) = showId (lookupUpref i env)
showExpr _   (Var x) = showId x
showExpr _   (Prim op) = showId $ primName op
showExpr _   (Lit Null) = "null"
showExpr _   (Lit (Bool b)) = if b then "true" else "false"
showExpr _   (Lit (Num n)) = case denominator n of
  1 -> text $ (show $ numerator n)
  d -> text $ (show $ numerator n) ++ "/" ++ (show d)
showExpr _   (Lit (String s)) = text (show s)
showExpr _   (Lit (Symbol s)) = text (':':s)
showExpr env (Perform eff) = "perform(" <> se env eff <> ")"
showExpr env (Cons hd tl) = "cons(" <> se env hd <> ", " <> se env tl <> ")"
showExpr env (Tag k v) = "tag(" <> se env k <> ", " <> se env v <> ")"
showExpr env (Func params body) = hang 2 $ sep [ paramsDoc <+> "->", bodyDoc ]
  where (params', env') = addParams params env
        paramsDoc = parens $ sep $ punctuate "," $ map showId params'
        bodyDoc = se env' body
showExpr env (App (Func [x] body) [e]) = sep [ hsep [ "let", showId x', "=", se env e, "in" ]
                                         , se env' body
                                         ]
  where ([x'], env') = addParams [x] env
showExpr env (App f a) = case f of
  Prim op -> showInfix env (primName op) a
  Var s | not (isId s) -> showInfix env s a
  _ -> se env f <> showArgs env a
        
showExpr env (If t c a) = sep [ "if" <+> se env t
                          , "then" <+> se env c
                          , "else" <+> se env a
                          ]
showExpr _   (Error msg) = "error(" <> text (show msg) <> ")"

showInfix :: Env -> String -> Expr -> Doc
showInfix env op a = case parseExprList a of
  Nothing -> text op <> showArgs env a
  Just []  -> se env (Var op) <> showArgs env a
  Just [a0] -> text op <+> se env a0
  Just (a0:aa) -> sep docs
    where docs = a0':aa'
          a0' = se env a0
          aa' = zipWith (<+>) (repeat $ text op) (map (se env) aa)
  Just _ -> error "unreachable - why doesn't GHC see this?"


-- TODO use flatAlt to have a different representation when one line vs multiline
---- good for eliding the final comma only for single line
---- good for changing newlines into semicolons
showArgs :: Env -> Expr -> Doc
showArgs env a = case parseExprList a of
  Nothing -> "(*" <> se env a <> ")"
  Just a' -> parens $ align $ sep $ punctuate "," $ map (showExpr env) a'

isId :: String -> Bool
isId = and . map isIdChar
  where isIdChar c = isLetter c || isDigit c || c == '_'

showId :: String -> Doc
showId x = if isId x then text x
           else char '(' <> text x <> char ')'

prop_wc_reparse =
  (parseProgram $ show $ showDefs wc)
  == wc

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
