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

addParams :: [String] -> Env -> Env
-- TODO fix shadowing here
addParams ps env = env{ locals = ps ++ locals env }

lookupUpref :: Int -> Env -> String
lookupUpref up env =
  if up < length (locals env)
  then (locals env)!!up
  else error "malformed core term: upref exceeds binding depth: " ++ (show up)


showDefs :: [Def] -> Doc
showDefs defs = vcat $ intersperse "" $ map (showDef env) defs
  where env = toplevelEnv (map (\(Def lhs _) -> lhs) defs)

showDef :: Env -> Def -> Doc
showDef env (Def x e) = showId x <+> char '=' <+> hang 2 (showExpr env e)

showExpr :: Env -> Expr -> Doc
showExpr env (Upref i) = showId (lookupUpref i env)
showExpr _   (Var x) = showId x
showExpr _   (Prim op) = showId $ primName op
showExpr _   (Lit Null) = "null"
showExpr _   (Lit (Bool b)) = if b then "true" else "false"
showExpr _   (Lit (Integer i)) = text (show i)
showExpr _   (Lit (String s)) = text (show s)
showExpr _   (Lit (Symbol s)) = text (':':s)
showExpr env (Perform eff) = "perform(" <> showExpr env eff <> ")"
showExpr env (Cons hd tl) = "cons(" <> showExpr env hd <> ", " <> showExpr env tl <> ")"
showExpr env (Tag k v) = "tag(" <> showExpr env k <> ", " <> showExpr env v <> ")"
showExpr env (Func params body) = sep [ params' <+> "->", body' ]
  where env' = addParams params env
        params' = parens $ sep $ punctuate "," $ map showId params
        body' = showExpr env' body
showExpr env (App (Func [x] body) [e]) = sep [ hsep [ "let", showId x, "=", showExpr env e, "in" ]
                                         , showExpr env body
                                         ]
showExpr env (App f a) = case f of
  Prim op -> showInfix env (primName op) a
  Var s | isOperator s -> showInfix env s a
  _ -> showExpr env f <> showArgs env a
  where isOperator s = and $ map (not . isLetter) s
        
showExpr env (If t c a) = sep [ "if" <+> showExpr env t
                          , "then" <+> showExpr env c
                          , "else" <+> showExpr env a
                          ]
showExpr _   (Error msg) = "error(" <> text (show msg) <> ")"

showInfix :: Env -> String -> Expr -> Doc
showInfix env op a = case parseExprList a of
  Nothing -> text op <> showArgs env a
  Just a' -> hsep $ intersperse (text op) (map (showExpr env) a')

-- TODO use flatAlt to have a different representation when one line vs multiline
---- good for eliding the final comma only for single line
---- good for changing newlines into semicolons
showArgs :: Env -> Expr -> Doc
showArgs env a = case parseExprList a of
  Nothing -> "(*" <> showExpr env a <> ")"
  -- TODO indent??
  Just a' -> parens $ sep $ punctuate "," $ map (showExpr env) a'

primName :: PrimFunc -> String
primName OpIsEmpty = "isEmpty"
primName OpIsCons = "isCons"
primName OpFirst = "first"
primName OpRest = "rest"
primName OpIsTagged = "isTagged"
primName OpUntag = "untag"
primName OpPlus = "+"
primName OpMinus = "-"
primName OpTimes = "*"
primName OpLessThan = "<"



showId :: String -> Doc
showId x = if and (map isLetter x) then text x
           else char '(' <> text x <> char ')'

prop_wc_reparse =
  (parseProgram $ show $ showDefs wc)
  == wc

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
