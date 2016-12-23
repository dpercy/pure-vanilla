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


showDefs :: [Def] -> Doc
showDefs defs = vcat $ intersperse "" $ map showDef defs

showDef :: Def -> Doc
showDef (Def x e) = sep [showId x <+> char '=', hang 2 (showExpr e)]

showExpr :: Expr -> Doc
-- TODO better handling of uprefs:
---- need a way to grab free variables of a subexpression
showExpr (Upref i) = "UP[" <> text (show i) <> "]"
showExpr (Var x) = showId x
showExpr (Prim op) = showId $ primName op
showExpr (Lit Null) = "null"
showExpr (Lit (Bool b)) = if b then "true" else "false"
showExpr (Lit (Integer i)) = text (show i)
showExpr (Lit (String s)) = text (show s)
showExpr (Lit (Symbol s)) = text (':':s)
showExpr (Perform eff) = "perform(" <> showExpr eff <> ")"
showExpr (Cons hd tl) = "cons(" <> showExpr hd <> ", " <> showExpr tl <> ")"
showExpr (Tag k v) = "tag(" <> showExpr k <> ", " <> showExpr v <> ")"
showExpr (Func params body) = sep [ params' <+> "->", hang 2 body' ]
  where params' = parens $ sep $ punctuate "," $ map showId params
        body' = showExpr body
showExpr (App (Func [x] body) [e]) = sep [ hsep [ "let", showId x, "=", showExpr e, "in" ]
                                         , showExpr body
                                         ]
showExpr (App f a) = case f of
  Prim op -> showInfix (primName op) a
  Var s | isOperator s -> showInfix s a
  _ -> showExpr f <> showArgs a
  where isOperator s = and $ map (not . isLetter) s
        
showExpr (If t c a) = sep [ "if" <+> showExpr t
                          , "then" <+> showExpr c
                          , "else" <+> showExpr a
                          ]
showExpr (Error msg) = "error(" <> text (show msg) <> ")"

showInfix :: String -> Expr -> Doc
showInfix op a = case parseExprList a of
  Nothing -> text op <> showArgs a
  Just a' -> hsep $ intersperse (text op) (map showExpr a')

-- TODO use flatAlt to have a different representation when one line vs multiline
---- good for eliding the final comma only for single line
---- good for changing newlines into semicolons
showArgs :: Expr -> Doc
showArgs a = case parseExprList a of
  Nothing -> "(*" <> showExpr a <> ")"
  -- TODO indent??
  Just a' -> parens $ sep $ punctuate "," $ map showExpr a'

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
