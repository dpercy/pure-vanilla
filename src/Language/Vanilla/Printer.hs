{-# LANGUAGE TemplateHaskell, OverloadedLists, OverloadedStrings #-}
module Language.Vanilla.Printer where

import Prelude hiding (showList)

import Language.Vanilla.Core

import Test.QuickCheck
import Text.PrettyPrint.Leijen
import Data.Char
import Data.String
import Data.List
import Data.Ratio
import GHC.Exts (toList)

instance IsString Doc where
  fromString = text


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

prop_appendNoCollide_ex1 = once $ appendNoCollide [] ["x", "x", "y", "z"] ["z", "x"]
                           == ["x2", "x1", "y", "z1", "z", "x"]

prop_incName_ex1 = once $ incName "x0" == "x1"
prop_incName_ex2 = once $ incName "x9" == "x10"
prop_incName_ex3 = once $ incName "x" == "x1"

showTrace :: [(String, [Expr])] -> Doc
showTrace trace = showDefs (concatMap f trace)
  where f (x, es) = map (Def x) es

showDefs :: [Def] -> Doc
showDefs defs = vcat $ intersperse "" $ map showDef defs

showDef :: Def -> Doc
showDef (Def x e) = hang 2 $ showGlobal x <+> char '=' <+> showExpr e

se :: Expr -> Doc
se expr = wrap (showExpr expr)
  where wrap = case expr of
                -- leaf expressions don't need parens
                Global _ -> id
                Local _ -> id
                Lit _ -> id
                -- constructor and effect calls don't need parens
                Perform _ -> id
                Cons _ _ -> id
                Tag _ _ -> id
                Error _ -> id
                Quote _ -> id
               -- lambdas and ifs need parens: they're low-precedence right-associative
                Func _ _ -> parens . align
                If _ _ _ -> parens . align
                -- function calls need parens in case they are an infix op
                App _ _ -> parens . align

showExpr :: Expr -> Doc
showExpr (Local v) = showVar v
showExpr (Global x) = showGlobal x
showExpr e@(Lit Null) = showList e
showExpr e@(Cons _ _) = showList e
showExpr (Lit (Bool b)) = if b then "true" else "false"
showExpr (Lit (Num n)) = case denominator n of
  1 -> text $ (show $ numerator n)
  d -> text $ (show $ numerator n) ++ "/" ++ (show d)
showExpr (Lit (String s)) = text (show s)
showExpr (Perform eff) = "perform(" <> se eff <> ")"
showExpr (Tag k v) = "tag(" <> se k <> ", " <> se v <> ")"
showExpr (Func params body) = hang 2 $ sep [ paramsDoc <+> "->", se body ]
  where paramsDoc = parens $ sep $ punctuate "," $ map showVar params
showExpr (App (Func xs body) es) = sep [ "let"
                                         <+> sep (punctuate ","
                                                  (zipWith showBind xs
                                                   (toList es)))
                                         <+> "in"
                                       , se body
                                       ]
  where showBind :: Var -> Expr -> Doc
        showBind x e = showVar x <+> "=" <+> se e
showExpr (App f a) = case f of
  -- TODO do this more generally
  Global "+" -> showInfix "+" a
  Global "-" -> showInfix "-" a
  Global "<" -> showInfix "<" a
  _ -> se f <> showArgs a
        
showExpr (If t c a) = sep [ "if" <+> se t
                          , "then" <+> se c
                          , "else" <+> se a
                          ]
showExpr (Error msg) = "error(" <> text (show msg) <> ")"
showExpr (Quote stx) = ":(" <> showExpr stx <> ")"

showList :: Expr -> Doc
showList e = brackets $ align $ sep $ punctuate "," $ docs e
  where docs (Lit Null) = []
        docs (Cons hd tl) = (se hd):(docs tl)
        docs rest = ["..." <> se rest]


showInfix :: String -> Expr -> Doc
showInfix op a = case parseExprList a of
  Nothing -> text op <> showArgs a
  Just []  -> showGlobal op <> showArgs a
  Just [a0] -> text op <+> se a0
  Just (a0:aa) -> sep docs
    where docs = a0':aa'
          a0' = se a0
          aa' = zipWith (<+>) (repeat $ text op) (map se aa)
  Just _ -> error "unreachable - why doesn't GHC see this?"


-- TODO use flatAlt to have a different representation when one line vs multiline
---- good for eliding the final comma only for single line
---- good for changing newlines into semicolons
showArgs :: Expr -> Doc
showArgs a = case parseExprList a of
  Nothing -> "(*" <> se a <> ")"
  Just a' -> parens $ align $ sep $ punctuate "," $ map showExpr a'

isId :: String -> Bool
isId = and . map isIdChar
  where isIdChar c = isLetter c || isDigit c || c == '_'

showGlobal :: String -> Doc
showGlobal x = if isId x then text x
               else char '(' <> text x <> char ')'

showVar :: Var -> Doc
showVar (Var x i) = if isId x then s
                    else parens s
  where s = text (x ++ "'" ++ show i)

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
