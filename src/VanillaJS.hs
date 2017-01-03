{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module VanillaJS where

{--------------------------------------------------------------------------- }

Simple Approximate Core-to-JS compiler
- "simple" - emit concise, efficient code
- "approximate" - the translation is imperfect by design

- functions become functions (no loops or TCO)
- numbers (rationals) become numbers (float64)
  - arithmetic on non-numbers is undefined behavior
- booleans become booleans
- if/then/else becomes ternary "?:"
- other stuff fails to compile

(- maybe: cons-lists become arrays)



{ ---------------------------------------------------------------------------}

import VanillaCore
import Test.QuickCheck
import Data.List


nameVar (Var x i) = "_" ++ x ++ "_" ++ show i

-- It is the caller's responsibility to only pass a fully-evaluated set of defs.
-- Returns a JS expression that evaluates to an object with a property for each def.
trResidualProgram :: [Def] -> String
trResidualProgram defs = "(function() {\n" ++ body ++ exports ++ "})()\n"
  where body = concat (map trDef defs)
        exports = "return {\n" ++ concat (map mkexport defs) ++ "};\n"
        mkexport (Def x _) = x ++ ": " ++ x ++ ",\n"

trDef :: Def -> String
trDef (Def x e) = "var " ++ x ++ " = " ++ trExpr e ++ ";\n"


trExpr :: Expr -> String
trExpr (Local v) = nameVar v
trExpr (Global s) = s -- TODO avoid collision with Local
trExpr (Lit v) = trAtom v
trExpr e@(Perform _) = noCase e
trExpr e@(Cons _ _) = noCase e
trExpr e@(Tag _ _) = noCase e
trExpr (Func params body) = concat [ "function("
                                       , commas (map nameVar params)
                                       , ") { return "
                                       , trExpr body
                                       , "; }"
                                       ]
trExpr e@(App f a) = case parseExprList a of
  Nothing -> noCase e
  Just a' -> case (f, a') of
    -- any-arity operator
    (Prim op, args) | primName op `elem` ["+", "*"] ->
                        let args' = map trExpr args in
                        "(" ++ (concat $ intersperse (" " ++ primName op ++ " ") args') ++ ")"
    -- binary operators
    (Prim op, [x, y]) | primName op `elem` ["-", "<"] ->
                          concat [ "("
                                 , trExpr x
                                 , " "
                                 , primName op
                                 , " "
                                 , trExpr y
                                 , ")"
                                 ]
    -- unary operators
    (Prim OpMinus, [x]) -> "(- " ++ trExpr x ++ ")"
    -- general case for function application
    _ -> trExpr f ++ "(" ++ commas (map trExpr a') ++ ")"
trExpr e@(Prim _) = noCase e
trExpr (If t c a) = concat [ "("
                           , trExpr t
                           , " ? "
                           , trExpr c
                           , " : "
                           , trExpr a
                           , ")"
                           ]
trExpr (Error msg) = err msg

commas :: [String] -> String
commas = concat . intersperse ", "

err :: String -> String
err msg = "(function() { throw Error(" ++ show msg ++ "); })()"

noCase :: Expr -> String
noCase expr = err (show expr)

trAtom :: Atom -> String
trAtom Null = "null"
trAtom (Bool True) = "true"
trAtom (Bool False) = "false"
trAtom (Num n) = show (fromRational n :: Double)
trAtom (String s) = show s
trAtom (Symbol s) = "Symbol.for(" ++ show s ++ ")"


prop_example =
  trResidualProgram [Def "f" (Func [Var "x" 0] (App (Prim OpPlus) (Cons (Local (Var "x" 0)) (Cons 3 (Lit Null)))))]
  == unlines [ "(function() {"
             , "var f = function(_x_0) { return (_x_0 + 3.0); };"
             , "return {"
             , "f: f,"
             , "};"
             , "})()"
             ]

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
