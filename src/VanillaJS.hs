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


-- It is the caller's responsibility to only pass a fully-evaluated set of defs.
-- Returns a JS expression that evaluates to an object with a property for each def.
trResidualProgram :: [Def] -> String
trResidualProgram defs = "(function() {\n" ++ body ++ exports ++ "})()\n"
  where body = concat (map trDef defs)
        exports = "return {\n" ++ concat (map mkexport defs) ++ "};\n"
        mkexport (Def x _) = x ++ ": " ++ x ++ ",\n"

trDef :: Def -> String
trDef (Def x e) = "var " ++ x ++ " = " ++ trExpr [] e ++ ";\n"

trExpr :: [String] -> Expr -> String
-- TODO share shadow-avoiding code with VanillaPrinter
trExpr env (Upref i) = env !! i
trExpr _ (Var s) = s
trExpr _ (Lit v) = trAtom v
trExpr _ e@(Perform _) = noCase e
trExpr _ e@(Cons _ _) = noCase e
trExpr _ e@(Tag _ _) = noCase e
trExpr env (Func params body) = concat [ "function("
                                        , commas params
                                        , ") { return "
                                        , trExpr env' body
                                        , "; }"
                                        ]
  where env' = reverse params ++ env
trExpr env e@(App f a) = case parseExprList a of
  Nothing -> noCase e
  Just a' -> case (f, a') of
    -- any-arity operator
    (Prim op, args) | primName op `elem` ["+", "*"] ->
                        let args' = map (trExpr env) args in
                        "(" ++ (concat $ intersperse (" " ++ primName op ++ " ") args') ++ ")"
    -- binary operators
    (Prim op, [x, y]) | primName op `elem` ["-", "<"] ->
                          concat [ "("
                                 , trExpr env x
                                 , " "
                                 , primName op
                                 , " "
                                 , trExpr env y
                                 , ")"
                                 ]
    -- unary operators
    (Prim OpMinus, [x]) -> "(- " ++ trExpr env x ++ ")"
    -- general case for function application
    _ -> trExpr env f ++ "(" ++ commas (map (trExpr env) a') ++ ")"
trExpr _ e@(Prim _) = noCase e
trExpr env(If t c a) = concat [ "("
                              , trExpr env t
                              , " ? "
                              , trExpr env c
                              , " : "
                              , trExpr env a
                              , ")"
                              ]
trExpr _ (Error msg) = err msg

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
  trResidualProgram [Def "f" (func ["x"] (App (Prim OpPlus) (Cons (Var "x") (Cons 3 (Lit Null)))))]
  == unlines [ "(function() {"
             , "var f = function(x) { return (x + 3.0); };"
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
