{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
  

data Def e = Def String e
           deriving (Eq, Show)

data Atom = Null
          | Bool Bool
          | Integer Integer
          | String String
          | Symbol String
          deriving (Eq, Show)

data Expr = Var String
          | Lit Atom
          | Cons Expr Expr
          | Tag Expr Expr
          | Func [String] Expr
          | App Expr Expr -- expr must evaluate to an argument-list
          | If Expr Expr Expr
          | Error String
          deriving (Eq, Show)

data Context = Hole
             | Cons0 Context Expr
             | Cons1 Expr Context
             | Tag0 Context Expr
             | Tag1 Expr Context
             | App0 Context Expr
             | App1 Expr Context
             | If0 Context Expr Expr
             deriving (Eq, Show)



{-
split takes an expression and searches for the active expression.
There are three possible outcomes:
- there is no active expression because the expression is a value.
- the active expression is literally an Error form, in which case we should throw away the context.
- the active expression is a redex.
-}
data Split = Value
           | Crash String
           | Split Context Expr
           deriving (Eq, Show)
split :: Expr -> Split
split (Var x) = Split Hole (Var x)
split (Lit _) = Value
split (Cons h t) = splitHelper h t Cons0 Cons1
split (Tag k v) = splitHelper k v Tag0 Tag1
split (Func _ _) = Value
split (App f a) = splitHelper f a App0 App1
split (If test consq alt) = case split test of
  Value -> Split Hole (If test consq alt)
  Crash msg -> Crash msg
  Split ctx e -> Split (If0 ctx consq alt) e
split (Error msg) = Crash msg

splitHelper :: Expr
            -> Expr
            -> (Context -> Expr -> Context)
            -> (Expr -> Context -> Context)
            -> Split
splitHelper x y cleft cright = case split x of
  Value -> case split y of
            Value -> Value
            Crash msg -> Crash msg
            Split c e -> Split (cright x c) e
  Crash msg -> Crash msg
  Split c e -> Split (cleft c y) e


prop_splitConsExample :: Bool
prop_splitConsExample =
  split (Cons
         (Lit (Integer 7))
         (Cons (Var "x") (Lit Null)))
  == (Split
         (Cons1
            (Lit (Integer 7))
            (Cons0 Hole (Lit Null)))
         (Var "x"))



plug :: Context -> Expr -> Expr
plug c v = case c of
  Hole -> v
  Cons0 x y -> Cons (plug x v) y
  Cons1 x y -> Cons x (plug y v)
  Tag0 x y -> Tag (plug x v) y
  Tag1 x y -> Tag x (plug y v)
  App0 x y -> App (plug x v) y
  App1 x y -> App x (plug y v)
  If0 test consq alt -> If (plug test v) consq alt





-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
main :: IO ()
main = $quickCheckAll >> return ()
