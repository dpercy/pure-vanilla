{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, TypeFamilies #-}
-- TODO disable the warning about top-level type annotations;
-- it's really annoying with test cases

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Exts


data Def e = Def String e
           deriving (Eq, Show)

data Atom = Null
          | Bool Bool
          | Integer Integer
          | String String
          | Symbol String
          deriving (Eq, Show)

instance Num Atom where
  fromInteger = Integer


data Expr = Var String
          | Lit Atom
          | Cons Expr Expr
          | Tag Expr Expr
          | Func [String] Expr
          | App Expr Expr -- expr must evaluate to an argument-list
          | If Expr Expr Expr
          | Error String
          deriving (Eq, Show)

instance IsList Expr where
  type Item Expr = Expr
  fromListN _ exprs = foldr Cons (Lit Null) exprs

instance Num Expr where
  fromInteger = Lit . Integer

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
split (Var x) = error ("tried to split an open term: " ++ x)
split (Lit _) = Value
split (Cons h t) = splitHelper h t Cons0 Cons1 Value
split (Tag k v) = splitHelper k v Tag0 Tag1 Value
split (Func _ _) = Value
split (App f a) = splitHelper f a App0 App1 (Split Hole (App f a))
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
            -> Split
splitHelper x y cleft cright ifvalue = case split x of
  Value -> case split y of
            Value -> ifvalue
            Crash msg -> Crash msg
            Split c e -> Split (cright x c) e
  Crash msg -> Crash msg
  Split c e -> Split (cleft c y) e


prop_split_ex1 :: Bool
prop_split_ex1 =
  split (Cons
         [7, 8]
         (Cons
          (App 1 2)
          (Lit Null)))
  == (Split
      (Cons1
       [7, 8]
       (Cons0 Hole (Lit Null)))
      (App 1 2))

prop_split_ex2 :: Bool
prop_split_ex2 =
  split (App 1 2)
  == (Split Hole (App 1 2))



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

-- TODO fix problem with substitution:
-- if we have both globals and named function parameters,
-- then substitution doesn't work!!!
-- example:
-- subst (func (x) e) with e=(func () global x) and get (func (x) (func () local x))
-- Is it enough to have a separate form for globals?
-- I think so, because then function parameters don't capture a (Global "x") form.
-- 
prop_substShadow =
  (subst (App (Func ["x", "z"] [Var "x", Var "y", Var "z"])
          [Var "x", Var "y", Var "z"])
   (Map.fromList [ ("x", (Var "a")), ("y", (Var "b")) ]))
   == (App (Func ["x", "z"] [Var "x", Var "b", Var "z"])
       [Var "a", Var "b", Var "z"])



data Step = Done
          | Next Expr
step :: Expr -> Step
step (Error _) = Done
step expr = case split expr of
  Value -> Done
  Crash msg -> Next (Error msg)
  Split c e -> Next (plug c (stepRoot e))

-- stepRoot assumes there is a redex at the root of the expr tree.
stepRoot :: Expr -> Expr
stepRoot (Var x) = Error ("unbound variable: " ++ x)
stepRoot (App (Func params body) args) = case zipArgs params args of
                                          Right pairs -> subst body (Map.fromList pairs)
                                          Left err -> Error err
  where zipArgs :: [String] -> Expr -> Either String [(String, Expr)]
        zipArgs (p:ps) (Cons hd tl) = case zipArgs ps tl of
                                       Left err -> Left err
                                       Right rest -> Right ((p,hd):rest)
        zipArgs [] (Lit Null) = Right []
        zipArgs [] (Cons _ _) = Left "too many args"
        zipArgs (_:_) (Lit Null) = Left "not enough args"
        zipArgs _ _ = Left "args must be a list"
stepRoot (App _ _) = Error "callee must be a function"
stepRoot (If (Lit (Bool True)) c _) = c
stepRoot (If (Lit (Bool False)) _ a) = a
stepRoot (If _ _ _) = Error "if-test must be a boolean"
stepRoot (Lit _) = error "not a redex"
stepRoot (Cons _ _) = error "not a redex"
stepRoot (Tag _ _) = error "not a redex"
stepRoot (Func _ _) = error "not a redex"
stepRoot (Error _) = error "not a redex"

subst :: Expr -> Map String Expr -> Expr
subst (Var x) env = case Map.lookup x env of
                     Nothing -> Var x
                     Just e -> e
subst (Lit v) _ = Lit v
subst (Cons hd tl) env = Cons (subst hd env) (subst tl env)
subst (Tag k v) env = Tag (subst k env) (subst v env)
subst (Func p b) env = Func p (subst b (foldr Map.delete env p))
subst (App f a) env = App (subst f env) (subst a env)
subst (If t c a) env = If (subst t env) (subst c env) (subst a env)
subst (Error msg) _ = Error msg


evalExpr :: Expr -> Expr
evalExpr e = case step e of
  Done -> e
  Next e' -> evalExpr e'

prop_evalExprExample =
  (evalExpr (App (Func ["x", "y", "z"]
                  [Var "x", Var "z", Var "y"])
             [0, 1, 2]))
  == [0, 2, 1]

prop_evalExprClosure =
  (evalExpr (App (Func ["x"]
                  (Func ["y"]
                   [Var "x", Var "y"]))
             [3]))
  == (Func ["y"]
      [3, Var "y"])
 
-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
main :: IO ()
main = $quickCheckAll >> return ()
