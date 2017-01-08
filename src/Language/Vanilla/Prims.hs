module Language.Vanilla.Prims where

import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Exts (fromList)

import Data.List.Split (splitOn)

import Language.Vanilla.Core
import Language.Vanilla.Printer (showExpr) -- for "show" Primop

prims :: Map String ([Expr] -> Expr)
prims = Map.fromList [
  ("isEmpty", unop $ \v -> Lit $ Bool $ case v of Lit Null -> True ; _ -> False),
  ("isCons", unop $ \v -> Lit $ Bool $ case v of Cons _ _ -> True ; _ -> False),
  ("first", unop $ \v -> case v of Cons a _ -> a ; _ -> Error "first non-cons"),
  ("rest", unop $ \v -> case v of Cons _ b -> b ; _ -> Error "rest non-cons"),
  ("isTagged", binop $ \k t -> Lit $ Bool $ case t of Tag k' _ -> k == k' ; _ -> False),
  ("untag", binop $ \k t -> case k of
                             Lit (String s) ->
                               case t of
                                Tag (Lit (String s')) v ->
                                  if s == s'
                                  then v
                                  else Error "untag key mismatch"
                                _ -> Error "untag arg must be a tagged value"
                             _ -> Error "untag key must be a symbol"),
  ("+", \args -> case parseNums args of
                  Nothing -> Error "plus a non-number"
                  Just ns -> Lit (Num (foldr (+) 0 ns))),
  ("-", \args -> case args of
                  [] -> Error "not enough args"
                  [(Lit (Num x))] -> Lit (Num (- x))
                  [(Lit (Num x)), (Lit (Num y))] -> Lit (Num (x - y))
                  [_] -> Error "minus a non-number"
                  [_, _] -> Error "minus a non-number"
                  _ -> Error "too many args"),
  ("*", \args -> case parseNums args of
                  Nothing -> Error "times a non-number"
                  Just ns -> Lit (Num (foldr (*) 1 ns))),
  ("<", binop $ \x y -> case (x, y) of
                         ((Lit (Num x)), (Lit (Num y))) -> Lit (Bool (x < y))
                         _ -> Error "less-than a non-number"),
  ("show", unop $ Lit . String . show . showExpr),
  ("length", unop $ \v -> case parseExprList v of
                           Just lst -> Lit . Num . toRational $ length lst
                           Nothing -> Error "length non-list"),
  ("split", binop $ \str delim -> case str of
    Lit (String str) -> case delim of
      Lit (String delim) -> fromList $ map (Lit . String) $ splitOn delim str
      _ -> Error "split non-string"
    _ -> Error "split non-string"),
  ("splitlines", unop $ \v -> case v of
                               Lit (String s) -> fromList $ map (Lit . String) $ lines s
                               _ -> Error "splitlines non-string"),
  -- TODO can I use a type class to eliminate these tedious conversions?
  ("concat", unop $ \v -> case parseExprList v of
                           Nothing -> Error "concat non-list"
                           Just vs -> case sequence (map parseExprList vs) of
                             Nothing -> Error "concat list contained non-list"
                             Just vs -> fromList (concat vs))
  ]
  where unop :: (Expr -> Expr) -> [Expr] -> Expr
        unop f [a] = f a
        unop _ [] = Error "not enough args"
        unop _ _ = Error "too many args"
        binop :: (Expr -> Expr -> Expr) -> [Expr] -> Expr
        binop f [a,b] = f a b
        binop _ [] = Error "not enough args"
        binop _ [_] = Error "not enough args"
        binop _ _ = Error "too many args"
