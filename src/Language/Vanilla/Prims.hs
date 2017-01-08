module Language.Vanilla.Prims where

import qualified Data.Map as Map
import Data.Map (Map)

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
  ("+", varop $ \nums -> foldr (+) 0 nums :: Rational),
  ("-", \args -> case args of
                  [] -> Error "not enough args"
                  [(Lit (Num x))] -> Lit (Num (- x))
                  [(Lit (Num x)), (Lit (Num y))] -> Lit (Num (x - y))
                  [_] -> Error "minus a non-number"
                  [_, _] -> Error "minus a non-number"
                  _ -> Error "too many args"),
  ("*", varop $ \nums -> foldr (*) 1 nums :: Rational),
  ("<", binop $ \a b -> a < (b :: Rational)),
  ("show", unop $ Lit . String . show . showExpr),
  ("length", unop $ \v -> length (v :: [Expr])),
  ("split", binop $ \str delim -> splitOn delim str :: [String]),
  ("splitlines", unop lines),
  ("concat", unop $ \v -> concat (v :: [[Expr]])),
  ("slice", ternop $ \str lo hi -> drop lo (take hi str) :: String),
  ("parseInt", unop $ \v -> read v :: Integer)
  ]
  where unop :: Repr a => Repr b => (a -> b) -> [Expr] -> Expr
        unop f [a] = case fromExpr a of
                      Left err -> Error err -- TODO add name of prim here?
                      Right a -> toExpr (f a)
        unop _ [] = Error "not enough args"
        unop _ _ = Error "too many args"
        binop :: Repr a => Repr b => Repr c => (a -> b -> c) -> [Expr] -> Expr
        binop f [a,b] = case fromExpr a of
                         Left err -> Error err
                         Right a -> case fromExpr b of
                           Left err -> Error err
                           Right b ->
                             toExpr (f a b)
        binop _ [] = Error "not enough args"
        binop _ [_] = Error "not enough args"
        binop _ _ = Error "too many args"
        ternop :: Repr a => Repr b => Repr c => Repr d => (a -> b -> c -> d) -> [Expr] -> Expr
        ternop f [a, b, c] = case args of
                              Right (a, b, c) -> toExpr (f a b c)
                              Left err -> Error err
          where args = do a <- fromExpr a
                          b <- fromExpr b
                          c <- fromExpr c
                          return (a, b, c)
        ternop _ (_:_:_:_) = Error "too many args"
        ternop _ _ = Error "not enough args"
        varop :: Repr a => Repr b => ([a] -> b) -> [Expr] -> Expr
        varop f a = case mapM fromExpr a of
          Left err -> Error err
          Right a -> toExpr (f a)
