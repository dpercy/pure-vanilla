module Language.Vanilla.Prims (prims) where

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List.Split (splitOn)

import Language.Vanilla.Core
import Language.Vanilla.Printer (showExpr) -- for "show" Primop

prims :: Map String ([Expr] -> Expr)
prims = Map.fromList [
  unop "isEmpty" $ \v -> Lit $ Bool $ case v of Lit Null -> True ; _ -> False,
  unop "isCons" $ \v -> Lit $ Bool $ case v of Cons _ _ -> True ; _ -> False,
  unop "first" $ \v -> case v of Cons a _ -> a ; _ -> Error "first non-cons",
  unop "rest" $ \v -> case v of Cons _ b -> b ; _ -> Error "rest non-cons",
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
  unop "show" (Lit . String . show . showExpr),
  unop "length" (\v -> case v of
                  Lit (String s) -> toExpr (length s)
                  v -> case parseExprList v of
                    Just v -> toExpr (length v)
                    Nothing -> Error "length: non-list, non-string"),
  ("split", binop $ \str delim -> splitOn delim str :: [String]),
  unop "splitlines" lines,
  unop "concat" (\v -> concat (v :: [[Expr]])),
  ("strcat", binop $ \a b -> a ++ b :: String),
  ("slice", ternop $ \str lo hi -> drop lo (take hi str) :: String),
  unop "parseInt" (\v -> read v :: Integer),
  ("==", binop $ \a b -> (a :: Expr) == (b :: Expr))
  ]
  where die name err = Error (name ++ ": " ++ err)
        unop :: Repr a => Repr b => String -> (a -> b) -> (String, [Expr] -> Expr)
        unop name f = (name, f')
          where f' [a] = case fromExpr a of
                      Left err -> die name err
                      Right a -> toExpr (f a)
                f' [] = die name "not enough args"
                f' _ = die name "too many args"
        -- TODO do the same error-reporting thing for other helpers here
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
