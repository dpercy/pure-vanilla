{-# LANGUAGE TemplateHaskell, OverloadedLists, TypeFamilies, FlexibleContexts, EmptyCase, DeriveGeneric, FlexibleInstances #-}
module Language.Vanilla.Core where


import GHC.Exts
import GHC.Generics

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio

data Def = Def String Expr
         deriving (Eq, Show, Generic)

data Atom = Null
          | Bool Bool
          | Num Rational
          | String String
          deriving (Eq, Show, Generic)

instance Num Atom where
  fromInteger = Num . fromInteger
  (+) = error "Num Atom (+)"
  (*) = error "Num Atom (*)"
  (-) = error "Num Atom (-)"
  abs = error "Num Atom abs"
  signum = error "Num Atom signum"

instance Fractional Atom where
  fromRational = Num
  (Num a) / (Num b) = Num (a / b)
  a / b = error $ "divide on non-Num Atom: " ++ (show a) ++ " / " ++ (show b)


data Expr = Local Var
          | Global String
          | Lit Atom
          | Perform Expr -- an arbitrary value can be an effect
          | Cons Expr Expr
          | Tag Expr Expr
          | Func [Var] Expr
          | App Expr Expr -- expr must evaluate to an argument-list
          | If Expr Expr Expr
          | Error String
          | Quote Expr
          deriving (Eq, Show, Generic)

data Var = Var String Integer
         deriving (Eq, Ord, Show, Generic)


instance IsList Expr where
  type Item Expr = Expr
  fromList exprs = foldr Cons (Lit Null) exprs
  toList expr = case parseExprList expr of
    Nothing -> error "toList of a non list"
    Just exprs -> exprs

parseExprList :: Expr -> Maybe [Expr]
parseExprList (Lit Null) = Just []
parseExprList (Cons hd tl) = Just (hd:(toList tl))
parseExprList _ = Nothing

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+) = error "Num Expr (+)"
  (*) = error "Num Expr (*)"
  (-) (Lit (Num x)) (Lit (Num y)) = (Lit (Num (x - y)))
  (-) _ _ = error "subtraction on non-number expr"
  abs = error "Num Expr abs"
  signum = error "Num Expr signum"

instance Fractional Expr where
  fromRational = Lit . fromRational
  (Lit a) / (Lit b) = Lit (a / b)
  a / b = error $ "divide on non-Lit Expr: " ++ (show a) ++ " / " ++ (show b)


-- inscope represents a set of in-scope Vars:
-- it tracks the largest number-part of each name that is in scope.
data InScope = InScope (Map String Integer)
             deriving (Eq, Show, Generic)

-- for in infix:  Map.lookup k m `fallback` x
fallback :: Maybe a -> a -> a
fallback = flip fromMaybe

emptyScope = InScope Map.empty

addToScope :: Var -> InScope -> InScope
addToScope (Var x i) (InScope sc) = InScope $ Map.insert x newI sc
  where newI = max i oldI
        oldI = Map.lookup x sc `fallback` 0

instance IsList InScope where
  type Item InScope = Var
  fromList = foldr addToScope emptyScope
  toList = undefined

renameVar :: InScope -> Var -> Var
renameVar (InScope sc) v@(Var x i) = case Map.lookup x sc of
  Nothing -> if i > 0
             then v
                  --- HAX ALERT: ensure that the result of renameVar always
                  -- has a positive number.
                  -- This allows us to use (-1) as an unreadable sentinel.
             else Var x 0
  Just max -> if i > max
              then v
              else Var x (max + 1)    

renameVars :: InScope -> [Var] -> (InScope, [Var])
renameVars sc [] = (sc, [])
renameVars sc (v:vs) =
  let v' = renameVar sc v in
  let sc' = addToScope v' sc in
  let (sc'', vs') = renameVars sc' vs in
  (sc'', v':vs')
renameVars _ _ = error "silly GHC; this case is unreachable!"


class Repr t where
  -- toExpr always succeeds.
  -- For example, a Haskell String can always become a Lit String.
  toExpr :: t -> Expr
  -- fromExpr can fail.
  -- For example, when trying to extract a Haskell String, you might encounter some other Expr.
  fromExpr :: Expr -> Either String t

instance Repr (Ratio Integer) where
  toExpr = Lit . Num
  fromExpr (Lit (Num i)) = return i
  fromExpr _ = Left "non-number"

instance Repr Integer where
  toExpr = Lit . Num . toRational
  fromExpr e = do e <- fromExpr e
                  case denominator e of
                   1 -> return $ numerator e
                   _ -> Left "non-integer"

instance Repr Int where
  toExpr = Lit . Num . toRational
  fromExpr e = do e <- fromExpr e
                  case denominator e of
                   1 -> return $ fromInteger $ numerator e
                   _ -> Left "non-integer"

instance Repr Bool where
  toExpr = Lit . Bool
  fromExpr (Lit (Bool b)) = return b
  fromExpr _ = Left "non-boolean"

instance {-# OVERLAPPING #-} Repr [Char] where
  toExpr = Lit . String
  fromExpr (Lit (String s)) = return s
  fromExpr _ = Left "non-string"

instance {-# OVERLAPPABLE #-} Repr a => Repr [a] where
  toExpr xs = fromList (map toExpr xs)
  fromExpr e = case parseExprList e of
    Nothing -> Left "non-list"
    Just es -> mapM fromExpr es

instance Repr Expr where
  toExpr = id
  fromExpr = return
    
