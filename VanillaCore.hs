{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, TypeFamilies, FlexibleContexts, EmptyCase #-}
module VanillaCore where

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Exts
import Data.List (find)
import Data.Void
import Control.Monad.Writer


data Def = Def String Expr
         deriving (Eq, Show)

data Atom = Null
          | Bool Bool
          | Integer Integer
          | String String
          | Symbol String
          deriving (Eq, Show)

instance Num Atom where
  fromInteger = Integer
  (+) = error "Num Atom (+)"
  (*) = error "Num Atom (*)"
  (-) = error "Num Atom (-)"
  abs = error "Num Atom abs"
  signum = error "Num Atom signum"


data Expr = Var String
          | Global String
          | Prim PrimFunc
          | Lit Atom
          | Perform Expr -- an arbitrary value can be an effect
          | Cons Expr Expr
          | Tag Expr Expr
          | Func [String] Expr
          | App Expr Expr -- expr must evaluate to an argument-list
          | If Expr Expr Expr
          | Error String
          deriving (Eq, Show)


data PrimFunc = OpIsEmpty
              | OpIsCons
              | OpFirst
              | OpRest
              | OpIsTagged
              | OpUntag
              | OpPlus
              | OpLessThan
              deriving (Eq, Show)

unop :: (Expr -> Expr) -> [Expr] -> Expr
unop f [a] = f a
unop _ [] = Error "not enough args"
unop _ _ = Error "too many args"

binop :: (Expr -> Expr -> Expr) -> [Expr] -> Expr
binop f [a,b] = f a b
binop _ [] = Error "not enough args"
binop _ [_] = Error "not enough args"
binop _ _ = Error "too many args"

applyPrim :: PrimFunc -> [Expr] -> Expr
applyPrim OpIsEmpty  = unop $ \v -> Lit $ Bool $ case v of Lit Null -> True ; _ -> False
applyPrim OpIsCons   = unop $ \v -> Lit $ Bool $ case v of Cons _ _ -> True ; _ -> False
applyPrim OpFirst    = unop $ \v -> case v of Cons a _ -> a ; _ -> Error "first non-cons"
applyPrim OpRest     = unop $ \v -> case v of Cons _ b -> b ; _ -> Error "rest non-cons"
applyPrim OpIsTagged = binop $ \k t -> Lit $ Bool $ case t of Tag k' _ -> k == k' ; _ -> False
applyPrim OpUntag    = binop $ \k t -> case k of
                                        Lit (Symbol s) ->
                                          case t of
                                           Tag (Lit (Symbol s')) v ->
                                             if s == s'
                                             then v
                                             else Error "untag key mismatch"
                                           _ -> Error "untag arg must be a tagged value"
                                        _ -> Error "untag key must be a symbol"

applyPrim OpPlus = binop $ \x y -> case (x, y) of
  ((Lit (Integer x)), (Lit (Integer y))) -> Lit (Integer (x + y))
  _ -> Error "plus a non-integer"
applyPrim OpLessThan = binop $ \x y -> case (x, y) of
  ((Lit (Integer x)), (Lit (Integer y))) -> Lit (Bool (x < y))
  _ -> Error "less-than a non-integer"



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
  fromInteger = Lit . Integer
  (+) = error "Num Expr (+)"
  (*) = error "Num Expr (*)"
  (-) (Lit (Integer x)) (Lit (Integer y)) = (Lit (Integer (x - y)))
  (-) _ _ = error "subtraction on non-number expr"
  abs = error "Num Expr abs"
  signum = error "Num Expr signum"

data Context = Hole
             | Perform0 Context
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
split (Global x) = Split Hole (Global x)
split (Prim _) = Value
split (Lit _) = Value
split (Perform eff) = case split eff of
  Value -> Split Hole (Perform eff)
  Crash msg -> Crash msg
  Split ctx e -> Split (Perform0 ctx) e
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

prop_split_ex2 =
  split (App 1 2)
  == (Split Hole (App 1 2))



plug :: Context -> Expr -> Expr
plug c v = case c of
  Hole -> v
  Perform0 eff -> Perform (plug eff v)
  Cons0 x y -> Cons (plug x v) y
  Cons1 x y -> Cons x (plug y v)
  Tag0 x y -> Tag (plug x v) y
  Tag1 x y -> Tag x (plug y v)
  App0 x y -> App (plug x v) y
  App1 x y -> App x (plug y v)
  If0 test consq alt -> If (plug test v) consq alt

prop_substShadow =
  (subst (App (Func ["x", "z"] [Var "x", Var "y", Var "z"])
          [Var "x", Var "y", Var "z"])
   (Map.fromList [ ("x", (Var "a")), ("y", (Var "b")) ]))
   == (App (Func ["x", "z"] [Var "x", Var "b", Var "z"])
       [Var "a", Var "b", Var "z"])



data Step c e = Done
              | Next e
              | Yield c e
              deriving (Eq, Show)
step :: Expr -> Step Context Expr
step (Error _) = Done
step expr = case split expr of
  Value -> Done
  Crash msg -> Next (Error msg)
  Split c (Perform e) -> Yield c (Perform e)
  Split c e -> Next (plug c (stepRoot e))

-- stepRoot assumes there is a redex at the root of the expr tree.
stepRoot :: Expr -> Expr
stepRoot (Var x) = Error ("unbound variable: " ++ x)
stepRoot (Global x) = Error ("unbound global: " ++ x)
stepRoot (Prim _) = error "not a redex"
stepRoot (Perform _) = error "stepRoot can't handle Perform"
stepRoot (App (Prim op) args) = case parseExprList args of
                                 Nothing -> Error "args must be a list"
                                 Just args -> applyPrim op args
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
subst (Global x) _ = Global x
subst (Prim op) _ = Prim op
subst (Lit v) _ = Lit v
subst (Perform eff) env = Perform (subst eff env)
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
  Yield c _ -> evalExpr (plug c (Error "unhandled effect at import time"))

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


-- A definition is done when it is a Value or an Error.
-- Something like (App (Error _) (Lit Null)) is not done, because the error
-- still needs to propagate up to the top level.
isDefDone :: Def -> Bool
isDefDone (Def _ (Error _)) = True
isDefDone (Def _ expr) = case split expr of
  Value -> True
  Crash _ -> False
  Split _ _ -> False


-- stepDefs steps a top-level set of definitions once.
stepDefs :: [Def] -> Step Void [Def]
stepDefs defs = case findActiveDef defs of
  AllDone -> Done
  Cycle name -> Next $ updateDef name (Error $ "cyclic definition: " ++ name) defs
  OneActive (Def name expr) -> case stepInDefs defs expr of
    Done -> error "unreachable - active def can't be a value"
    Next e -> Next $ updateDef name e defs
    Yield c _ -> Next $ updateDef name (plug c (Error "unhandled effect at import time")) defs


{-

- find the first non-done def (by splitting each one), split it
  - if it's blocked on another global, either:
    - if that def is done, the global is the redex
    - if that def is not done, focus on it in the search for the active def.
- step the active def
  - if the redex was a global, plug in the value or some error
  - otherwise use stepRoot

If all the defs are done then there is no active def; returns Nothing.

-}

lookupDef :: String -> [Def] -> Maybe Expr
lookupDef name defs = lookup name $ map (\(Def n e) -> (n, e)) defs

updateDef :: String -> Expr -> [Def] -> [Def]
updateDef name newExpr defs = map f defs
  where f (Def n e) = if n == name
                      then (Def n newExpr)
                      else (Def n e)

data WhichActiveDef = AllDone
                    | OneActive Def
                    | Cycle String
                    deriving (Eq, Show)

findActiveDef :: [Def] -> WhichActiveDef
findActiveDef defs = case find (not . isDefDone) defs of
  Nothing -> AllDone
  Just def -> recur def []
    where recur :: Def -> [String] -> WhichActiveDef
          recur (Def name expr) blocked =
            if name `elem` blocked
            then Cycle name
            else
              case split expr of
               Value -> error "unreachable - this def satisfied (not . isDefDone)"
               -- A def that is about to crash is active and has no dependencies on other defs.
               Crash _ -> OneActive (Def name expr)
               -- If this def's active expr is a global, then it depends on another def.
               -- Look up that def and check whether it's done.
               -- If it is done, this def is the active one (the global is the redex).
               -- If it's not done, continue the search at that def.
               Split _ (Global g) -> case lookupDef g defs of
                 Nothing -> OneActive (Def name expr) -- missing other def means global steps to error
                 Just otherDefExpr ->
                   let otherDef = (Def g otherDefExpr) in
                   if isDefDone otherDef
                   then OneActive (Def name expr)
                   else recur otherDef (name:blocked)
               -- If the redex in this def is not a global, this is the active def.
               Split _ _ -> OneActive (Def name expr)


evalDefs :: [Def] -> [Def]
evalDefs defs = case stepDefs defs of
  Done -> defs
  Next defs' -> evalDefs defs'
  Yield void _ -> case void of

checkSteps :: Eq e => (e -> Step Void e) -> [e] -> Bool
checkSteps stepper cases =
  (and (map
        (\(a, b) -> case stepper a of
                     Done -> False
                     Next b' -> b == b'
                     Yield void _ -> case void of)
        (zip cases (tail cases))))
  && stepper (last cases) == Done


prop_evalEmpty = evalDefs [] == []
prop_evalEasy = checkSteps stepDefs [
  [ Def "x" (App (Global "z") [42])
  , Def "y" (Global "x")
  , Def "z" (If (Lit $ Bool True) (Func ["a"] [Var "a"]) 456)
  ],
  [ Def "x" (App (Global "z") [42])
  , Def "y" (Global "x")
  , Def "z" (Func ["a"] [Var "a"])
  ],
  [ Def "x" (App (Func ["a"] [Var "a"]) [42])
  , Def "y" (Global "x")
  , Def "z" (Func ["a"] [Var "a"])
  ],
  [ Def "x" [42]
  , Def "y" (Global "x")
  , Def "z" (Func ["a"] [Var "a"])
  ],
  [ Def "x" [42]
  , Def "y" [42]
  , Def "z" (Func ["a"] [Var "a"])
  ]
  ]

prop_evalCycle = checkSteps stepDefs [
 [ Def "x" (App (Global "y") [42]) , Def "y" (If (Lit $ Bool True) (Global "x") 456)],
 [ Def "x" (App (Global "y") [42]) , Def "y" (Global "x")],
 [ Def "x" (Error "cyclic definition: x") , Def "y" (Global "x")],
 [ Def "x" (Error "cyclic definition: x") , Def "y" (Error "depends on a failed def: x")]
 ]

-- Programs where a Global is under a function parameter with the same name
-- are tricky, because there isn't a nice way to represent them using s-expressions.
-- And a non-tricky program can step to a tricky one:
prop_evalShadowGlobal =
  stepDefs [ Def "x" (Lit $ Integer 7)
           , Def "y" (App
                      (Func ["v"] (Func ["x"] (Var "v")))
                      [(Func [] (Global "x"))])
           ]
  == Next [ Def "x" (Lit $ Integer 7)
          , Def "y" (Func ["x"] (Func [] (Global "x")))
          ]

prop_evalPrim =
  stepDefs [ Def "x" (App (Prim OpPlus) [3, 4]) ]
  == Next  [ Def "x" 7 ]

prop_evenOdd = lookupDef "result" (evalDefs
  [ Def "isEven" (Func ["n"] (If (App (Prim OpLessThan) [Var "n", 1])
                              (Lit $ Bool True)
                              (App (Global "isOdd") [(App (Prim OpPlus) [-1, Var "n"])])))
  , Def "isOdd" (Func ["n"] (If (App (Prim OpLessThan) [Var "n", 1])
                             (Lit $ Bool False)
                             (App (Global "isEven") [(App (Prim OpPlus) [-1, Var "n"])])))
  , Def "result" (App (Global "isEven") [5])
  ])
  == Just (Lit $ Bool False)



prop_stepYield_ex1 =
  step (Cons 1 (Perform 2))
  == Yield (Cons1 1 Hole) (Perform 2)

stepGlobal :: [Def] -> Expr -> Expr
stepGlobal defs (Global g) = case lookupDef g defs of
  Nothing -> Error $ "depends on a missing def: " ++ g
  Just (Error _) -> Error $ "depends on a failed def: " ++ g
  Just e -> e
stepGlobal _ _ = error "stepGlobal can only handle Global exprs"

stepInDefs :: [Def] -> Expr -> Step Context Expr
stepInDefs defs expr =
  case split expr of
     Split c (Global g) -> Next $ plug c $ stepGlobal defs (Global g)
     _ -> step expr


{-
runMain takes:
  - list of defs
  - an effect handler (something that consumes a Yield, does an effect, and returns a new expr)
And runs the computation, using the effect handler to handle any Yields that happen.
-}
runMain :: Monad m => [Def] -> (Expr -> m Expr) -> m Expr
runMain defs handler = case lookupDef "main" $ evalDefs defs of
  Nothing -> fail "no main function defined"
  Just mainFunc -> loop (App mainFunc [])
    where loop mainFunc = case stepInDefs defs mainFunc of
            Done -> return mainFunc
            Next e -> loop e
            Yield c e -> do
              e' <- handler e
              loop $ plug c e'

testHandler :: Expr -> Writer String Expr
testHandler (Perform (Lit (String s))) = do
  tell s
  return (Lit $ Symbol "ok")
testHandler _ = return $ Error "testHandler can't handle this effect"

prop_runMain_ex1 =
  runWriter (runMain [ Def "main" (Func [] (Perform (Lit $ String "hi"))) ] testHandler)
  == ((Lit $ Symbol "ok"), "hi")



-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
main :: IO ()
main = $quickCheckAll >> return ()
