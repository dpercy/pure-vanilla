{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, TypeFamilies, FlexibleContexts, EmptyCase, DeriveGeneric #-}
module VanillaCore where

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics
import GHC.Exts
import Data.List (find)
import Data.Void
import Data.Maybe
import Control.Monad.Writer


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
          | Prim PrimFunc
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

subst :: InScope -> Expr -> Map Var Expr -> Expr
subst scope term sub =
  let recur term = subst scope term sub in
   case term of
    Local v -> Map.lookup v sub `fallback` term
    Global _ -> term
    Prim _ -> term
    Lit _ -> term
    Perform e -> Perform (recur e)
    Cons e0 e1 -> Cons (recur e0) (recur e1)
    Tag e0 e1 -> Tag (recur e0) (recur e1)
    Func params body ->
      -- rename parameters so they don't shadow anything in scope
      let (scope', params') = renameVars scope params in
       -- extend the env with parameter renamings
       let newSubs = Map.union sub$ Map.fromList $ zip params (map Local params') in
       let sub' = Map.union sub newSubs in
       Func params' (subst scope' body sub')
    App e0 e1 -> App (recur e0) (recur e1)
    If e0 e1 e2 -> If (recur e0) (recur e1) (recur e2)
    Error _ -> term
    -- TODO what should happen when a quoted term refers to a parameter?
    --  - prevent that situation in the first place?
    --  - subst through the quote?
    --    - is this equivalent to quasiquoting??
    Quote _ -> term

-- convenience for subst when you don't want to specify the scope.
-- the scope comes from the set of free variables in the range of the substitution.
substTop :: Expr -> Map Var Expr -> Expr
substTop term sub = subst scope term sub
  where scope = fromList $ do
          (_, e) <- Map.toList sub
          fv <- Set.toList $ freeVars e
          return fv

freeVars :: Expr -> Set Var
freeVars (Local v) = [v]
freeVars (Func params body) = freeVars body `Set.difference` (fromList params)
freeVars (Global _) = []
freeVars (Prim _) = []
freeVars (Lit _) = []
freeVars (Error _) = []
freeVars (Quote _) = []
freeVars (Perform e) = freeVars e
freeVars (Cons e0 e1   ) = Set.unions . map freeVars $ [e0, e1]
freeVars (Tag  e0 e1   ) = Set.unions . map freeVars $ [e0, e1]
freeVars (App  e0 e1   ) = Set.unions . map freeVars $ [e0, e1]
freeVars (If   e0 e1 e2) = Set.unions . map freeVars $ [e0, e1, e2]

data PrimFunc = OpIsEmpty
              | OpIsCons
              | OpFirst
              | OpRest
              | OpIsTagged
              | OpUntag
              | OpPlus
              | OpMinus
              | OpTimes
              | OpLessThan
              deriving (Eq, Show, Generic, Enum, Bounded)
allPrimops :: [PrimFunc]
allPrimops = enumFrom minBound

primName :: PrimFunc -> String
primName OpIsEmpty = "isEmpty"
primName OpIsCons = "isCons"
primName OpFirst = "first"
primName OpRest = "rest"
primName OpIsTagged = "isTagged"
primName OpUntag = "untag"
primName OpPlus = "+"
primName OpMinus = "-"
primName OpTimes = "*"
primName OpLessThan = "<"

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
                                        Lit (String s) ->
                                          case t of
                                           Tag (Lit (String s')) v ->
                                             if s == s'
                                             then v
                                             else Error "untag key mismatch"
                                           _ -> Error "untag arg must be a tagged value"
                                        _ -> Error "untag key must be a symbol"

applyPrim OpPlus = \args -> case parseNums args of
  Nothing -> Error "plus a non-number"
  Just ns -> Lit (Num (foldr (+) 0 ns))
applyPrim OpMinus = \args -> case args of
  [] -> Error "not enough args"
  [(Lit (Num x))] -> Lit (Num (- x))
  [(Lit (Num x)), (Lit (Num y))] -> Lit (Num (x - y))
  [_] -> Error "minus a non-number"
  [_, _] -> Error "minus a non-number"
  _ -> Error "too many args"
applyPrim OpTimes = \args -> case parseNums args of
  Nothing -> Error "times a non-number"
  Just ns -> Lit (Num (foldr (*) 1 ns))
applyPrim OpLessThan = binop $ \x y -> case (x, y) of
  ((Lit (Num x)), (Lit (Num y))) -> Lit (Bool (x < y))
  _ -> Error "less-than a non-number"

parseNums :: [Expr] -> Maybe [Rational]
parseNums args = case args of
  [] -> Just []
  ((Lit (Num x)):rest) -> do rest' <- parseNums rest
                             Just (x:rest')
  _ -> Nothing



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
split (Local _) = error "unreachable - split doesn't reach under binders"
split (Global x) = Split Hole (Global x)
split (Prim _) = Value
split (Lit _) = Value
split (Perform eff) = case split eff of
  Value -> Split Hole (Perform eff)
  Crash msg -> Crash msg
  Split ctx e -> Split (Perform0 ctx) e
split (Cons h t) = splitHelper h t Cons0 Cons1 $ case t of
  -- when h and t are both values, we have to do an additional check:
  --   if t is not a list, then this call to cons has to be a redex (it will step to an error).
  Lit Null -> Value
  (Cons _ _) -> Value
  _ -> Split Hole (Cons h t)
split (Tag k v) = splitHelper k v Tag0 Tag1 Value
split (Func _ _) = Value
split (App f a) = splitHelper f a App0 App1 (Split Hole (App f a))
split (If test consq alt) = case split test of
  Value -> Split Hole (If test consq alt)
  Crash msg -> Crash msg
  Split ctx e -> Split (If0 ctx consq alt) e
split (Error msg) = Crash msg
split (Quote _) = Value

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
stepRoot (Local _) = error "stepRoot assumes locals have already been substituted"
stepRoot (Global x) = Error ("unbound global: " ++ x)
stepRoot (Prim _) = error "not a redex"
stepRoot (Perform _) = error "stepRoot can't handle Perform"
stepRoot (App (Prim op) args) = case parseExprList args of
                                 Nothing -> Error "args must be a list"
                                 Just args -> applyPrim op args
stepRoot (App (Func params body) args) = case parseExprList args of
  Nothing -> Error "args must be a list"
  Just args -> if length params == length args
               then substTop body (Map.fromList $ zip params args)
               else Error "arity"
stepRoot (App _ _) = Error "callee must be a function"
stepRoot (If (Lit (Bool True)) c _) = c
stepRoot (If (Lit (Bool False)) _ a) = a
stepRoot (If _ _ _) = Error "if-test must be a boolean"
stepRoot (Lit _) = error "not a redex"
stepRoot (Cons _ t) = case t of
  Lit Null -> error "not a redex"
  Cons _ _ -> error "not a redex"
  -- if the tail isn't a list, we step to an error
  _ -> Error "cons a non-list"
stepRoot (Tag _ _) = error "not a redex"
stepRoot (Func _ _) = error "not a redex"
stepRoot (Error _) = error "not a redex"
stepRoot (Quote _) = error "not a redex"

evalExpr :: Expr -> Expr
evalExpr e = case step e of
  Done -> e
  Next e' -> evalExpr e'
  Yield c _ -> evalExpr (plug c (Error "unhandled effect at import time"))

prop_evalExprExample =
  (evalExpr (App (Func [Var "x" 0, Var "y" 0, Var "z" 0]
                  [Local (Var "x" 0), Local (Var "z" 0), Local (Var "y" 0)])
             [0, 1, 2]))
  == [0, 2, 1]

prop_evalExprClosure =
  -- note that Var "y" 42 does not get renamed, because it doesn't need to be renamed,
  -- because there is no other "y" above or below it in scope.
  (evalExpr (App (Func [Var "x" 0] (Func [Var "y" 42] [Local (Var "x" 0), Local (Var "y" 42)])) [3]))
  == (Func [Var "y" 42]
      [3, Local (Var "y" 42)])


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
checkSteps stepper cases = cases == (head cases):(trace stepper (head cases))
{-
  (and (map
        (\(a, b) -> case stepper a of
                     Done -> False
                     Next b' -> b == b'
                     Yield void _ -> case void of)
        (zip cases (tail cases))))
  && stepper (last cases) == Done
-}

trace stepper init =
  case stepper init of
   Done -> []
   Next v -> v:(trace stepper v)
   Yield c _ -> absurd c

prop_evalEmpty = evalDefs [] == []
prop_evalEasy = checkSteps stepDefs [
  [ Def "x" (App (Global "z") [42])
  , Def "y" (Global "x")
  , Def "z" (If (Lit $ Bool True) (Func [Var "a" 0] [Local (Var "a" 0)]) 456)
  ],
  [ Def "x" (App (Global "z") [42])
  , Def "y" (Global "x")
  , Def "z" (Func [Var "a" 0] [Local (Var "a" 0)])
  ],
  [ Def "x" (App (Func [Var "a" 0] [Local (Var "a" 0)]) [42])
  , Def "y" (Global "x")
  , Def "z" (Func [Var "a" 0] [Local (Var "a" 0)])
  ],
  [ Def "x" [42]
  , Def "y" (Global "x")
  , Def "z" (Func [Var "a" 0] [Local (Var "a" 0)])
  ],
  [ Def "x" [42]
  , Def "y" [42]
  , Def "z" (Func [Var "a" 0] [Local (Var "a" 0)])
  ]
  ]

prop_evalCycle = checkSteps stepDefs [
 [ Def "x" (App (Global "y") [42]) , Def "y" (If (Lit $ Bool True) (Global "x") 456)],
 [ Def "x" (App (Global "y") [42]) , Def "y" (Global "x")],
 [ Def "x" (Error "cyclic definition: x") , Def "y" (Global "x")],
 [ Def "x" (Error "cyclic definition: x") , Def "y" (Error "depends on a failed def: x")]
 ]

-- Programs where a global reference is under a function parameter with the same name
-- are ok, because we distinguish between Global and Local vars.
-- This is especially important when this global-under-same-local case
-- only occurs after the program steps (as opposed to being present in the source).
prop_evalShadowGlobal =
  stepDefs [ Def "x" (Lit $ Num 7)
           , Def "y" (App
                      (Func [Var "v" 0] (Func [Var "x" 0] (Local (Var "v" 0))))
                      [(Func [] (Global "x"))])
           ]
  == Next [ Def "x" (Lit $ Num 7)
            -- Note the capitalized Func constructor:
            -- this Global "x" doesn't refer to the parameter "x".
          , Def "y" (Func [Var "x" 0] (Func [] (Global "x")))
          ]

prop_evalPrim =
  stepDefs [ Def "x" (App (Prim OpPlus) [3, 4]) ]
  == Next  [ Def "x" 7 ]

prop_evenOdd = lookupDef "result" (evalDefs
  [ Def "isEven" (Func [Var "n" 0]
                  (If (App (Prim OpLessThan) [Local (Var "n" 0), 1])
                   (Lit $ Bool True)
                   (App (Global "isOdd") [(App (Prim OpPlus) [-1, Local (Var "n" 0)])])))
  , Def "isOdd" (Func [Var "n" 0]
                 (If (App (Prim OpLessThan) [Local (Var "n" 0), 1])
                  (Lit $ Bool False)
                  (App (Global "isEven") [(App (Prim OpPlus) [-1, Local (Var "n" 0)])])))
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
runMain defs handler = runInDefs defs (App (Global "main") []) handler

runInDefs :: Monad m => [Def] -> Expr -> (Expr -> m Expr) -> m Expr
runInDefs defs expr handler = loop expr
  where defs' = evalDefs defs
        loop expr = case stepInDefs defs' expr of
          Done -> return expr
          Next e -> loop e
          Yield c e -> do
            e' <- handler e
            loop $ plug c e'

testHandler :: Expr -> Writer String Expr
testHandler (Perform (Lit (String s))) = do
  tell s
  return (Lit $ String "ok")
testHandler _ = return $ Error "testHandler can't handle this effect"

prop_runMain_ex1 =
  runWriter (runMain [ Def "main" (Func [] (Perform (Lit $ String "hi"))) ] testHandler)
  == ((Lit $ String "ok"), "hi")

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
