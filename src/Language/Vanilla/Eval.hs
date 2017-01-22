{-# LANGUAGE TemplateHaskell, OverloadedLists, TypeFamilies, FlexibleContexts, EmptyCase, DeriveGeneric #-}
module Language.Vanilla.Eval (
  evalDefs,
  runInDefs,
  traceDefs,
  test,
  ) where

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Exts
import Data.Maybe
import Data.Functor.Identity
import Control.Monad.Writer
import Data.Graph

import Language.Vanilla.Core
import Language.Vanilla.Prims


-- subst does capture-avoiding substitution, and also renames every binding to avoid shadowing.
subst :: InScope -> Expr -> Map Var Expr -> Expr
subst scope term sub =
  let recur term = subst scope term sub in
   case term of
    Local v -> Map.lookup v sub `fallback` term
    Global _ -> term
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
freeVars (Lit _) = []
freeVars (Error _) = []
freeVars (Quote _) = []
freeVars (Perform e) = freeVars e
freeVars (Cons e0 e1   ) = Set.unions . map freeVars $ [e0, e1]
freeVars (Tag  e0 e1   ) = Set.unions . map freeVars $ [e0, e1]
freeVars (App  e0 e1   ) = Set.unions . map freeVars $ [e0, e1]
freeVars (If   e0 e1 e2) = Set.unions . map freeVars $ [e0, e1, e2]


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
split (Global x) = case Map.lookup x prims of
                    Just _ -> Value
                    Nothing -> Split Hole (Global x)
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


prop_split_ex1 = once $
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

prop_split_ex2 = once $
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
  Split c (Global x) -> Yield c (Global x)
  Split c e -> Next (plug c (stepRoot e))

-- stepRoot assumes there is a redex at the root of the expr tree.
stepRoot :: Expr -> Expr
stepRoot (Local _) = error "stepRoot assumes locals have already been substituted"
stepRoot (Global _) = error "stepRoot doesn't handle globals"
stepRoot (Perform _) = error "stepRoot can't handle Perform"
stepRoot (App (Global op) args) = case Map.lookup op prims of
  Nothing -> error "unreachable - if app/global is a redex, global must be a value - a prim."
  Just f -> case parseExprList args of
             Nothing -> Error "args must be a list"
             Just args -> f args
stepRoot (App (Func params body) args) = case parseExprList args of
  Nothing -> Error "args must be a list"
  Just args -> if length params == length args
               then substTop body (Map.fromList $ zip params args)
               else Error ("arity: " ++ show params ++ " vs " ++ show (length args))
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

-- A YieldHandler tells the evaluator what to do when a step return a Yield.
-- Yields can happen:
--  - when a Global is the redex
--  - when an unhandled (Perform eff) is the redex
-- The one extra wrinkle is that in some contexts, a YieldHandler might need to
-- do some IO (or other monad) action.
type YieldHandler m = (Expr -> m Expr)

{-

TODO explicitly define a "trace" as a coroutine.

-}

traceExpr :: YieldHandler Identity -> Expr -> [Expr]
traceExpr h e = runIdentity (traceExprM h e)

traceExprM :: Monad m => YieldHandler m -> Expr -> m [Expr]
traceExprM h e = do rest <- computeRest
                    return (e:rest)
  where computeRest = case step e of
                       Done -> return []
                       Next e' -> traceExprM h e'
                       Yield c e' -> do e'' <- h e'
                                        traceExprM h (plug c e'')

evalExpr :: Expr -> Expr
evalExpr e = last (traceExpr h e)
  where h (Global _) = return $ Error "unbound global"
        h (Perform _) = return $ Error "unhandled effect"
        h _ = error "yielded a non-global, non-perform expression"


prop_evalExprExample = once $
  (evalExpr (App (Func [Var "x" 0, Var "y" 0, Var "z" 0]
                  [Local (Var "x" 0), Local (Var "z" 0), Local (Var "y" 0)])
             [0, 1, 2]))
  == [0, 2, 1]

prop_evalExprClosure = once $
  -- note that Var "y" 42 does not get renamed, because it doesn't need to be renamed,
  -- because there is no other "y" above or below it in scope.
  (evalExpr (App (Func [Var "x" 0] (Func [Var "y" 42] [Local (Var "x" 0), Local (Var "y" 42)])) [3]))
  == (Func [Var "y" 42]
      [3, Local (Var "y" 42)])


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

-- A Trace is not quite a list of expressions:
-- it's a list of expressions that sometimes has to stop and ask for the value of a Global.
data Trace = Steps [Expr] | ReadGlobal [Expr] String (Expr -> Trace)
instance Show Trace where
  show (Steps es) = "Steps " ++ show es
  show (ReadGlobal es x _) = "ReadGlobal " ++ show es ++ " " ++ show x ++ " (\\v -> ...)"
emptyTrace = Steps []
consTrace e tr = case tr of
  Steps es -> Steps (e:es)
  ReadGlobal es x k -> ReadGlobal (e:es) x k
yieldTrace x k = ReadGlobal [] x k
blockedOn tr = case tr of
  Steps _ -> Nothing
  ReadGlobal _ x _ -> Just x
appendTrace :: Foldable list => list Expr -> Trace -> Trace
appendTrace es tr = foldr (consTrace) tr es

generateTracesForDefs :: [Def] -> (Map String Trace)
generateTracesForDefs defs = traces
  where traces = Map.fromList (map traceDef defs)
        traceDef (Def x e) = (x, t e)
        t :: Expr -> Trace
        t e = consTrace e rest
          where rest = case step e of
                  Done -> emptyTrace
                  Next e' -> t e'
                  Yield c (Perform _) -> t (plug c (Error "unhandled effect at import time"))
                  Yield c (Global x) -> yieldTrace x $ \v -> t (plug c v)
                  Yield _ _ -> error "yielded a non-effect, non-global"

traceDefs :: [Def] -> Map String [Expr]
traceDefs defs = close (generateTracesForDefs defs)
  where close :: Map String Trace -> Map String [Expr]
        -- The idea with close is to step all the defs, until some defs block.
        -- Then every def is either done, runnable, or deadlocked.
        -- Resume the runnable ones, kill the deadlocked ones, and continue.
        close defs =
          let blocked = Map.mapMaybe blockedOn defs in
          let deadlocked = findCycles (Map.toList blocked)
                where findCycles :: [(String, String)] -> [String]
                      findCycles m = concat $ filter nontrivial $ map flattenSCC (stronglyConnComp (map (\(k, v) -> (k, k, [v])) m))
                        where nontrivial xs = length xs > 1
          in
          if Map.size blocked == 0
          then Map.map (\(Steps es) -> es) defs
          else let
            resume :: Trace -> Trace
            resume tr@(Steps _) = tr
            resume tr@(ReadGlobal es x k) =
              if x `elem` deadlocked
              then appendTrace es (k (Error "cyclic definitions"))
              else
                case fromJust (Map.lookup x defs) of
                 -- if x is not done yet, the ReadGlobal blocks
                 ReadGlobal _ _ _ -> tr
                 -- if x is done, plug in the value (or an error, if x failed)
                 Steps vs -> case last vs of
                   Error _ -> appendTrace es (k (Error ("depends on a failed def: " ++ x)))
                   e -> appendTrace es (k e)
            in close (Map.map resume defs)



-- Be warned: evalDefs is undefined if any def fails to terminate.
evalDefs :: [Def] -> [Def]
evalDefs defs = map (\(Def x _) -> (Def x (fromJust (Map.lookup x traces)))) defs
  where traces = Map.map last (traceDefs defs)


prop_evalEmpty = once $ traceDefs [] == Map.fromList []
prop_evalEasy = once $
  traceDefs [ Def "x" (App (Global "z") [42])
            , Def "y" (Global "x")
            , Def "z" (If (Lit $ Bool True) (Func [Var "a" 0] [Local (Var "a" 0)]) 456)
            ]
  == Map.fromList [
    ("x", [ (App (Global "z") [42])
          , (App (Func [Var "a" 0] [Local (Var "a" 0)]) [42])
          , [42]
          ]),
    ("y", [ (Global "x")
          , [42]
          ]),
    ("z", [ (If (Lit $ Bool True) (Func [Var "a" 0] [Local (Var "a" 0)]) 456)
          , (Func [Var "a" 0] [Local (Var "a" 0)])
          ])
    ]


prop_evalCycle = once $
  traceDefs [ Def "x" (App (Global "y") [42])
            , Def "y" (If (Lit $ Bool True) (Global "x") 456)
            ]
  == Map.fromList [
    ("x", [ (App (Global "y") [42])
          , (App (Error "cyclic definitions") [42])
          , (Error "cyclic definitions")
          ]),
    ("y", [ (If (Lit $ Bool True) (Global "x") 456)
          , (Global "x")
          , (Error "cyclic definitions")
          ])
    ]


-- Programs where a global reference is under a function parameter with the same name
-- are ok, because we distinguish between Global and Local vars.
-- This is especially important when this global-under-same-local case
-- only occurs after the program steps (as opposed to being present in the source).
prop_evalShadowGlobal = once $
  evalDefs [ Def "x" (Lit $ Num 7)
           , Def "y" (App
                      (Func [Var "v" 0] (Func [Var "x" 0] (Local (Var "v" 0))))
                      [(Func [] (Global "x"))])
           ]
  == [ Def "x" (Lit $ Num 7)
       -- Note the capitalized Func constructor:
       -- this Global "x" doesn't refer to the parameter "x".
     , Def "y" (Func [Var "x" 0] (Func [] (Global "x")))
     ]

prop_evalPrim = once $
  evalDefs [ Def "x" (App (Global "+") [3, 4]) ]
  == [ Def "x" 7 ]

prop_evenOdd = once $ lookupDef "result" (evalDefs
  [ Def "isEven" (Func [Var "n" 0]
                  (If (App (Global "<") [Local (Var "n" 0), 1])
                   (Lit $ Bool True)
                   (App (Global "isOdd") [(App (Global "+") [-1, Local (Var "n" 0)])])))
  , Def "isOdd" (Func [Var "n" 0]
                 (If (App (Global "<") [Local (Var "n" 0), 1])
                  (Lit $ Bool False)
                  (App (Global "isEven") [(App (Global "+") [-1, Local (Var "n" 0)])])))
  , Def "result" (App (Global "isEven") [5])
  ])
  == Just (Lit $ Bool False)



prop_stepYield_ex1 = once $
  step (Cons 1 (Perform 2))
  == Yield (Cons1 1 Hole) (Perform 2)

{-
runMain takes:
  - list of defs
  - an effect handler (something that consumes a Yield, does an effect, and returns a new expr)
And runs the computation, using the effect handler to handle any Yields that happen.
-}
runMain :: Monad m => [Def] -> (Expr -> m Expr) -> m Expr
runMain defs handler = runInDefs defs (App (Global "main") []) handler

runInDefs :: Monad m => [Def] -> Expr -> (Expr -> m Expr) -> m Expr
runInDefs defs expr handler = last `fmap` traceExprM h expr
  where h (Global x) = return $ case lookupDef x defs of
          Nothing -> Error $ "depends on a missing def: " ++ x
          Just (Error _) -> Error $ "depends on a failed def: " ++ x
          Just e -> e
        h e = handler e

testHandler :: Expr -> Writer String Expr
testHandler (Perform (Lit (String s))) = do
  tell s
  return (Lit $ String "ok")
testHandler _ = return $ Error "testHandler can't handle this effect"

prop_runMain_ex1 = once $
  runWriter (runMain [ Def "main" (Func [] (Perform (Lit $ String "hi"))) ] testHandler)
  == ((Lit $ String "ok"), "hi")

-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll

