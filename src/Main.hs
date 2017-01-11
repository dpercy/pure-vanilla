module Main where

import System.Environment

import qualified Language.Vanilla.Server

import qualified Language.Vanilla.Parser
import Language.Vanilla.Printer (showExpr)
import Language.Vanilla.Core (Def(..), Expr(..), Atom(..))
import Language.Vanilla.Eval (runInDefs, traceInDefs)

import Data.List (intersperse)

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["serve"] -> Language.Vanilla.Server.main
   ["trace", file] -> interactMain Verbose file
   ["run", file] -> interactMain Quiet file
   _ -> error "bad usage: should be   serve | run <file> | trace <file>"


data Verbosity = Quiet | Verbose

interactMain :: Verbosity -> String -> IO ()
interactMain verbosity file = do
  contents <- readFile file
  interact $ \input -> do
    let prog = Language.Vanilla.Parser.pp contents :: [Def]
    let expr = App (Global "main") (Cons (Lit $ String input) (Lit Null))
    let handler = (const $ return $ Error "unhandled effect")
    case verbosity of
     Verbose -> do steps <- traceInDefs prog expr handler
                   concat $ intersperse "\n\n" $ map (show . showExpr) steps
     Quiet -> do result <- runInDefs prog expr handler
                 case result of
                  Lit (String s) -> s
                  Error err -> error err
                  v -> error ("main returned a non-string: " ++ show v)
