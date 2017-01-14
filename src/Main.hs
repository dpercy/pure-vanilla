module Main where

import System.Environment

import qualified Language.Vanilla.Server

import qualified Language.Vanilla.Parser
import Language.Vanilla.Core (Def(..), Expr(..), Atom(..))
import Language.Vanilla.Eval (runInDefs, traceDefs)
import Language.Vanilla.Printer (showTrace)

import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["serve"] -> Language.Vanilla.Server.main
   ["interact", file] -> interactMain file
   ["trace", file] -> traceMain file
   _ -> error "bad usage: should be   serve | interact <file> | trace <file>"

traceMain :: String -> IO ()
traceMain file = do
  contents <- readFile file
  let prog = Language.Vanilla.Parser.pp contents :: [Def]
  let trace = traceDefs prog
  putStrLn . show . showTrace $ (Map.toList trace)


interactMain :: String -> IO ()
interactMain file = do
  contents <- readFile file
  interact $ \input -> do
    let prog = Language.Vanilla.Parser.pp contents :: [Def]
    let expr = App (Global "main") (Cons (Lit $ String input) (Lit Null))
    let handler = (const $ return $ Error "unhandled effect")
    result <- runInDefs prog expr handler
    case result of
     Lit (String s) -> s
     Error err -> error err
     v -> error ("main returned a non-string: " ++ show v)
