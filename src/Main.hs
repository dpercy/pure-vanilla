module Main where

import System.Environment

import qualified Language.Vanilla.Server

import qualified Language.Vanilla.Parser
import Language.Vanilla.Core (Def(..), Expr(..), Atom(..), runInDefs)

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["serve"] -> Language.Vanilla.Server.main
   ["interact", file] -> interactMain file
   _ -> error "bad usage: should be   serve | interact <file>"



interactMain :: String -> IO ()
interactMain file = do
  contents <- readFile file
  interact $ \input -> do
    let prog = Language.Vanilla.Parser.pp contents :: [Def]
    let expr = App (Global "main") (Cons (Lit $ String contents) (Lit Null))
    let handler = (const $ return $ Error "unhandled effect")
    result <- runInDefs prog expr handler
    case result of
     Lit (String s) -> s
     v -> error ("main returned a non-string: " ++ show v)
