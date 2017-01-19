module Main where

import System.Exit

import qualified Language.Vanilla.Eval (test)
import qualified Language.Vanilla.Parser (test)
import qualified Language.Vanilla.Printer (test)
import qualified Language.Vanilla.Server (test)
import qualified Language.Vanilla.JS (test)


main :: IO ()
main = do
  ok <- and `fmap` sequence
        [ Language.Vanilla.Eval.test
        , Language.Vanilla.Parser.test
        , Language.Vanilla.Printer.test
        , Language.Vanilla.Server.test
        , Language.Vanilla.JS.test
        ]
  if ok then exitSuccess else exitFailure
