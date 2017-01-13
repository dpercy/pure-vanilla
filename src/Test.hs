module Main where

import System.Exit

import qualified Language.Vanilla.Eval (test)
import qualified Language.Vanilla.Parser (test)
import qualified Language.Vanilla.Printer (test)
import qualified Language.Vanilla.Server (test)
import qualified Language.Vanilla.JS (test)

import Language.Vanilla.Core
import Language.Vanilla.Eval hiding (test)

fromNext (Next e) = e
fromNext _ = error "fromNext a non-Next"

nthStep :: [Def] -> Expr -> Integer -> Expr
nthStep _   e 0 = e
nthStep defs e i = nthStep defs (fromNext (stepInDefs defs e)) (i-1)


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
