module Main where

import System.Exit

import qualified Language.Vanilla.Core (test)
import qualified Language.Vanilla.Parser (test)
import qualified Language.Vanilla.Printer (test)
import qualified Language.Vanilla.Server (test)
import qualified Language.Vanilla.JS (test)

import Language.Vanilla.Core hiding (test, main)
import Language.Vanilla.Parser hiding (test, main)
import Language.Vanilla.Printer hiding (test, main)
import Language.Vanilla.Server hiding (test, main)
import Language.Vanilla.JS hiding (test, main)

fromNext (Next e) = e

nthStep :: [Def] -> Expr -> Integer -> Expr
nthStep defs e 0 = e
nthStep defs e i = nthStep defs (fromNext (stepInDefs defs e)) (i-1)


main :: IO ()
main = do
  ok <- and `fmap` sequence
        [ Language.Vanilla.Core.test
        , Language.Vanilla.Parser.test
        , Language.Vanilla.Printer.test
        , Language.Vanilla.Server.test
        , Language.Vanilla.JS.test
        ]
  if ok then exitSuccess else exitFailure
