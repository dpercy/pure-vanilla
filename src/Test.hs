module Main where

import System.Exit

import qualified VanillaCore (test)
import qualified VanillaParser (test)
import qualified VanillaPrinter (test)
import qualified VanillaServer (test)

import VanillaCore hiding (test, main)
import VanillaParser hiding (test, main)
import VanillaPrinter hiding (test, main)
import VanillaServer hiding (test, main)

fromNext (Next e) = e

nthStep :: [Def] -> Expr -> Integer -> Expr
nthStep defs e 0 = e
nthStep defs e i = nthStep defs (fromNext (stepInDefs defs e)) (i-1)


main :: IO ()
main = do
  ok <- and `fmap` sequence
        [ VanillaCore.test
        , VanillaParser.test
        , VanillaPrinter.test
        , VanillaServer.test
        ]
  if ok then exitSuccess else exitFailure
