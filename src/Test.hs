module Test where

import qualified VanillaCore (test)
import qualified VanillaParser (test)

import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  ok <- and `fmap` sequence
        [ VanillaCore.test
        , VanillaParser.test
        ]
  if ok then exitSuccess else exitFailure
