module Main where

import qualified VanillaCore (test)
import qualified VanillaParser (test)
import qualified VanillaServer (test)

import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  ok <- and `fmap` sequence
        [ VanillaCore.test
        , VanillaParser.test
        , VanillaServer.test
        ]
  if ok then exitSuccess else exitFailure
