#!/usr/bin/env runghc
{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, TypeFamilies, FlexibleContexts, EmptyCase #-}
import VanillaCore hiding (main)
import VanillaParser hiding (main)

import Data.Char (ord)
import System.IO.Error (catchIOError)


wc :: [Def]
wc = parseProgram $ unlines [
  "main = () -> let v = loop(0) in perform(tag(:write, v));",
  "loop = (n) ->",
  "  let c = perform(:read) in",
  "    if c < 0", -- negative means eof
  "    then n",
  "    else if c < 10",
  "    then loop(n)",
  "    else if 10 < c",
  "    then loop(n)",
  "    else loop(n + 1)"
  ]

getOrd :: IO Integer
getOrd = (catchIOError (do
                           c <- getChar
                           return $ toInteger $ ord c)
          (\_ -> return (-1)))
  

handler :: Expr -> IO Expr
handler (Perform (Lit (Symbol "read"))) = do
  c <- getOrd
  return (Lit (Integer c))
handler (Perform (Tag
                  (Lit (Symbol "write"))
                  (Lit (Integer n)))) = do
  putStrLn $ show n
  return (Lit $ String "ok")
handler _ = undefined "no handler for this effect"

main :: IO ()
main = do
  Lit (String "ok") <- runMain wc handler
  return ()
