#!/usr/bin/env runghc
{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, TypeFamilies, FlexibleContexts, EmptyCase #-}
import Prototype hiding (main)

import Data.Char (ord)
import System.IO.Error (catchIOError)

wc :: [Def]
wc = [
  -- wc: count the number of newlines
  Def "main" (Func [] (Perform (Tag (Lit $ Symbol "write") (App (Global "loop") [0])))),
  Def "loop"
  (Func ["n"]
   (App (Func ["c"]
         (If (App (Prim OpLessThan) [Var "c", 0])
          -- it's EOF
          (Var "n")
          (If (App (Prim OpLessThan) [Var "c", 10])
           -- not a newline
           (App (Global "loop") [Var "n"])
           (If (App (Prim OpLessThan) [10, Var "c"])
            -- also not a newline
            (App (Global "loop") [Var "n"])
            -- it's a newline
            (App (Global "loop") [(App (Prim OpPlus)
                                  [Var "n", 1])])))))
    [(Perform (Lit (Symbol "read")))]))
  
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
