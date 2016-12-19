{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts, NamedFieldPuns, BangPatterns #-}
module VanillaServer where

import VanillaCore

import Test.QuickCheck
import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)


-- a server is some stateful object that lets you define and query things.
data Server = Server { addDefs :: [Def] -> IO ()
                     , setDefs :: [Def] -> IO ()
                     , query :: Expr -> IO Expr
                     }

update :: IORef a -> (a -> a) -> IO ()
update ref f = atomicModifyIORef' ref $ \(!v) -> (f v, ())

defsToMap :: [Def] -> Map String Expr
defsToMap = Map.fromList . map (\(Def x e) -> (x, e))

mkServer :: IO Server
mkServer = do
  defsBox <- newIORef ([] :: Map String Expr)
  let addDefs defs = update defsBox (Map.union $ defsToMap defs)
      setDefs defs = update defsBox (\_ -> defsToMap defs)
      query e = do
        defs <- readIORef defsBox
        runInDefs defs e (const $ Error "unhandled effect")
  return Server { addDefs, setDefs, query }


-- TODO hook up Server to the IO monad and JSON and bullshit in main
---- TODO figure out how to download libraries for HTTP and JSON


main :: IO ()
main = error "TODO"
