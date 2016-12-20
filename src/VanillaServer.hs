{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts, NamedFieldPuns, BangPatterns #-}
module VanillaServer where

import VanillaCore

import Test.QuickCheck
import Data.IORef
import qualified Data.Aeson as Aeson


instance Aeson.FromJSON PrimFunc
instance Aeson.FromJSON Atom
instance Aeson.FromJSON Expr
instance Aeson.FromJSON Def

instance Aeson.ToJSON PrimFunc
instance Aeson.ToJSON Atom
instance Aeson.ToJSON Expr
instance Aeson.ToJSON Def



-- a server is some stateful object that lets you define and query things.
data Server = Server { addDefs :: [Def] -> IO ()
                     , setDefs :: [Def] -> IO ()
                     , query :: Expr -> IO Expr
                     }

update :: IORef a -> (a -> a) -> IO ()
update ref f = atomicModifyIORef' ref $ \(!v) -> (f v, ())

unionDefs :: [Def] -> [Def] -> [Def]
unionDefs a b = foldr f [] (a ++ b)
  where f (Def x e) defs = if any (\(Def x' _) -> x == x') defs
                           then defs
                           else (Def x e):defs

prop_union_empty = unionDefs [] [] == []
prop_union_left = unionDefs [Def "x" 1, Def "y" 2] [] == [Def "x" 1, Def "y" 2]
prop_union_right = unionDefs [] [Def "x" 1, Def "y" 2] == [Def "x" 1, Def "y" 2]
prop_union_both =
  unionDefs [Def "x" 1, Def "y" 2] [Def "a" 3, Def "b" 4]
  == [Def "x" 1, Def "y" 2, Def "a" 3, Def "b" 4]
prop_union_right_wins =
  unionDefs [Def "y" 3, Def "x" 4] [Def "x" 1, Def "y" 2]
  == [Def "x" 1, Def "y" 2]
prop_union_full =
    (unionDefs
     [Def "a" 1, Def "b" 2, Def "c" 3, Def "d" 4]
     [Def "e" 5, Def "c" 0, Def "b" 0, Def "f" 6]
    ) == [Def "a" 1,                       Def "d" 4,
          Def "e" 5, Def "c" 0, Def "b" 0, Def "f" 6]



                                

mkServer :: IO Server
mkServer = do
  defsBox <- newIORef ([] :: [Def])
  return Server {
    addDefs = \defs -> update defsBox (unionDefs defs),
    setDefs = \defs -> update defsBox (const defs),
    query = \e -> do
      defs <- readIORef defsBox
      runInDefs defs e (const $ return $ Error "unhandled effect")
    }


-- TODO hook up Server to the IO monad and JSON and bullshit in main
---- TODO figure out how to download libraries for HTTP and JSON


main :: IO ()
main = error "TODO"



-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
