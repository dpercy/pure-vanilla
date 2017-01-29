{-# LANGUAGE TemplateHaskell, OverloadedLists, OverloadedStrings, FlexibleContexts, NamedFieldPuns, BangPatterns #-}
module Language.Vanilla.Server (serve, test) where

import Language.Vanilla.Core
import Language.Vanilla.Eval hiding (test)
import Language.Vanilla.Parser (parseProgram, parseExpr)
import Language.Vanilla.Printer (showExpr, showDefs)
import Language.Vanilla.JS (trResidualProgram)

import Test.QuickCheck
import Data.IORef
import qualified Data.Aeson as Aeson
import Web.Scotty
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (unpack)
import Data.String (fromString)
import System.Timeout

instance Aeson.FromJSON Var
instance Aeson.FromJSON Atom
instance Aeson.FromJSON Expr
instance Aeson.FromJSON Def

instance Aeson.ToJSON Var
instance Aeson.ToJSON Atom
instance Aeson.ToJSON Expr
instance Aeson.ToJSON Def



-- a server is some stateful object that lets you define and query things.
data Server = Server { addDefs :: [Def] -> IO ()
                     , setDefs :: [Def] -> IO ()
                     , getDefs :: IO [Def]
                     , query :: Expr -> IO Expr
                     , getResidualDefs :: IO [Def]
                     }

update :: IORef a -> (a -> a) -> IO ()
update ref f = atomicModifyIORef' ref $ \(!v) -> (f v, ())

unionDefs :: [Def] -> [Def] -> [Def]
unionDefs a b = foldr f [] (a ++ b)
  where f (Def x e) defs = if any (\(Def x' _) -> x == x') defs
                           then defs
                           else (Def x e):defs

prop_union_empty = once $ unionDefs [] [] == []
prop_union_left = once $ unionDefs [Def "x" 1, Def "y" 2] [] == [Def "x" 1, Def "y" 2]
prop_union_right = once $ unionDefs [] [Def "x" 1, Def "y" 2] == [Def "x" 1, Def "y" 2]
prop_union_both = once $
  unionDefs [Def "x" 1, Def "y" 2] [Def "a" 3, Def "b" 4]
  == [Def "x" 1, Def "y" 2, Def "a" 3, Def "b" 4]
prop_union_right_wins = once $
  unionDefs [Def "y" 3, Def "x" 4] [Def "x" 1, Def "y" 2]
  == [Def "x" 1, Def "y" 2]
prop_union_full = once $
    (unionDefs
     [Def "a" 1, Def "b" 2, Def "c" 3, Def "d" 4]
     [Def "e" 5, Def "c" 0, Def "b" 0, Def "f" 6]
    ) == [Def "a" 1,                       Def "d" 4,
          Def "e" 5, Def "c" 0, Def "b" 0, Def "f" 6]



                                

mkServer :: Libs -> IO Server
mkServer libs = do
  defsBox <- newIORef ([] :: [Def])
  return Server {
    addDefs = \defs -> update defsBox (`unionDefs` defs),
    setDefs = \defs -> update defsBox (const defs),
    getDefs = readIORef defsBox,
    query = \e -> do
      defs <- readIORef defsBox
      runInDefs libs defs e (const $ return $ Error "unhandled effect"),
    getResidualDefs = do
      defs <- readIORef defsBox
      let !defs' = evalDefs libs defs
      return defs'
    }


-- TODO hook up Server to the IO monad and JSON and bullshit in main
---- TODO figure out how to download libraries for HTTP and JSON





serve :: Libs -> IO ()
serve libs = do
  server <- mkServer libs
  scotty 3000 $ do
    get "/" $ do
      setHeader "Content-type" "text/html"
      file "static/index.html"
    post "/addDefs" $ do
      s <- body
      case parseProgram libs (unpack s) of
       Left err -> do
         status status400
         text $ fromString $ "Parse error: " ++ show err
       Right defs -> do
         liftIO $ addDefs server defs
         defs <- liftIO $ getDefs server
         text $ fromString $ show $ showDefs defs
    post "/setDefs" $ do
      s <- body
      case parseProgram libs (unpack s) of
       Left err -> do
         status status400
         text $ fromString $ "Parse error: " ++ show err
       Right defs -> do
         liftIO $ setDefs server defs
         defs <- liftIO $ getDefs server
         text $ fromString $ show $ showDefs defs
    post "/query" $ do
      s <- body
      case parseExpr libs (unpack s) of
       Left err -> do
         status status400
         text $ fromString $ "Parse error: " ++ show err
       Right e -> do
         resultOrTimeout <- liftIO $ timeout requestTimeoutMicros $ query server e
         case resultOrTimeout of
          Nothing -> do status status400
                        text $ fromString $ "Computation timed out"
          Just result -> do
            text $ fromString $ show $ showExpr result
    post "/residualDefs" $ do
      defsOrTimeout <- liftIO $ timeout requestTimeoutMicros $ getResidualDefs server
      case defsOrTimeout of
       Nothing -> do status status400
                     text $ fromString $ "Computation timed out"
       Just defs' -> text $ fromString $ trResidualProgram defs'


requestTimeoutMicros :: Int
requestTimeoutMicros = 1 * 1000 * 1000


-- scary quickCheck macros!
-- see haskell docs for quickCheckAll
return []
test :: IO Bool
test = $quickCheckAll
