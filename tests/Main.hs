{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, newMVar, killThread)
import Data.Text (Text)
import qualified Data.Text as T
import Network.WebSockets
import Test.HUnit
import Collab.App (app)

client :: ClientApp String
client conn = do
  res <- receiveData conn
  sendClose conn ("Bye" :: Text)
  return $ T.unpack res

testJoin :: Assertion
testJoin = do
  state <- newMVar []
  serverThread <- forkIO $ runServer "127.0.0.1" 9000 $ app state
  res <- runClient "localhost" 9000 "/foo" client
  killThread serverThread
  "foo" @=? (take 4 res)

main = runTestTT $ TestList
  [ TestCase testJoin
  ]
