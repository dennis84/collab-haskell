{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, newMVar, killThread)
import Data.Text (Text)
import Network.WebSockets
import Test.Hspec
import Collab.App (app, parseMessage)

withServerApp :: IO () -> IO ()
withServerApp a = do
    bracket start killThread (const a)
  where start = do
          state <- newMVar []
          forkIO $ runServer "127.0.0.1" 9000 $ app state

main :: IO ()
main = hspec $ around withServerApp $ do
  describe "The Collab API" $ do
    it "should send a join message" $ do
      runClient "localhost" 9000 "/foo" $ \conn -> do
        (event, _) <- parseMessage <$> receiveData conn
        event `shouldBe` "join"
        sendClose conn ("Bye" :: Text)
