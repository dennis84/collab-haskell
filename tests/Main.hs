{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, newMVar, killThread)
import Data.Text (Text)
import Network.WebSockets
import Test.Hspec
import Collab.App (app, parseMessage)
import Collab.Json
import Collab.Util (textToByteString)
import Data.Aeson (decode)
import Data.Maybe (fromJust)

runServerApp = do
  state <- newMVar []
  runServer "127.0.0.1" 9000 $ app state

withClient :: ClientApp () -> IO ()
withClient = runClient "localhost" 9000 "/foo"

specs = hspec $ do
  describe "The Collab API" $ do
    it "should send a join message" $ withClient $ \conn -> do
      (event, _) <- parseMessage <$> receiveData conn
      event `shouldBe` "join"
      sendClose conn ("Bye" :: Text)

    it "should return a list of all members of a room" $ withClient $ \conn -> do
      _ <- receiveDataMessage conn
      sendTextData conn ("members" :: Text)
      (event, message) <- parseMessage <$> receiveData conn
      let ms = decode (textToByteString message) :: Maybe [Member]
      event `shouldBe` "members"
      (length $ fromJust ms) `shouldBe` 1
      sendClose conn ("Bye" :: Text)

main :: IO ()
main = bracket (forkIO runServerApp) killThread (const specs)
