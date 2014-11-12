{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.FunctionalTests
  ( tests
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Text (Text, unpack)
import Data.Maybe (fromJust)
import Data.Aeson (decode)
import Network.WebSockets
import Test.HUnit
import Collab.Parse (parseMessage)
import Collab.Json
import Collab.Util (textToByteString)
import Collab.Test.Util

loopA conn = do
  (event, _, _) <- parseMessage <$> receiveData conn
  print $ "Client A: " ++ unpack event
  loopA conn

loopB conn = do
  (event, _, message) <- parseMessage <$> receiveData conn
  let messageBS = textToByteString message
  print $ "Client B: " ++ unpack event
  case event of
    "join" -> do
      sendTextData conn ("members" :: Text)
      loopB conn
    "members" -> do
      let members = decode messageBS :: Maybe [Member]
      2 @=? (length $ fromJust members)
      sendTextData conn ("code{\"content\":\"foo\",\"file\":\"foo.hs\",\"lang\":\"haskell\"}" :: Text)
      loopB conn
    "code" -> do
      sendTextData conn ("cursor{\"x\":1,\"y\":2,\"file\":\"foo.hs\"}" :: Text)
      loopB conn
    "cursor" -> do
      sendTextData conn ("change-nick{\"name\":\"dennis\"}" :: Text)
      loopB conn
    "change-nick" -> do
      let nick = decode messageBS :: Maybe ChangeNick
      "dennis" @=? (changeNick_name $ fromJust nick)
      return ()
    _ -> error "Fail"

tests = TestCase $ withServerApp $ do
  _ <- forkIO $ runClientApp loopA
  waitSome
  _ <- runClientApp loopB
  return ()
