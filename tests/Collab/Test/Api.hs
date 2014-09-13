{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.Api
  ( tests
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Text (Text, unpack)
import Data.Maybe (fromJust)
import Data.Aeson (decode)
import Network.WebSockets
import Test.HUnit
import Collab.App (parseMessage)
import Collab.Json
import Collab.Util (textToByteString)
import Collab.Test.Util

loopA conn = do
  (event, _) <- parseMessage <$> receiveData conn
  print $ "Client A: " ++ unpack event
  loopA conn

loopB conn = do
  (event, message) <- parseMessage <$> receiveData conn
  print $ "Client B: " ++ unpack event
  case event of
    "join"   -> do
      sendTextData conn ("members" :: Text)
      loopB conn
    "members" -> do
      sendTextData conn ("code{\"content\":\"foo\",\"file\":\"foo.hs\"}" :: Text)
      loopB conn
    "code" -> do
      sendTextData conn ("cursor{\"x\":1,\"y\":2,\"file\":\"foo.hs\"}" :: Text)
      loopB conn
    "cursor" -> return ()
    _        -> error "Fail"

tests = TestCase $ withServerApp $ do
  _ <- forkIO $ runClient "localhost" 9000 "/foo" loopA
  runClient "localhost" 9000 "/foo" loopB
