{-# LANGUAGE OverloadedStrings #-}

module Collab.Response
  ( makeMessage
  , makeMessageT
  , pongT
  , broadcast
  , broadcastT
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad (forM_, when)
import Data.Typeable (Typeable, typeOf)
import Data.Aeson (ToJSON, encode)
import Network.WebSockets (Connection, sendTextData)

import Collab.Client
import Collab.State
import Collab.Naming (toDash)

-- | Creates a message by passing the event, senderId and the data.
--
-- > makeMessage "join" "foo" "{}"
-- ==> "join@foo{}"
makeMessage :: String -> String -> B.ByteString -> B.ByteString
makeMessage evt id msg = info `B.append` msg
  where info = C.pack $ evt ++ "@" ++ id

-- | Creates a message by passing the senderId and
-- a data type which derives `Typeable` and `ToJSON`.
--
-- > makeMessageT "foo" $ Join "foo"
-- ==> "join@foo{"id":"foo"}
makeMessageT :: (Typeable a, ToJSON a) => String -> a -> B.ByteString
makeMessageT id a = makeMessage evt id $ encode a
  where evt = toDash . show $ typeOf a

-- | Sends a message to a single client.
pongT :: (Typeable a, ToJSON a) => Client -> a -> IO ()
pongT (Client id _ _ conn) msg =
  sendTextData conn $ makeMessageT (T.unpack id) msg

broadcast :: Client -> String -> B.ByteString -> Clients -> IO ()
broadcast client evt msg = forAll (sameRoom client) $ \conn -> do
    sendTextData conn $ makeMessage evt id msg
  where id = T.unpack $ getId client

-- | Sends a message to all clients of the given room.
broadcastT :: (Typeable a, ToJSON a) => Client -> a -> Clients -> IO ()
broadcastT client msg = forAll (sameRoom client) $ \conn -> do
    sendTextData conn $ makeMessageT id msg
  where id = T.unpack $ getId client

forAll :: (Client -> Bool) -> (Connection -> IO ()) -> Clients -> IO ()
forAll p f cs = forM_ (Map.elems cs) $ \c -> do
  when (p c) $ f (getConnection c)
