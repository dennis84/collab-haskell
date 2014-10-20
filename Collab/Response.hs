{-# LANGUAGE OverloadedStrings #-}

module Collab.Response
  ( makeMessage
  , makeMessageT
  , pongT
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

makeMessage :: String -> String -> B.ByteString -> B.ByteString
makeMessage evt id msg = info `B.append` msg
  where info = C.pack $ evt ++ "@" ++ id

makeMessageT :: (Typeable a, ToJSON a) => String -> a -> B.ByteString
makeMessageT id a = makeMessage evt id $ encode a
  where evt = toDash . show $ typeOf a

pongT :: (Typeable a, ToJSON a) => Client -> a -> IO ()
pongT (Client id _ _ conn) msg =
  sendTextData conn $ makeMessageT (T.unpack id) msg

broadcastT :: (Typeable a, ToJSON a) => Client -> a -> Clients -> IO ()
broadcastT (Client id _ room _) msg clients =
  forRoommates room clients $ \conn -> do
    sendTextData conn $ makeMessageT (T.unpack id) msg

forRoommates :: Text -> Clients -> (Connection -> IO ()) -> IO ()
forRoommates room cs f =
  forM_ (Map.elems cs) $ \c -> when (room == getRoom c) $ f (getConnection c)
