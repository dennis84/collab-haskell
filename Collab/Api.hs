{-# LANGUAGE OverloadedStrings #-}

module Collab.Api
  ( join
  , leave
  , members
  , code
  , cursor
  , changeNick
  ) where

import Control.Monad (forM_, when)
import Control.Concurrent (modifyMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.List (intercalate)
import qualified Network.WebSockets as WS
import Collab.State
import Collab.Json
import Control.Applicative ((<$>))

-- When a user enters the room.
join :: State -> Member -> IO ()
join state sender =
  modifyMVar_ state $ \s -> do
    let s' = sender : s
    sendToAll "join" sender s'
    return s'

-- When a user leaves the room.
leave :: State -> Member -> IO ()
leave state sender@(id, room, _) = do
  s <- modifyMVar state $ \s ->
    let s' = filter ((/= id) . getId) s in return (s',s')
  liftIO $ sendToAll "leave" sender s

-- When a client asks for all room members.
members :: State -> Member -> IO ()
members state sender = do
  res <- pack . intercalate "," <$> map getId
                                <$> filter ((== getRoom sender) . getRoom)
                                <$> readMVar state
  liftIO $ pong res sender

-- When a room receives code.
code :: State -> Member -> Code -> IO ()
code state sender (Code c f) =
  readMVar state >>= sendToAll c sender

-- When a room receives a cursor.
cursor :: State -> Member -> Cursor -> IO ()
cursor state sender cursor =
  readMVar state >>= sendToAll "cursor" sender

-- When a user changes his nickname.
changeNick :: State -> Member -> IO ()
changeNick state sender =
  readMVar state >>= sendToAll "change-nick" sender

-- Sends a message to a member.
pong :: Text -> Member -> IO ()
pong msg (_, _, conn) = WS.sendTextData conn msg

-- Sends a message to all members of given room.
sendToAll :: Text -> Member -> Members -> IO ()
sendToAll msg (_, room, _) members =
  forM_ members $ \(_, r, conn) -> do
    when (r == room) $ WS.sendTextData conn msg
