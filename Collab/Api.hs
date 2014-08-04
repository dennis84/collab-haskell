{-# LANGUAGE OverloadedStrings #-}

module Collab.Api
  ( join
  , leave
  , members
  , code
  , cursor
  , changeNick
  ) where

import Control.Monad (forM_)
import Control.Concurrent (modifyMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.List (intercalate)
import qualified Network.WebSockets as WS
import Collab.Types (State, Member, Members, getId)
import Control.Applicative ((<$>))

-- When a user enters the room.
join :: State -> Member -> IO ()
join state sender@(_, room, _) =
  modifyMVar_ state $ \s -> do
    let s' = sender : s
    sendToAll "join" room s'
    return s'

-- When a user leaves the room.
leave :: State -> Member -> IO ()
leave state sender@(id, room, _) = do
  s <- modifyMVar state $ \s ->
    let s' = filter ((/= id) . getId) s in return (s',s')
  liftIO $ sendToAll "leave" room s

-- When a client asks for all room members.
members :: State -> Member -> IO ()
members state sender = do
  res <- pack . intercalate "," <$> map getId <$> readMVar state
  liftIO $ pong res sender

-- When a room receives code.
code :: State -> Member -> IO ()
code state sender@(_, room, _) =
  readMVar state >>= sendToAll "code" room

-- When a room receives a cursor.
cursor :: State -> Member -> IO ()
cursor state sender@(_, room, _) =
  readMVar state >>= sendToAll "cursor" room

-- When a user changes his nickname.
changeNick :: State -> Member -> IO ()
changeNick state sender@(_, room, _) =
  readMVar state >>= sendToAll "change-nick" room

-- Sends a message to a member.
pong :: Text -> Member -> IO ()
pong msg (_, _, conn) = WS.sendTextData conn msg

-- Sends a message to all members of given room.
sendToAll :: Text -> Text -> Members -> IO ()
sendToAll msg room members =
  forM_ members $ \(_, r, conn) -> do
    if r == room
      then WS.sendTextData conn msg
      else return ()
