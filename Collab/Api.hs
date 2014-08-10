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
import qualified Network.WebSockets as WS
import Data.Aeson (ToJSON, encode)
import Collab.State
import Collab.Json
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (toLower)

-- When a user enters the room.
join :: State -> Member -> IO ()
join state sender =
  modifyMVar_ state $ \s -> do
    let s' = sender : s
    let id = getId sender
    let m = Join id id
    sendToAll m sender s'
    return s'

-- When a user leaves the room.
leave :: State -> Member -> IO ()
leave state sender@(id, room, _) = do
  s <- modifyMVar state $ \s ->
    let s' = filter ((/= id) . getId) s in return (s',s')
  liftIO $ sendToAll (Leave id id) sender s

-- When a client asks for all room members.
members :: State -> Member -> IO ()
members state sender = do
  ms <- readMVar state
  let d = map (\(id,_,_) -> Roommate id id ((getId sender) == id)) ms
  liftIO $ pong (Roommates d $ getId sender) sender

-- When a room receives code.
code :: State -> Member -> Code -> IO ()
code state sender code = readMVar state >>= sendToAll res sender
  where res = code { code_sender = Just $ getId sender }

-- When a room receives a cursor.
cursor :: State -> Member -> Cursor -> IO ()
cursor state sender cursor = readMVar state >>= sendToAll res sender
  where res = cursor { cursor_sender = Just $ getId sender }

-- When a user changes his nickname.
changeNick :: State -> Member -> Text -> IO ()
changeNick state sender@(id,_,_) nick =
  readMVar state >>= sendToAll (UpdateMember id nick id) sender

-- Sends a message to a member.
pong :: (Show a, ToJSON a) => a -> Member -> IO ()
pong a (_, _, conn) = WS.sendTextData conn $ makeResponse a

sendToAll :: (Show a, ToJSON a) => a -> Member -> Members -> IO ()
sendToAll a (_, room, _) members =
  forM_ members $ \(_, r, conn) -> do
    when (r == room) $ WS.sendTextData conn $ makeResponse a

makeResponse :: (Show a, ToJSON a) => a -> B.ByteString
makeResponse a = e `B.append` (encode a)
  where e = C.pack . map toLower . takeWhile (/= ' ') $ show a
