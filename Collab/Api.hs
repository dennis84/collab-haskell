{-# LANGUAGE OverloadedStrings #-}

module Collab.Api
  ( join
  , leave
  , members
  , code
  , cursor
  , changeNick
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (modifyMVar, modifyMVar_, readMVar)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.WebSockets as WS
import Collab.Json
import Collab.State
import Collab.Util (slugifyType)

-- | When a user enters the room.
join :: State -> Member -> IO ()
join state sender =
  modifyMVar_ state $ \s -> do
    let s' = sender : s
    let id = getId sender
    sendToAll sender (Join id id) s'
    return s'

-- | When a user leaves the room.
leave :: State -> Member -> IO ()
leave state sender@(id, room, _) = do
  s <- modifyMVar state $ \s ->
    let s' = filter ((/= id) . getId) s in return (s',s')
  liftIO $ sendToAll sender (Leave id id) s

-- | When a client asks for all room members.
members :: State -> Member -> IO ()
members state sender = do
    ms <- map makeRoommate <$> readMVar state
    liftIO $ pong sender $ Roommates ms $ getId sender
  where makeRoommate (id,_,_) = Roommate id id $ (getId sender) == id

-- | When a room receives code.
code :: State -> Member -> Code -> IO ()
code state sender code =
    readMVar state >>= sendToAll sender c
  where c = code { code_sender = Just $ getId sender }

-- | When a room receives a cursor.
cursor :: State -> Member -> Cursor -> IO ()
cursor state sender cursor =
    readMVar state >>= sendToAll sender c
  where c = cursor { cursor_sender = Just $ getId sender }

-- | Change the nickname in the state and sends the
-- updated member back to all members of the room.
changeNick :: State -> Member -> ChangeNick -> IO ()
changeNick state sender@(id,_,_) nick =
    readMVar state >>= sendToAll sender n
  where n = nick { changeNick_id = Just id
                 , changeNick_sender = Just id
                 }

-- | Sends a message to a member.
pong :: (Typeable a, ToJSON a) => Member -> a -> IO ()
pong (_,_,conn) a = WS.sendTextData conn $ makeResponse a

-- | Sends the message to all members of the room.
sendToAll :: (Typeable a, ToJSON a) => Member -> a -> Members -> IO ()
sendToAll (_,room,_) a members =
  forM_ members $ \(_,r,conn) -> do
    when (r == room) $ WS.sendTextData conn $ makeResponse a

-- | Transforms the given type to a valid response format.
-- > Input: Code { code_content = "", code_file = "", ... }
-- > Output: code{"content":"", "file":"", ...}
makeResponse :: (Typeable a, ToJSON a) => a -> B.ByteString
makeResponse a = C.pack (slugifyType a) `B.append` (encode a)
