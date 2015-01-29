{-# LANGUAGE OverloadedStrings #-}

module Collab.Api
  ( join
  , leave
  , members
  , code
  , cursor
  , changeNick
  , message
  ) where

import Prelude hiding (join)
import Control.Applicative ((<$>))
import Control.Concurrent (readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B

import Collab.Client
import Collab.Json
import Collab.State (State)
import Collab.Response (pongT, broadcast, broadcastT)
import qualified Collab.State as State

-- | When a user enters the room.
join :: State -> Client -> IO ()
join state sender = do
    liftIO $ State.insert state sender
    readMVar state >>= broadcastT sender (Join id)
  where id = getId sender

-- | When a user leaves the room.
leave :: State -> Client -> IO ()
leave state sender = do
    liftIO $ State.delete state sender
    readMVar state >>= broadcastT sender (Leave id)
  where id = getId sender

-- | Sends the client a list of all members of the room.
-- The response does not contain the `sender` field.
members :: State -> Client -> IO ()
members state sender =
    map makeMember           <$>
    filter (sameRoom sender) <$>
    Map.elems                <$> readMVar state >>= pongT sender . Members
  where makeMember (Client id name _ _) =
          Member id name $ id == getId sender

-- | When a room receives code.
code :: State -> Client -> B.ByteString -> IO ()
code state sender code =
  readMVar state >>= broadcast sender "code" code

-- | When a room receives a cursor.
cursor :: State -> Client -> B.ByteString -> IO ()
cursor state sender cursor =
  readMVar state >>= broadcast sender "cursor" cursor

-- | Change the nickname in the state and sends the
-- updated member back to all members of the room.
changeNick :: State -> Client -> ChangeNick -> IO ()
changeNick state sender nick@(ChangeNick name _) = do
  liftIO $ State.insert state sender { client_name = name }
  readMVar state >>= broadcastT sender nick
                       { changeNick_id = Just $ getId sender
                       }

-- | Sends a chat message to all room members.
message :: State -> Client -> B.ByteString -> IO ()
message state sender msg =
  readMVar state >>= broadcast sender "message" msg
