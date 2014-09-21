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
import Data.Typeable (Typeable, typeOf)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.WebSockets as WS
import Collab.Json
import Collab.State
import Collab.Naming (toDash)
import Collab.Util (replaceWith)

-- | When a user enters the room.
join :: State -> Client -> IO ()
join state sender =
  modifyMVar_ state $ \s -> do
    let s' = sender:s
    let id = getId sender
    sendToAll sender (Join id id) s'
    return s'

-- | When a user leaves the room.
leave :: State -> Client -> IO ()
leave state sender@(Client id _ room _) = do
  s <- modifyMVar state $ \s ->
    let s' = filter (/= sender) s in return (s',s')
  liftIO $ sendToAll sender (Leave id id) s

-- | Sends the client a list of all members of the room.
-- The response does not contain the `sender` field.
members :: State -> Client -> IO ()
members state sender = do
    liftIO $ map makeMember <$> readMVar state >>= pong sender . Members
  where makeMember (Client id name _ _) =
          Member id name $ id == getId sender

-- | When a room receives code.
code :: State -> Client -> Code -> IO ()
code state sender code =
    readMVar state >>= sendToAll sender c
  where c = code { code_sender = Just $ getId sender }

-- | When a room receives a cursor.
cursor :: State -> Client -> Cursor -> IO ()
cursor state sender cursor =
    readMVar state >>= sendToAll sender c
  where c = cursor { cursor_sender = Just $ getId sender }

-- | Change the nickname in the state and sends the
-- updated member back to all members of the room.
changeNick :: State -> Client -> ChangeNick -> IO ()
changeNick state sender@(Client sId _ _ _) nick@(ChangeNick name _ _) =
    modifyMVar_ state $ \s -> do
      let s' = replaceWith (== sender) (\x -> x { client_name = name }) s
      liftIO $ sendToAll sender nick { changeNick_id = Just sId
                                     , changeNick_sender = Just sId
                                     } s'
      return s'

-- | Sends a message to a member.
pong :: (Typeable a, ToJSON a) => Client -> a -> IO ()
pong m a = WS.sendTextData conn $ makeResponse a
  where conn = getConnection m

-- | Sends the message to all members of the room.
sendToAll :: (Typeable a, ToJSON a) => Client -> a -> Clients -> IO ()
sendToAll (Client _ _ roomA _) a clients =
  forM_ clients $ \(Client _ _ roomB conn) -> do
    when (roomA == roomB) $ WS.sendTextData conn $ makeResponse a

-- | Transforms the given type to a valid response format.
--
-- > makeResponse $ Code "foo" "bar" Nothing
-- > ==> "code{\"sender\":null,\"content\":\"foo\",\"file\":\"bar\"}"
makeResponse :: (Typeable a, ToJSON a) => a -> B.ByteString
makeResponse a = t `B.append` encode a
  where t = C.pack . toDash . show $ typeOf a
