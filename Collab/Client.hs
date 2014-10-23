module Collab.Client
  ( Client(..)
  , getId
  , getName
  , getRoom
  , getConnection
  , sameRoom
  ) where

import Data.Text (Text)
import Network.WebSockets (Connection)

data Client = Client
  { client_id   :: Text
  , client_name :: Text
  , client_room :: Text
  , client_conn :: Connection
  }

instance Eq Client where
  (==) (Client a _ _ _) (Client b _ _ _) = a == b

getId :: Client -> Text
getId = client_id

getName :: Client -> Text
getName = client_name

getRoom :: Client -> Text
getRoom = client_room

getConnection :: Client -> Connection
getConnection = client_conn

sameRoom :: Client -> Client -> Bool
sameRoom a b = (getRoom a) == (getRoom b)
