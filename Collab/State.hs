module Collab.State
  ( State
  , Client(..)
  , Clients
  , getId
  , getName
  , getRoom
  , getConnection
  ) where

import Control.Concurrent (MVar)
import Data.Text (Text)
import Network.WebSockets (Connection)

type State   = MVar Clients
type Clients = [Client]

data Client = Client
  { client_id   :: Text
  , client_name :: Text
  , client_room :: Text
  , client_conn :: Connection
  }

getId :: Client -> Text
getId = client_id

getName :: Client -> Text
getName = client_name

getRoom :: Client -> Text
getRoom = client_room

getConnection :: Client -> Connection
getConnection = client_conn
