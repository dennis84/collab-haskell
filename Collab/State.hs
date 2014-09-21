module Collab.State
  ( State
  , Client(..)
  , Clients
  , generateID
  , getId
  , getName
  , getRoom
  , getConnection
  ) where

import Control.Concurrent (MVar)
import Data.Text (Text, pack)
import Network.WebSockets (Connection)
import System.Random (getStdGen, randomRs)

type State   = MVar Clients
type Clients = [Client]

data Client = Client
  { client_id   :: Text
  , client_name :: Text
  , client_room :: Text
  , client_conn :: Connection
  }

instance Eq Client where
  (==) (Client a _ _ _) (Client b _ _ _) = a == b

-- | Generates a very unsafe random string.
generateID :: IO Text
generateID = getStdGen >>= return . pack . take 8 . randomRs ('a', 'z')

getId :: Client -> Text
getId = client_id

getName :: Client -> Text
getName = client_name

getRoom :: Client -> Text
getRoom = client_room

getConnection :: Client -> Connection
getConnection = client_conn
