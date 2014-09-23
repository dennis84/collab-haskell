module Collab.State
  ( State
  , Client(..)
  , Clients
  , generateID
  , getId
  , getName
  , getRoom
  , getConnection
  , new
  , insert
  , delete
  , lookup
  ) where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Network.WebSockets (Connection)
import System.Random (getStdGen, randomRs)
import Prelude hiding (lookup)

-- | Client stuff

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

-- | State API

type Clients = Map.Map Text Client
type State = MVar Clients

new :: IO State
new = newMVar Map.empty

insert :: State -> Text -> Client -> IO ()
insert state id client =
  modifyMVar_ state $ return . Map.insert id client

delete :: State -> Text -> IO ()
delete state id =
  modifyMVar_ state $ return . Map.delete id

lookup :: State -> Text -> IO (Maybe Client)
lookup state id =
  Map.lookup id `liftM` readMVar state

-- | Generates a very unsafe random string.
generateID :: IO Text
generateID = getStdGen >>= return . pack . take 8 . randomRs ('a', 'z')
