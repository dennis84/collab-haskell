module Collab.State
  ( State
  , Clients
  , new
  , insert
  , delete
  ) where

import Control.Concurrent (MVar, newMVar, modifyMVar_)
import qualified Data.Map as Map
import Data.Text (Text)
import Collab.Client

type Clients = Map.Map Text Client
type State = MVar Clients

new :: IO State
new = newMVar Map.empty

insert :: State -> Client -> IO ()
insert state client =
  modifyMVar_ state $ return . Map.insert (getId client) client

delete :: State -> Client -> IO ()
delete state client =
  modifyMVar_ state $ return . Map.delete (getId client)
