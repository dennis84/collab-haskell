module Collab.State
  ( State
  , Clients
  , new
  , insert
  , delete
  , lookup
  ) where

import Prelude hiding (lookup)
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Text (Text)
import Collab.Client

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
