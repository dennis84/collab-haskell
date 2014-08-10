module Collab.State
  ( State
  , Member
  , Members
  , getId
  , getRoom
  ) where

import Control.Concurrent (MVar)
import Data.Text (Text)
import Network.WebSockets (Connection)

type State   = MVar Members
type Member  = (Text, String, Connection)
type Members = [Member]

getId :: Member -> Text
getId (id, _, _) = id

getRoom :: Member -> String
getRoom (_, room, _) = room
