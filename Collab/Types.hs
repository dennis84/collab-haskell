module Collab.Types
  ( State
  , Member
  , Members
  , getId
  ) where

import Control.Concurrent (MVar)
import Data.Text (Text)
import Network.WebSockets (Connection)

type State   = MVar Members
type Member  = (String, Text, Connection)
type Members = [Member]

getId :: Member -> String
getId (id, _, _) = id
