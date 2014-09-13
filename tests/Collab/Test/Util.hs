module Collab.Test.Util
  ( runServerApp
  ) where

import Control.Concurrent (newMVar)
import Network.WebSockets
import Collab.App (app)

runServerApp = do
  state <- newMVar []
  runServer "127.0.0.1" 9000 $ app state
