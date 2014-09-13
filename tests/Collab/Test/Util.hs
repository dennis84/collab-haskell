module Collab.Test.Util
  ( runServerApp
  , withServerApp
  ) where

import Control.Concurrent (newMVar, forkIO, killThread)
import Network.WebSockets
import Collab.App (app)

runServerApp = do
  state <- newMVar []
  runServer "127.0.0.1" 9000 $ app state

withServerApp action = do
  thread <- forkIO $ runServerApp
  result <- action
  killThread thread
  return result
