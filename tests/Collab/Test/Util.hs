module Collab.Test.Util
  ( runServerApp
  , withClient
  , withNoopClient
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, when, guard)
import Control.Exception (bracket)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, newMVar, killThread)
import Data.Text (Text)
import Network.WebSockets
import Test.Hspec
import Collab.App (app, parseMessage)
import Collab.Json
import Collab.Util (textToByteString)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Control.Concurrent.MVar

runServerApp = do
  state <- newMVar []
  runServer "127.0.0.1" 9000 $ app state

withClient :: ClientApp () -> IO ()
withClient = runClient "localhost" 9000 "/foo"

withNoopClient a = withClient $ \_ -> a
