{-# LANGUAGE ScopedTypeVariables #-}

module Collab.Test.Util
  ( runServerApp
  , withServerApp
  , runClientApp
  , waitSome
  ) where

import Control.Exception (SomeException, handle)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Network.WebSockets
import Collab.App (app)
import Collab.State (new)

runServerApp :: IO ()
runServerApp = do
  state <- new
  runServer "127.0.0.1" 9000 $ app state

withServerApp :: IO () -> IO ()
withServerApp action = do
  thread <- forkIO $ runServerApp
  waitSome
  result <- action
  killThread thread
  return result

runClientApp :: ClientApp () -> IO ()
runClientApp action = retry $ runClient "localhost" 9000 "/foo" action

waitSome :: IO ()
waitSome = threadDelay $ 200 * 1000

retry :: IO a -> IO a
retry action = (\(_ :: SomeException) -> waitSome >> action) `handle` action
