{-# LANGUAGE OverloadedStrings #-}

module Collab.App
  ( app
  ) where

import Control.Monad (forever)
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (guard)
import Network.HTTP.Types.URI (decodePathSegments)
import qualified Network.WebSockets as WS
import Collab.Types
import Collab.Api
import Collab.Identifier (generateID)
import Control.Exception (finally)

hub :: State -> Member -> Text -> IO ()
hub state sender msg = case msg of
  "code"        -> code state sender
  "cursor"      -> cursor state sender
  "update-nick" -> changeNick state sender
  "members"     -> members state sender
  _             -> return ()

app :: State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    guard(length pathSegments > 0)
    let room = head pathSegments
    id <- generateID
    let member = (id, room, conn)
    liftIO $ join state member
    flip finally (leave state member) $ do
      forever $ do
        msg <- WS.receiveData conn
        liftIO $ hub state member msg
  where
    req = WS.pendingRequest pending
    pathSegments = decodePathSegments (WS.requestPath req)
