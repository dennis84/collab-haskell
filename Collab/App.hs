{-# LANGUAGE OverloadedStrings #-}

module Collab.App
  ( app
  ) where

import Control.Exception (finally)
import Control.Monad (forever, guard)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.HTTP.Types.URI (decodePathSegments)
import qualified Network.WebSockets as WS
import Data.Aeson (decode)
import Collab.Types
import Collab.Api
import Collab.Identifier (generateID)
import Collab.Util (textToString)
import Collab.JSON

hub :: State -> Member -> Message -> IO ()
hub state sender msg = case msg of
  (Message "code"        d) -> code state sender $ fromJust (decodeValue d :: Maybe Code)
  (Message "cursor"      _) -> cursor state sender
  (Message "update-nick" _) -> changeNick state sender
  (Message "members"     _) -> members state sender
  _                         -> return ()

app :: State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    guard(length pathSegments > 0)
    let room = textToString $ head pathSegments
    id <- generateID
    let member = (id, room, conn)
    liftIO $ join state member
    flip finally (leave state member) $ do
      forever $ do
        msg <- WS.receiveData conn
        let message = decode msg :: Maybe Message
        liftIO $ hub state member $ fromJust message
  where
    req = WS.pendingRequest pending
    pathSegments = decodePathSegments (WS.requestPath req)
