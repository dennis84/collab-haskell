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
import Collab.State
import Collab.Api
import Collab.Identifier (generateID)
import Collab.Util (textToString)
import Collab.Json

hub :: State -> Member -> Message -> IO ()
hub state sender msg = case msg of
    (Message "code"        d) -> maybeDo code (decodeValue d :: Maybe Code)
    (Message "cursor"      d) -> maybeDo cursor (decodeValue d :: Maybe Cursor)
    (Message "update-nick" _) -> changeNick state sender
    (Message "members"     _) -> members state sender
    _                         -> return ()
  where maybeDo f d = maybe (return ()) (f state sender) d

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
        json <- WS.receiveData conn
        let msg = (decode json :: Maybe Message)
        liftIO $ maybe (return ()) (hub state member) msg
  where
    req = WS.pendingRequest pending
    pathSegments = decodePathSegments (WS.requestPath req)
