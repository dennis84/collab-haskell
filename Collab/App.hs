{-# LANGUAGE OverloadedStrings #-}

module Collab.App
  ( app
  ) where

import Control.Exception (finally)
import Control.Monad (forever, guard)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.URI (decodePathSegments)
import qualified Network.WebSockets as WS
import Data.Aeson (decode)
import Collab.State
import Collab.Api
import Collab.Json
import Collab.Identifier (generateID)
import Collab.Util (textToString, textToByteString)

hub :: State -> Member -> Text -> Text -> IO ()
hub state sender event message = case event of
    "code"        -> maybeDo code (decode m :: Maybe Code)
    "cursor"      -> maybeDo cursor (decode m :: Maybe Cursor)
    "update-nick" -> maybeDo changeNick (decode m :: Maybe Text)
    "members"     -> members state sender
    _             -> return ()
  where maybeDo f m = maybe (return ()) (f state sender) m
        m = textToByteString message

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
        (event, message) <- fmap parseMessage $ WS.receiveData conn
        liftIO $ hub state member event message
  where
    req = WS.pendingRequest pending
    pathSegments = decodePathSegments (WS.requestPath req)

parseMessage :: Text -> (Text, Text)
parseMessage xs = (T.takeWhile f xs, T.dropWhile f xs)
  where f = (/= '{')
