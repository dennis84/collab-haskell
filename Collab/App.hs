{-# LANGUAGE OverloadedStrings #-}

module Collab.App
  ( app
  ) where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Monad (forever, guard)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.URI (decodePathSegments)
import qualified Network.WebSockets as WS
import Collab.Api
import Collab.Identifier (generateID)
import Collab.Json
import Collab.State
import Collab.Util (textToByteString)

hub :: State -> Client -> Text -> Text -> IO ()
hub state sender event message = case event of
    "code"        -> maybeDo code (decode m :: Maybe Code)
    "cursor"      -> maybeDo cursor (decode m :: Maybe Cursor)
    "change-nick" -> maybeDo changeNick (decode m :: Maybe ChangeNick)
    "members"     -> members state sender
    _             -> return ()
  where maybeDo f m = maybe (return ()) (f state sender) m
        m = textToByteString message

app :: State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    guard(length pathSegments > 0)
    let room = head pathSegments
    id <- generateID
    let client = Client id id room conn
    liftIO $ join state client
    flip finally (leave state client) $ do
      forever $ do
        (event, message) <- parseMessage <$> WS.receiveData conn
        liftIO $ hub state client event message
  where
    req = WS.pendingRequest pending
    pathSegments = decodePathSegments (WS.requestPath req)

parseMessage :: Text -> (Text, Text)
parseMessage xs = (T.takeWhile f xs, T.dropWhile f xs)
  where f = (/= '{')
