{-# LANGUAGE OverloadedStrings #-}

module Collab.App
  ( app
  , parseMessage
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
import Collab.Json
import Collab.State
import Collab.Util (textToByteString)

-- | The main application. It accepts every request
-- with at least one path segment. The path segment
-- is used as room name.
app :: State -> WS.ServerApp
app state pending = do
    conn <- WS.acceptRequest pending
    guard $ length pathSegments > 0
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

-- | The message hub. It distributes the messages to the
-- corresponding actions.
hub :: State -> Client -> Text -> Text -> IO ()
hub state sender event message = case event of
    "code"        -> maybeDo code (decode m :: Maybe Code)
    "cursor"      -> maybeDo cursor (decode m :: Maybe Cursor)
    "change-nick" -> maybeDo changeNick (decode m :: Maybe ChangeNick)
    "members"     -> members state sender
    _             -> return ()
  where maybeDo f m = maybe (return ()) (f state sender) m
        m = textToByteString message

-- | Returns a tuple with two texts. The first text is the 
-- event name and the other is the data. (If the given text
-- doesn't contain data, then the second text will be empty.)
--
-- > parseMessage "code{}" 
-- > ==> ("code", "{}")
-- > parseMessage "members"
-- > ==> ("members", "")
parseMessage :: Text -> (Text, Text)
parseMessage xs = (T.takeWhile f xs, T.dropWhile f xs)
  where f x = (x /= '{') && (x /= '[')
