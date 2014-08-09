module Collab.Util
  ( textToString
  , textToByteString
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

textToString :: Text -> String
textToString = T.unpack . T.fromStrict

textToByteString :: Text -> B.ByteString
textToByteString = E.encodeUtf8 . T.fromStrict
