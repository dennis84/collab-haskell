module Collab.Util
  ( textToByteString
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

textToByteString :: Text -> B.ByteString
textToByteString = E.encodeUtf8 . T.fromStrict
