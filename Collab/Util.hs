module Collab.Util
  ( textToString
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as T

textToString :: Text -> String
textToString = T.unpack . T.fromStrict
