module Collab.Util
  ( textToByteString
  , generateID
  ) where

import Data.Text (Text, pack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B
import System.Random (getStdGen, randomRs)

textToByteString :: Text -> B.ByteString
textToByteString = E.encodeUtf8 . T.fromStrict

-- | Generates a very unsafe random string.
generateID :: IO Text
generateID = getStdGen >>= return . pack . take 8 . randomRs ('a', 'z')
