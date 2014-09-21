module Collab.Util
  ( textToByteString
  , replaceWith
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

textToByteString :: Text -> B.ByteString
textToByteString = E.encodeUtf8 . T.fromStrict

replaceWith :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceWith _ _ []     = []
replaceWith p r (x:xs)
  | p x == True = r x:xs
  | otherwise   = x:replaceWith p r xs
