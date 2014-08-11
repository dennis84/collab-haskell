module Collab.Util
  ( textToString
  , textToByteString
  , slugify
  , slugifyType
  ) where

import Data.Char (toLower, isUpper)
import Data.Typeable (Typeable, typeOf)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

textToString :: Text -> String
textToString = T.unpack . T.fromStrict

textToByteString :: Text -> B.ByteString
textToByteString = E.encodeUtf8 . T.fromStrict

slugify :: Char -> String -> String
slugify d (x:xs) = toLower x:f xs
  where f []                 = []
        f (c:cs) | isUpper c = d:toLower c:f cs
                 | otherwise = c:f cs

slugifyType :: (Typeable a) => a -> String
slugifyType = slugify '-' . show . typeOf
