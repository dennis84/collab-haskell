module Collab.Util
  ( textToByteString
  , slugify
  , slugifyType
  ) where

import Data.Char (toLower, isUpper)
import Data.Typeable (Typeable, typeOf)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

textToByteString :: Text -> B.ByteString
textToByteString = E.encodeUtf8 . T.fromStrict

-- | Converts a string into a slug.
slugify :: Char -> String -> String
slugify separator (x:xs) = toLower x:f xs
  where f []                 = []
        f (c:cs) | isUpper c = separator:toLower c:f cs
                 | otherwise = c:f cs

-- | Converts a typable data type into a slug.
slugifyType :: (Typeable a) => a -> String
slugifyType = slugify '-' . show . typeOf
