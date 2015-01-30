module Collab.Parse
  ( parseMessage
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Returns a tuple with three texts: (event, sender, data)
--
-- > parseMessage "code{}"
-- > ==> ("code", "", "{}")
-- > parseMessage "members"
-- > ==> ("members", "", "")
-- > parsemessage "code@sender{}"
-- > ==> ("code", "sender", "{}")
parseMessage :: Text -> (Text, Text, Text)
parseMessage xs = (event, T.drop 1 sender, message)
  where (info, message) = splitWith atMessage xs
        (event, sender) = splitWith atSender info
        atMessage x = x /= '{' && x /= '['
        atSender    = (/= '@')

splitWith :: (Char -> Bool) -> Text -> (Text, Text)
splitWith f xs = (T.takeWhile f xs, T.dropWhile f xs)
