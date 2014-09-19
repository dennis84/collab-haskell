module Collab.Naming
  ( toUnderscore
  , toDash
  ) where

import Data.Char (toLower, isUpper)

-- | Converts a camelcase string into underscore separated words.
toUnderscore = transform '_'

-- | Converts a camelcase string into dash separated words.
toDash = transform '-'

transform :: Char -> String -> String
transform _ []     = []
transform s (x:xs) = toLower x:f xs
  where f []                 = []
        f (c:cs) | isUpper c = s:toLower c:f cs
                 | otherwise = c:f cs
