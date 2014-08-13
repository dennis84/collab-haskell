module Collab.Identifier
  ( generateID
  ) where

import Data.Text (Text, pack)
import System.Random (getStdGen, randomRs)

-- | Generates a very unsafe random string.
generateID :: IO Text
generateID = getStdGen >>= return . pack . take 8 . randomRs ('a', 'z')
