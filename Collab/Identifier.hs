module Collab.Identifier
  ( generateID
  ) where

import System.Random

generateID :: IO String
generateID = getStdGen >>= return . take 8 . randomRs ('a', 'z')
