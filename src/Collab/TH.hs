module Collab.TH
  ( options
  ) where

import Data.Aeson.TH (Options, defaultOptions, fieldLabelModifier)
import Collab.Naming (toUnderscore)

options :: Options
options = defaultOptions
  { fieldLabelModifier = toUnderscore . dropPrefix
  }

dropPrefix :: String -> String
dropPrefix = tail . dropWhile (/= '_')
