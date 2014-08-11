module Collab.TH
  ( options
  ) where

import Data.Aeson.TH (Options, defaultOptions, fieldLabelModifier)
import Collab.Util (slugify)

options :: Options
options = defaultOptions
  { fieldLabelModifier = slugify '_' . dropPrefix
  }

dropPrefix :: String -> String
dropPrefix = tail . dropWhile (/= '_')
