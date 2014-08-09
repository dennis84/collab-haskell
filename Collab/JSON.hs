{-# LANGUAGE TemplateHaskell #-}

module Collab.Json
  ( Code(..)
  , Cursor(..)
  ) where

import Data.Text (Text)
import Data.Aeson.TH (deriveFromJSON)
import Collab.TH (options)

data Code = Code
  { code_content :: Text
  , code_file    :: Text
  } deriving Show

$(deriveFromJSON options ''Code)

data Cursor = Cursor
  { cursor_x    :: Int
  , cursor_y    :: Int
  , cursor_file :: Text
  } deriving Show

$(deriveFromJSON options ''Cursor)
