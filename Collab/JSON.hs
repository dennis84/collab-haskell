{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Collab.Json
  ( Join(..)
  , Leave(..)
  , Code(..)
  , Cursor(..)
  , ChangeNick(..)
  , Roommate(..)
  , Roommates(..)
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Collab.TH (options)

data Join = Join
  { join_id     :: Text
  , join_sender :: Text
  } deriving (Show, Typeable)
$(deriveJSON options ''Join)

data Leave = Leave
  { leave_id     :: Text
  , leave_sender :: Text
  } deriving (Show, Typeable)
$(deriveJSON options ''Leave)

data Code = Code
  { code_content :: Text
  , code_file    :: Text
  , code_sender  :: Maybe Text
  } deriving (Show, Typeable)
$(deriveJSON options ''Code)

data Cursor = Cursor
  { cursor_x      :: Int
  , cursor_y      :: Int
  , cursor_file   :: Text
  , cursor_sender :: Maybe Text
  } deriving (Show, Typeable)
$(deriveJSON options ''Cursor)

data ChangeNick = ChangeNick
  { changeNick_name   :: Text
  , changeNick_id     :: Maybe Text
  , changeNick_sender :: Maybe Text
  } deriving (Show, Typeable)
$(deriveJSON options ''ChangeNick)

data Roommate = Roommate
  { roommate_id   :: Text
  , roommate_name :: Text
  , roommate_me   :: Bool
  } deriving (Show, Typeable)
$(deriveJSON options ''Roommate)

data Roommates = Roommates
  { roommates_members :: [Roommate]
  , roommates_sender  :: Text
  } deriving (Show, Typeable)
$(deriveJSON options ''Roommates)
