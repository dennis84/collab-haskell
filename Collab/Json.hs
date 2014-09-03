{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Collab.Json
  ( Join(..)
  , Leave(..)
  , Code(..)
  , Cursor(..)
  , ChangeNick(..)
  , Member(..)
  , Members(..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
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

data Member = Member
  { member_id   :: Text
  , member_name :: Text
  , member_me   :: Bool
  } deriving (Show, Typeable)
$(deriveJSON options ''Member)

data Members = Members [Member] deriving (Show, Typeable)
$(deriveJSON defaultOptions ''Members)
