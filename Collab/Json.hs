{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Collab.Json
  ( Join(..)
  , Leave(..)
  , ChangeNick(..)
  , Member(..)
  , Members(..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Collab.TH (options)

data Join = Join
  { join_id :: Text
  } deriving (Show, Typeable)
$(deriveJSON options ''Join)

data Leave = Leave
  { leave_id :: Text
  } deriving (Show, Typeable)
$(deriveJSON options ''Leave)

data ChangeNick = ChangeNick
  { changeNick_name :: Text
  , changeNick_id   :: Maybe Text
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
