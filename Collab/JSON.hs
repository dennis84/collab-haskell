{-# LANGUAGE TemplateHaskell #-}

module Collab.Json
  ( Join(..)
  , Leave(..)
  , Code(..)
  , Cursor(..)
  , UpdateMember(..)
  , Roommate(..)
  , Roommates(..)
  ) where

import Data.Text (Text)
import Data.Aeson.TH (deriveJSON)
import Collab.TH (options)

data Join = Join
  { join_id     :: Text
  , join_sender :: Text
  } deriving Show
$(deriveJSON options ''Join)

data Leave = Leave
  { leave_id     :: Text
  , leave_sender :: Text
  } deriving Show
$(deriveJSON options ''Leave)

data Code = Code
  { code_content :: Text
  , code_file    :: Text
  , code_sender  :: Maybe Text
  } deriving Show
$(deriveJSON options ''Code)

data Cursor = Cursor
  { cursor_x      :: Int
  , cursor_y      :: Int
  , cursor_file   :: Text
  , cursor_sender :: Maybe Text
  } deriving Show
$(deriveJSON options ''Cursor)

data UpdateMember = UpdateMember
  { updateMember_id     :: Text
  , updateMember_name   :: Text
  , updateMember_sender :: Text
  } deriving Show
$(deriveJSON options ''UpdateMember)

data Roommate = Roommate
  { roommate_id   :: Text
  , roommate_name :: Text
  , roommate_me   :: Bool
  } deriving Show
$(deriveJSON options ''Roommate)

data Roommates = Roommates
  { roommates_members :: [Roommate]
  , roommates_sender  :: Text
  } deriving Show
$(deriveJSON options ''Roommates)
