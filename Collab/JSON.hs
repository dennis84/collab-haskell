{-# LANGUAGE DeriveGeneric #-}

module Collab.JSON
  ( Message(..)
  , Code(..)
  , decodeValue
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson

data Message = Message { t :: Text
                       , d :: Value
                       } deriving (Show, Generic)

data Code = Code { content :: Text
                 , file    :: Text
                 } deriving (Show, Generic)

instance FromJSON Message
instance FromJSON Code

decodeValue :: (FromJSON a) => Value -> Maybe a
decodeValue v = case (fromJSON v) of
                  Success a -> Just a
                  _         -> Nothing
