{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.AppTests
  ( tests
  ) where

import Test.HUnit
import Collab.App (parseMessage)

tests = TestList
  [ TestCase $ ("code", "{}")           @=? parseMessage "code{}"
  , TestCase $ ("code", "{\"foo\":{}}") @=? parseMessage "code{\"foo\":{}}"
  , TestCase $ ("code", "")             @=? parseMessage "code"
  ]
