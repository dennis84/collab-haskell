{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.AppTests
  ( tests
  ) where

import Test.HUnit
import Collab.App (parseMessage)

tests = TestList
  [ TestCase $ assertEqual "" ("code", "{}") $ parseMessage "code{}"
  , TestCase $ assertEqual "" ("code", "") $ parseMessage "code"
  ]
