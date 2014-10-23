{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.ParseTests
  ( tests
  ) where

import Test.HUnit
import Collab.Parse

tests = TestList
  [ TestCase $ ("code", "{}", "")           @=? parseMessage "code{}"
  , TestCase $ ("code", "{\"foo\":{}}", "") @=? parseMessage "code{\"foo\":{}}"
  , TestCase $ ("members", "[]", "")        @=? parseMessage "members[]"
  , TestCase $ ("code", "", "")             @=? parseMessage "code"
  ]
