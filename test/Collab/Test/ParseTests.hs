{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.ParseTests
  ( tests
  ) where

import Test.HUnit
import Collab.Parse

tests = TestList
  [ TestCase $ ("", "", "")             @=? parseMessage ""
  , TestCase $ ("foo", "", "")          @=? parseMessage "foo"
  , TestCase $ ("foo", "bar", "")       @=? parseMessage "foo@bar"
  , TestCase $ ("foo", "", "[]")        @=? parseMessage "foo[]"
  , TestCase $ ("foo", "bar", "[]")     @=? parseMessage "foo@bar[]"
  , TestCase $ ("foo", "bar", "{}")     @=? parseMessage "foo@bar{}"
  , TestCase $ ("foo", "bar", "{{[]}}") @=? parseMessage "foo@bar{{[]}}"
  ]
