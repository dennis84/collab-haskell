{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.NamingTests
  ( tests
  ) where

import Test.HUnit
import Collab.Naming

tests = TestList
  [ TestCase $ "foo-bar"   @=? toDash "FooBar"
  , TestCase $ "blah_blub" @=? toUnderscore "blahBlub"
  , TestCase $ "" @=? toDash ""
  ]
