{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.ResponseTests
  ( tests
  ) where

import Test.HUnit
import Collab.Response
import Collab.Json

tests = TestList
  [ TestCase $ "foo@bar{}"  @=? makeMessage "foo" "bar" "{}"
  , TestCase $ "member@bar{\"me\":true,\"name\":\"bar\",\"id\":\"foo\"}" @=?
               makeMessageT "bar" (Member "foo" "bar" True)
  ]
