{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.UtilTests
  ( tests
  ) where

import Test.HUnit
import Collab.Util (slugify, slugifyType)
import Collab.Json

tests = TestList
  [ TestCase $ assertEqual "" "foo-bar" $ slugify '-' "FooBar"
  , TestCase $ assertEqual "" "blah_blub" $ slugify '_' "blahBlub"
  , TestCase $ assertEqual "" "join" $ slugifyType $ Join "" ""
  , TestCase $ assertEqual "" "change-nick" $ slugifyType $ ChangeNick ""
  ]
