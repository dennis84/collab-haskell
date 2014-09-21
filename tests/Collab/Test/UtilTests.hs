{-# LANGUAGE OverloadedStrings #-}

module Collab.Test.UtilTests
  ( tests
  ) where

import Test.HUnit
import Collab.Util

tests = TestList
  [ TestCase $ [1,3,3]   @=? replaceWith even (+ 1) [1,2,3]
  , TestCase $ [1,3,3,4] @=? replaceWith even (+ 1) [1,2,3,4]
  ]
