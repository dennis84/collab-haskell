import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit
import Collab.Test.Api (apiTest)

main :: IO ()
main = do res <- runTestTT tests
          when (errors res > 0 || failures res > 0) exitFailure

tests = TestList
  [ TestLabel "API" apiTest
  ]
