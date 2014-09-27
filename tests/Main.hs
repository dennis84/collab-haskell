import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit
import qualified Collab.Test.AppTests as App
import qualified Collab.Test.NamingTests as Naming
import qualified Collab.Test.FunctionalTests as Functional

main :: IO ()
main = do res <- runTestTT tests
          when (errors res > 0 || failures res > 0) exitFailure

tests = TestList
  [ TestLabel "App" App.tests
  , TestLabel "Naming" Naming.tests
  , TestLabel "Functional" Functional.tests
  ]
