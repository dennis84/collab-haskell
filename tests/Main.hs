import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit
import qualified Collab.Test.ApiTests as Api
import qualified Collab.Test.AppTests as App
import qualified Collab.Test.UtilTests as Util

main :: IO ()
main = do res <- runTestTT tests
          when (errors res > 0 || failures res > 0) exitFailure

tests = TestList
  [ TestLabel "Api" Api.tests
  , TestLabel "App" App.tests
  , TestLabel "Util" Util.tests
  ]
