import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit
import qualified Collab.Test.ParseTests as Parse
import qualified Collab.Test.ResponseTests as Resp
import qualified Collab.Test.NamingTests as Naming
import qualified Collab.Test.FunctionalTests as Functional

main :: IO ()
main = do res <- runTestTT tests
          when (errors res > 0 || failures res > 0) exitFailure

tests = TestList
  [ TestLabel "Parse" Parse.tests
  , TestLabel "Response" Resp.tests
  , TestLabel "Naming" Naming.tests
  , TestLabel "Functional" Functional.tests
  ]
