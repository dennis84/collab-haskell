import Collab.Test.Api (tests)
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList
         [ TestLabel "API" tests
         ]
