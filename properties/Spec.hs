import Test.Tasty (TestTree, defaultMain, testGroup)

import Tests.List (listTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ listTests
    ]
