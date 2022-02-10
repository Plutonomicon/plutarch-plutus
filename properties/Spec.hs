import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (TestTree, defaultMain, testGroup)

import Tests.List (listTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ listTests
    ]
