module Properties.Spec (propertyTests) where

import Properties.Tests.List (listTests)
import Test.Tasty (TestTree, testGroup)

propertyTests :: TestTree
propertyTests =
  testGroup
    "property tests"
    [ listTests
    ]
