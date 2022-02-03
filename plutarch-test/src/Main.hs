
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import Plutarch
import Plutarch.Prelude
import Test.Tasty.Golden
import Data.Text.Encoding
import qualified Data.Text as T

main :: IO ()
main =
  defaultMain $ testGroup "all tests" [newTests] 

add1 :: Term s (PInteger :--> PInteger :--> PInteger)
add1 = plam $ \x y -> x + y + 1

newTests :: TestTree
newTests =
  testGroup
    "sridplay"
    [ testCase "add1" $ do 
        (printTerm add1) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
    , goldenVsString "foo" "examples/goldens/foo.uplc.golden" $ do 
        pure $ BS.fromStrict . encodeUtf8 . T.pack $ printTerm add1
    ]
