module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Integer (PInteger)
import Plutarch.Positive (PPositive)
import Plutarch.Test.Laws (checkLedgerPropertiesPCountable, checkLedgerPropertiesPEnumerable)
import Plutarch.Test.Suite.Plutarch.Bool qualified as Bool
import Plutarch.Test.Suite.Plutarch.ByteString qualified as ByteString
import Plutarch.Test.Suite.Plutarch.Either qualified as Either
import Plutarch.Test.Suite.Plutarch.Field qualified as Field
import Plutarch.Test.Suite.Plutarch.Integer qualified as Integer
import Plutarch.Test.Suite.Plutarch.List qualified as List
import Plutarch.Test.Suite.Plutarch.Maybe qualified as Maybe
import Plutarch.Test.Suite.Plutarch.Monadic qualified as Monadic
import Plutarch.Test.Suite.Plutarch.PLam qualified as PLam
import Plutarch.Test.Suite.Plutarch.POrd qualified as POrd
import Plutarch.Test.Suite.Plutarch.Pair qualified as Pair
import Plutarch.Test.Suite.Plutarch.Rational qualified as Rational
import Plutarch.Test.Suite.Plutarch.Recursion qualified as Recursion
import Plutarch.Test.Suite.Plutarch.Show qualified as Show
import Plutarch.Test.Suite.Plutarch.String qualified as String
import Plutarch.Test.Suite.PlutarchLedgerApi.Regressions qualified as Regressions
import Plutarch.Test.Suite.PlutarchLedgerApi.V1 qualified as V1
import Plutarch.Test.Suite.PlutarchLedgerApi.V2 qualified as V2
import Plutarch.Test.Suite.PlutarchLedgerApi.V3 qualified as V3
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)

main :: IO ()
main = do
  -- Pre-emptively avoid encoding issues
  setLocaleEncoding utf8
  defaultMain . adjustOption moreTests . testGroup "Tests" $
    [ testGroup
        "Plutarch"
        [ testGroup
            "Laws"
            [ testGroup
                "PCountable"
                [ checkLedgerPropertiesPCountable @PInteger
                , checkLedgerPropertiesPCountable @PPositive
                ]
            , testGroup
                "PEnumerable"
                [ checkLedgerPropertiesPEnumerable @PInteger
                ]
            ]
        , Bool.tests
        , ByteString.tests
        , Either.tests
        , Field.tests
        , Integer.tests
        , List.tests
        , Maybe.tests
        , Monadic.tests
        , Pair.tests
        , PLam.tests
        , POrd.tests
        , Rational.tests
        , Recursion.tests
        , Show.tests
        , String.tests
        ]
    , testGroup
        "PlutarchLedgerApi"
        [ testGroup
            "Laws"
            [ V1.tests
            , V2.tests
            , V3.tests
            ]
        , testGroup "Regressions" Regressions.tests
        ]
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000
