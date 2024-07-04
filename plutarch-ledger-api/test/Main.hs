{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)
import V1 qualified
import V2 qualified
import V3 qualified

main :: IO ()
main = do
  -- Pre-emptively avoid encoding issues
  setLocaleEncoding utf8
  defaultMain . adjustOption moreTests . testGroup "Laws" $
    [ V1.tests
    , V2.tests
    , V3.tests
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000
