{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (POpaque, popaque, printTerm)
import Plutarch.Api.V1 (PScriptPurpose (PMinting))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin)
import Plutus.V1.Ledger.Value (CurrencySymbol, currencySymbol)
import Plutus.V2.Ledger.Contexts (ScriptPurpose (Minting))
import qualified PlutusCore as PLC
import qualified PlutusTx

import Utils

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "all tests" [standardTests] -- , shrinkTests ]

standardTests :: TestTree
standardTests = testGroup "standard tests" [let ?tester = standardTester in tests]

tests :: HasTester => TestTree
tests =
  testGroup
    "unit tests"
    [ plutarchTests
    ]

plutarchTests :: HasTester => TestTree
plutarchTests =
  testGroup
    "plutarch tests"
    [ testCase "ScriptPurpose literal" $
        let d :: ScriptPurpose
            d = Minting dummyCurrency
            f :: Term s PScriptPurpose
            f = pconstant @PScriptPurpose d
         in printTerm f @?= "(program 1.0.0 #d8799f58201111111111111111111111111111111111111111111111111111111111111111ff)"
    , testCase "decode ScriptPurpose" $
        let d :: ScriptPurpose
            d = Minting dummyCurrency
            d' :: Term s PScriptPurpose
            d' = pconstant @PScriptPurpose d
            f :: Term s POpaque
            f = pmatch d' $ \case
              PMinting c -> popaque c
              _ -> perror
         in printTerm f @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> force (force ifThenElse (equalsInteger 0 i2) (delay i1) (delay error))) (force (force sndPair) i2)) (force (force fstPair) i1)) (unConstrData #d8799f58201111111111111111111111111111111111111111111111111111111111111111ff)))"
    , testCase "PData equality" $ do
        expect $ let dat = pconstant @PData (PlutusTx.List [PlutusTx.Constr 1 [PlutusTx.I 0]]) in dat #== dat
        expect $ pnot #$ pconstant @PData (PlutusTx.Constr 0 []) #== pconstant @PData (PlutusTx.I 42)
    , testCase "PAsData equality" $ do
        expect $ let dat = pdata @PInteger 42 in dat #== dat
        expect $ pnot #$ pdata (phexByteStr "12") #== pdata (phexByteStr "ab")
    ]

dummyCurrency :: CurrencySymbol
dummyCurrency = currencySymbol "\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11"
