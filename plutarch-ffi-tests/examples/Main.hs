{-# LANGUAGE TemplateHaskell #-}

module Examples.FFI (tests) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
--import qualified Ledger.Typed.Scripts as TypedScripts
import Plutarch (printScript)
import qualified PlutusTx
{-
import Plutarch.Bool (PBool (PFalse, PTrue), pif, (#==))
import Plutarch.Builtin (PAsData, PIsData, pasConstr, pdata, pforgetData, pfromData)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.Rec (
  DataReader (DataReader, readData),
  DataWriter (DataWriter, writeData),
  PRecord (PRecord),
  RecordFromData,
  ScottEncoded,
  ScottEncoding,
  field,
  fieldFromData,
  letrec,
  rcon,
  recordDataFromFieldWriters,
  recordFromFieldReaders,
  rmatch,
 )
import Plutarch.Rec.TH (deriveAll)
import Plutarch.String (PString, pdecodeUtf8, pencodeUtf8)
import qualified Rank2.TH
-}
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (HasTester)

tests :: HasTester => TestTree
tests =
  testGroup
    "FFI"
    [ testCase "Integer" $ printScript (fromCompiledCode $$(PlutusTx.compile [|| 42 :: Integer ||])) @?= "program 1.0.0 42"]
