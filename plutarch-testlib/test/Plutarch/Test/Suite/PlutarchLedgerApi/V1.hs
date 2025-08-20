module Plutarch.Test.Suite.PlutarchLedgerApi.V1 (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V1 qualified as PLA
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import Plutarch.Test.Laws (
  checkHaskellOrdEquivalent,
  checkLedgerProperties,
  checkLedgerPropertiesPCountable,
  checkLedgerPropertiesPEnumerable,
  checkPAdditiveGroupLaws,
  checkPAdditiveMonoidLaws,
  checkPAdditiveSemigroupLaws,
 )
import Plutarch.Test.QuickCheck (propPTryFromRoundrip)
import Plutarch.Test.Suite.PlutarchLedgerApi.V1.Interval qualified as Interval
import Plutarch.Test.Suite.PlutarchLedgerApi.V1.Value qualified as Value
import Plutarch.Test.Utils (fewerTests, typeName)
import PlutusLedgerApi.V1.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V1"
    [ testGroup
        (typeName @(S -> Type) @PLA.PScriptPurpose)
        [ checkLedgerProperties @PLA.PScriptPurpose
        , propPTryFromRoundrip @PLA.PScriptPurpose
        ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PScriptContext)
          [ checkLedgerProperties @PLA.PScriptContext
          , propPTryFromRoundrip @PLA.PScriptContext
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDCert)
        [ checkLedgerProperties @PLA.PDCert
        , propPTryFromRoundrip @PLA.PDCert
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PCredential)
        [ checkLedgerProperties @PLA.PCredential
        , propPTryFromRoundrip @PLA.PCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PStakingCredential)
        [ checkLedgerProperties @PLA.PStakingCredential
        , propPTryFromRoundrip @PLA.PStakingCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PLovelace)
        [ checkLedgerProperties @PLA.PLovelace
        , propPTryFromRoundrip @PLA.PLovelace
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PCurrencySymbol)
        [ checkLedgerProperties @PLA.PCurrencySymbol
        , propPTryFromRoundrip @PLA.PCurrencySymbol
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTokenName)
        [ checkLedgerProperties @PLA.PTokenName
        , propPTryFromRoundrip @PLA.PTokenName
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PPosixTime)
        [ checkLedgerPropertiesPCountable @PLA.PPosixTime
        , checkLedgerPropertiesPEnumerable @PLA.PPosixTime
        , checkLedgerProperties @PLA.PPosixTime
        , checkPAdditiveSemigroupLaws @PLA.PPosixTime
        , checkPAdditiveMonoidLaws @PLA.PPosixTime
        , checkPAdditiveGroupLaws @PLA.PPosixTime
        , propPTryFromRoundrip @PLA.PPosixTime
        ]
    , -- We only care about intervals of PPosixTime, so we don't check anything else
      testGroup
        (typeName @(S -> Type) @(PLA.PExtended PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PExtended PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PExtended PLA.PPosixTime)
        , propPTryFromRoundrip @(PLA.PExtended PLA.PPosixTime)
        ]
    , testGroup
        (typeName @(S -> Type) @(PLA.PLowerBound PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PLowerBound PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PLowerBound PLA.PPosixTime)
        , propPTryFromRoundrip @(PLA.PLowerBound PLA.PPosixTime)
        ]
    , testGroup
        (typeName @(S -> Type) @(PLA.PUpperBound PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PUpperBound PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PUpperBound PLA.PPosixTime)
        , propPTryFromRoundrip @(PLA.PUpperBound PLA.PPosixTime)
        ]
    , Interval.tests
    , testGroup
        (typeName @(S -> Type) @PLA.PDatum)
        [ checkLedgerProperties @PLA.PDatum
        , propPTryFromRoundrip @PLA.PDatum
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PRedeemer)
        [ checkLedgerProperties @PLA.PRedeemer
        , propPTryFromRoundrip @PLA.PRedeemer
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDatumHash)
        [ checkLedgerProperties @PLA.PDatumHash
        , propPTryFromRoundrip @PLA.PDatumHash
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PRedeemerHash)
        [ checkLedgerProperties @PLA.PRedeemerHash
        , propPTryFromRoundrip @PLA.PRedeemerHash
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptHash)
        [ checkLedgerProperties @PLA.PScriptHash
        , propPTryFromRoundrip @PLA.PScriptHash
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAddress)
        [ checkLedgerProperties @PLA.PAddress
        , propPTryFromRoundrip @PLA.PAddress
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxId)
        [ checkLedgerProperties @PLA.PTxId
        , propPTryFromRoundrip @PLA.PTxId
        ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxOut)
          [ checkLedgerProperties @PLA.PTxOut
          , propPTryFromRoundrip @PLA.PTxOut
          ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInInfo)
          [ checkLedgerProperties @PLA.PTxInInfo
          , propPTryFromRoundrip @PLA.PTxInInfo
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxOutRef)
        [ checkLedgerProperties @PLA.PTxOutRef
        , propPTryFromRoundrip @PLA.PTxOutRef
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PPubKeyHash)
        [ checkLedgerProperties @PLA.PPubKeyHash
        , propPTryFromRoundrip @PLA.PPubKeyHash
        ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInfo)
          [ checkLedgerProperties @PLA.PTxInfo
          , propPTryFromRoundrip @PLA.PTxInfo
          ]
    , Value.tests
    , testGroup
        (typeName @(S -> Type) @Value.PAssetClass)
        [ checkLedgerProperties @Value.PAssetClass
        , propPTryFromRoundrip @Value.PAssetClass
        ]
    ]
