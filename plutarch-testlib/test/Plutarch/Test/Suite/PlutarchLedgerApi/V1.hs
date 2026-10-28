module Plutarch.Test.Suite.PlutarchLedgerApi.V1 (tests) where

import Data.Kind (Type)
import Plutarch.Evaluate (evalTerm')
import Plutarch.Internal.Term (Config (NoTracing))
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
import Plutarch.Test.Methods (
  ppredecessorNBetter,
  pscaleIntegerBetter,
  pscaleNaturalBetter,
  pscalePositiveBetter,
  psuccessorNBetter,
 )
import Plutarch.Test.QuickCheck (propPTryFromRoundtrip)
import Plutarch.Test.Suite.PlutarchLedgerApi.V1.Interval qualified as Interval
import Plutarch.Test.Suite.PlutarchLedgerApi.V1.Value qualified as Value
import Plutarch.Test.Utils (fewerTests, typeName)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Orphans ()
import Test.Tasty (TestTree, adjustOption, testGroup)

tests :: TestTree
tests =
  testGroup
    "V1"
    [ testGroup
        (typeName @(S -> Type) @PLA.PScriptPurpose)
        [ checkLedgerProperties @PLA.PScriptPurpose
        , propPTryFromRoundtrip @PLA.PScriptPurpose
        ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PScriptContext)
          [ checkLedgerProperties @PLA.PScriptContext
          , propPTryFromRoundtrip @PLA.PScriptContext
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDCert)
        [ checkLedgerProperties @PLA.PDCert
        , propPTryFromRoundtrip @PLA.PDCert
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PCredential)
        [ checkLedgerProperties @PLA.PCredential
        , propPTryFromRoundtrip @PLA.PCredential
        , checkHaskellOrdEquivalent @PLA.PCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PStakingCredential)
        [ checkLedgerProperties @PLA.PStakingCredential
        , propPTryFromRoundtrip @PLA.PStakingCredential
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PLovelace)
        [ checkLedgerProperties @PLA.PLovelace
        , propPTryFromRoundtrip @PLA.PLovelace
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PCurrencySymbol)
        [ checkLedgerProperties @PLA.PCurrencySymbol
        , propPTryFromRoundtrip @PLA.PCurrencySymbol
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTokenName)
        [ checkLedgerProperties @PLA.PTokenName
        , propPTryFromRoundtrip @PLA.PTokenName
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PPosixTime)
        [ checkLedgerPropertiesPCountable @PLA.PPosixTime
        , checkLedgerPropertiesPEnumerable @PLA.PPosixTime
        , checkLedgerProperties @PLA.PPosixTime
        , checkPAdditiveSemigroupLaws @PLA.PPosixTime
        , checkPAdditiveMonoidLaws @PLA.PPosixTime
        , checkPAdditiveGroupLaws @PLA.PPosixTime
        , propPTryFromRoundtrip @PLA.PPosixTime
        , psuccessorNBetter (punsafeCoerce @_ @PInteger 10) (evalTerm' NoTracing $ PLA.pposixTime 2000)
        , ppredecessorNBetter (punsafeCoerce @_ @PInteger 10) (evalTerm' NoTracing $ PLA.pposixTime 2000)
        , pscalePositiveBetter (evalTerm' NoTracing $ PLA.pposixTime 2000) (punsafeCoerce @_ @PInteger 10)
        , pscaleNaturalBetter (evalTerm' NoTracing $ PLA.pposixTime 2000) (punsafeCoerce @_ @PInteger 10)
        , pscaleIntegerBetter (evalTerm' NoTracing $ PLA.pposixTime 2000) 10
        ]
    , -- We only care about intervals of PPosixTime, so we don't check anything else
      testGroup
        (typeName @(S -> Type) @(PLA.PExtended PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PExtended PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PExtended PLA.PPosixTime)
        , propPTryFromRoundtrip @(PLA.PExtended PLA.PPosixTime)
        ]
    , testGroup
        (typeName @(S -> Type) @(PLA.PLowerBound PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PLowerBound PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PLowerBound PLA.PPosixTime)
        , propPTryFromRoundtrip @(PLA.PLowerBound PLA.PPosixTime)
        ]
    , testGroup
        (typeName @(S -> Type) @(PLA.PUpperBound PLA.PPosixTime))
        [ checkHaskellOrdEquivalent @(PLA.PUpperBound PLA.PPosixTime)
        , checkLedgerProperties @(PLA.PUpperBound PLA.PPosixTime)
        , propPTryFromRoundtrip @(PLA.PUpperBound PLA.PPosixTime)
        ]
    , Interval.tests
    , testGroup
        (typeName @(S -> Type) @PLA.PDatum)
        [ checkLedgerProperties @PLA.PDatum
        , propPTryFromRoundtrip @PLA.PDatum
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PRedeemer)
        [ checkLedgerProperties @PLA.PRedeemer
        , propPTryFromRoundtrip @PLA.PRedeemer
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PDatumHash)
        [ checkLedgerProperties @PLA.PDatumHash
        , propPTryFromRoundtrip @PLA.PDatumHash
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PRedeemerHash)
        [ checkLedgerProperties @PLA.PRedeemerHash
        , propPTryFromRoundtrip @PLA.PRedeemerHash
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PScriptHash)
        [ checkLedgerProperties @PLA.PScriptHash
        , propPTryFromRoundtrip @PLA.PScriptHash
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PAddress)
        [ checkLedgerProperties @PLA.PAddress
        , propPTryFromRoundtrip @PLA.PAddress
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxId)
        [ checkLedgerProperties @PLA.PTxId
        , propPTryFromRoundtrip @PLA.PTxId
        ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxOut)
          [ checkLedgerProperties @PLA.PTxOut
          , propPTryFromRoundtrip @PLA.PTxOut
          ]
    , adjustOption (fewerTests 4) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInInfo)
          [ checkLedgerProperties @PLA.PTxInInfo
          , propPTryFromRoundtrip @PLA.PTxInInfo
          ]
    , testGroup
        (typeName @(S -> Type) @PLA.PTxOutRef)
        [ checkLedgerProperties @PLA.PTxOutRef
        , propPTryFromRoundtrip @PLA.PTxOutRef
        ]
    , testGroup
        (typeName @(S -> Type) @PLA.PPubKeyHash)
        [ checkLedgerProperties @PLA.PPubKeyHash
        , propPTryFromRoundtrip @PLA.PPubKeyHash
        ]
    , adjustOption (fewerTests 16) $
        testGroup
          (typeName @(S -> Type) @PLA.PTxInfo)
          [ checkLedgerProperties @PLA.PTxInfo
          , propPTryFromRoundtrip @PLA.PTxInfo
          ]
    , Value.tests
    , testGroup
        (typeName @(S -> Type) @Value.PAssetClass)
        [ checkLedgerProperties @Value.PAssetClass
        , propPTryFromRoundtrip @Value.PAssetClass
        ]
    ]
