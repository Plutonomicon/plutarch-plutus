{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Utils (
  checkLedgerProperties,
  checkLedgerPropertiesValue,
  fewerTests,
) where

import Laws (pisDataLaws, ptryFromLaws, ptryFromLawsValue, punsafeLiftDeclLaws)
import Plutarch.LedgerApi.V1 qualified as PLA
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V1.Orphans ()
import Prettyprinter (Pretty)
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

checkLedgerProperties ::
  forall (a :: S -> Type).
  ( Typeable a
  , PUnsafeLiftDecl a
  , PTryFrom PData a
  , Eq (PLifted a)
  , Show (PLifted a)
  , Arbitrary (PLifted a)
  , PIsData a
  , Plutus.ToData (PLifted a)
  , Typeable (PLifted a)
  , Pretty (PLifted a)
  ) =>
  TestTree
checkLedgerProperties =
  testGroup (typeName @(S -> Type) @a) . mconcat $
    [ punsafeLiftDeclLaws @a punsafeLiftDeclLawsName
    , pisDataLaws @a (typeName @(S -> Type) @a)
    , ptryFromLaws @a
    ]
  where
    punsafeLiftDeclLawsName :: String
    punsafeLiftDeclLawsName =
      typeName @(S -> Type) @a
        <> " <-> "
        <> typeName @Type @(PLifted a)

-- This is an ugly kludge because PValue doesn't have a direct PData conversion,
-- and bringing one in would break too much other stuff to be worth it.
checkLedgerPropertiesValue :: TestTree
checkLedgerPropertiesValue =
  testGroup "PValue" . mconcat $
    [ punsafeLiftDeclLaws @(PLA.PValue PLA.Unsorted PLA.NoGuarantees) "PValue <-> Value"
    , pisDataLaws @(PLA.PValue PLA.Unsorted PLA.NoGuarantees) "PValue"
    , ptryFromLawsValue
    ]

fewerTests :: QuickCheckTests -> QuickCheckTests -> QuickCheckTests
fewerTests divisor = (`quot` divisor)

-- Helpers

typeName :: forall k (a :: k). Typeable a => String
typeName = tyConName . typeRepTyCon $ typeRep @a
