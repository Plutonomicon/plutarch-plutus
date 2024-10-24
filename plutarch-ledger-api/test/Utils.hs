{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Utils (
  checkLedgerProperties,
  checkLedgerPropertiesValue,
  checkLedgerPropertiesAssocMap,
  checkLedgerPropertiesPCountable,
  checkLedgerPropertiesPCountableVia,
  checkLedgerPropertiesPEnumerable,
  fromPPositive,
  toPPositive,
  fewerTests,
) where

import Laws (
  pcountableLaws,
  pcountableLawsVia,
  penumerableLaws,
  pisDataLaws,
  ptryFromLaws,
  ptryFromLawsAssocMap,
  ptryFromLawsValue,
  punsafeLiftDeclLaws,
 )
import Plutarch.Enum (PCountable, PEnumerable)
import Plutarch.LedgerApi.V1 qualified as PLA
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Num (PNum (pfromInteger))
import Plutarch.Positive (PPositive)
import Plutarch.Prelude
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V1.Orphans ()
import Prettyprinter (Pretty)
import Test.QuickCheck (Arbitrary, Positive (getPositive))
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

-- Same as above
checkLedgerPropertiesAssocMap :: TestTree
checkLedgerPropertiesAssocMap =
  testGroup "PMap" . mconcat $
    [ punsafeLiftDeclLaws @(PLA.PMap PLA.Unsorted PInteger PInteger) "PMap <-> AssocMap.Map"
    , pisDataLaws @(PLA.PMap PLA.Unsorted PInteger PInteger) "PMap"
    , ptryFromLawsAssocMap
    ]

checkLedgerPropertiesPCountable ::
  forall (a :: S -> Type).
  ( Typeable a
  , PCountable a
  , Arbitrary (PLifted a)
  , Eq (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  ) =>
  TestTree
checkLedgerPropertiesPCountable = testGroup (typeName @(S -> Type) @a) (pcountableLaws @a)

checkLedgerPropertiesPCountableVia ::
  forall (input :: Type) (output :: Type) (plutarch :: S -> Type).
  ( Typeable plutarch
  , PCountable plutarch
  , Arbitrary input
  , Show input
  , Eq output
  , Show output
  ) =>
  (input -> ClosedTerm plutarch) ->
  (ClosedTerm plutarch -> output) ->
  TestTree
checkLedgerPropertiesPCountableVia mkInput mkOutput =
  testGroup (typeName @(S -> Type) @plutarch) (pcountableLawsVia mkInput mkOutput)

toPPositive :: Positive Integer -> ClosedTerm PPositive
toPPositive = pfromInteger . getPositive

fromPPositive :: ClosedTerm PPositive -> Integer
fromPPositive t = plift $ pto t

checkLedgerPropertiesPEnumerable ::
  forall (a :: S -> Type).
  ( Typeable a
  , PEnumerable a
  , Arbitrary (PLifted a)
  , Pretty (PLifted a)
  , Eq (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  ) =>
  TestTree
checkLedgerPropertiesPEnumerable = testGroup (typeName @(S -> Type) @a) (penumerableLaws @a)

fewerTests :: QuickCheckTests -> QuickCheckTests -> QuickCheckTests
fewerTests divisor = (`quot` divisor)

-- Helpers

typeName :: forall k (a :: k). Typeable a => String
typeName = tyConName . typeRepTyCon $ typeRep @a
