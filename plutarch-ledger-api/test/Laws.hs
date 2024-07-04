{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Laws (
  punsafeLiftDeclLaws,
  pisDataLaws,
  ptryFromLaws,
  ptryFromLawsValue,
) where

import Plutarch.Builtin (pforgetData)
import Plutarch.LedgerApi.V1 qualified as V1
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans ()
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  forAllShrinkShow,
  (===),
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- plift . pconstant = id
punsafeLiftDeclLaws ::
  forall (a :: S -> Type).
  ( PUnsafeLiftDecl a
  , Eq (PLifted a)
  , Show (PLifted a)
  , Arbitrary (PLifted a)
  , Pretty (PLifted a)
  ) =>
  String ->
  [TestTree]
punsafeLiftDeclLaws propName =
  [ testProperty propName . forAllShrinkShow arbitrary shrink prettyShow $ \(x :: PLifted a) ->
      plift (pconstant x) === x
  ]

-- pfromData . pdata = id
-- plift . pforgetData . pdata . pconstant = toData
-- plift . pfromData . punsafeCoerce @(PAsData X) . pconstant . toData = id
pisDataLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  , PIsData a
  , Eq (PLifted a)
  , Plutus.ToData (PLifted a)
  , Pretty (PLifted a)
  ) =>
  String ->
  [TestTree]
pisDataLaws tyName =
  [ fromToProp
  , toDataProp
  , coerceProp
  ]
  where
    fromToProp :: TestTree
    fromToProp =
      testProperty "pfromData . pdata = id"
        . forAllShrinkShow arbitrary shrink prettyShow
        $ \(x :: PLifted a) ->
          plift (pfromData . pdata . pconstant $ x) === x
    toDataProp :: TestTree
    toDataProp =
      testProperty "plift . pforgetData . pdata . pconstant = toData"
        . forAllShrinkShow arbitrary shrink prettyShow
        $ \(x :: PLifted a) ->
          plift (pforgetData . pdata . pconstant $ x) === Plutus.toData x
    coerceProp :: TestTree
    coerceProp =
      testProperty coerceName
        . forAllShrinkShow arbitrary shrink prettyShow
        $ \(x :: PLifted a) ->
          plift (pfromData . punsafeCoerce @_ @_ @(PAsData a) . pconstant . Plutus.toData $ x) === x
    coerceName :: String
    coerceName = "plift . pfromData . punsafeCoerce @(PAsData " <> tyName <> ") . pconstant . toData = id"

-- ptryFrom should successfully parse a toData of a type
ptryFromLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  , Eq (PLifted a)
  , PTryFrom PData a
  , Plutus.ToData (PLifted a)
  , Pretty (PLifted a)
  ) =>
  [TestTree]
ptryFromLaws = [pDataAgreementProp]
  where
    pDataAgreementProp :: TestTree
    pDataAgreementProp = testProperty "can parse toData of original"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(x :: PLifted a) ->
        plift (ptryFrom @a (pconstant . Plutus.toData $ x) fst) === x

-- This is an ugly kludge because PValue doesn't have a direct PData conversion,
-- and bringing one in would break too much other stuff to be worth it.
ptryFromLawsValue :: [TestTree]
ptryFromLawsValue = [pDataAgreementProp]
  where
    pDataAgreementProp :: TestTree
    pDataAgreementProp = testProperty "can parse toData of original"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(v :: PLA.Value) ->
        plift (pfromData . ptryFrom @(PAsData (V1.PValue V1.Unsorted V1.NoGuarantees)) (pconstant . Plutus.toData $ v) $ fst) === v

-- Helpers

prettyShow :: forall (a :: Type). Pretty a => a -> String
prettyShow = renderString . layoutPretty defaultLayoutOptions . pretty
