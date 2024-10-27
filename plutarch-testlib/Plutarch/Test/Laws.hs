{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Plutarch.Test.Laws (
  punsafeLiftDeclLaws,
  pisDataLaws,
  ptryFromLaws,
  pcountableLaws,
  penumerableLaws,
  ptryFromLawsValue,
  ptryFromLawsAssocMap,
  checkLedgerPropertiesValue,
  checkLedgerPropertiesAssocMap,
  checkLedgerProperties,
  checkLedgerPropertiesPCountable,
  checkLedgerPropertiesPEnumerable,
  checkHaskellEquivalent,
  ordHaskellEquivalents,
) where

import Plutarch.Builtin (pforgetData)
import Plutarch.Enum (PCountable (psuccessor, psuccessorN), PEnumerable (ppredecessor, ppredecessorN))
import Plutarch.LedgerApi.V1 qualified as V1
import Plutarch.Lift (PConstantDecl (PConstanted), PUnsafeLiftDecl (PLifted))
import Plutarch.Positive (Positive)
import Plutarch.Prelude
import Plutarch.Test.Utils (instanceOfType, prettyShow, typeName, typeName')
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans ()
import PlutusTx.AssocMap qualified as AssocMap
import Prettyprinter (Pretty)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  forAllShrinkShow,
  (=/=),
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Type.Reflection (Typeable, typeRep)

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

-- Same as before
ptryFromLawsAssocMap :: [TestTree]
ptryFromLawsAssocMap = [pDataAgreementProp]
  where
    pDataAgreementProp :: TestTree
    pDataAgreementProp = testProperty "can parse toData of original"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(v :: AssocMap.Map Integer Integer) ->
        plift (pfromData . ptryFrom @(PAsData (V1.PMap V1.Unsorted PInteger PInteger)) (pconstant . Plutus.toData $ v) $ fst)
          === v

-- This is an ugly kludge because PValue doesn't have a direct PData conversion,
-- and bringing one in would break too much other stuff to be worth it.
checkLedgerPropertiesValue :: TestTree
checkLedgerPropertiesValue =
  testGroup "PValue" . mconcat $
    [ punsafeLiftDeclLaws @(V1.PValue V1.Unsorted V1.NoGuarantees) "PValue <-> Value"
    , pisDataLaws @(V1.PValue V1.Unsorted V1.NoGuarantees) "PValue"
    , ptryFromLawsValue
    ]

-- Same as above
checkLedgerPropertiesAssocMap :: TestTree
checkLedgerPropertiesAssocMap =
  testGroup "PMap" . mconcat $
    [ punsafeLiftDeclLaws @(V1.PMap V1.Unsorted PInteger PInteger) "PMap <-> AssocMap.Map"
    , pisDataLaws @(V1.PMap V1.Unsorted PInteger PInteger) "PMap"
    , ptryFromLawsAssocMap
    ]

pcountableLaws ::
  forall (a :: S -> Type).
  ( PCountable a
  , Arbitrary (PLifted a)
  , Pretty (PLifted a)
  , Eq (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  ) =>
  [TestTree]
pcountableLaws =
  [ testProperty "x /= psuccessor x" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: PLifted a) ->
        plift (psuccessor # pconstant x) =/= x
  , testProperty "y < x = psuccessor y <= x" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: PLifted a, y :: PLifted a) ->
        plift (pconstant y #< pconstant x) === plift ((psuccessor # pconstant y) #<= pconstant x)
  , testProperty "x < psuccessor y = x <= y" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: PLifted a, y :: PLifted a) ->
        plift (pconstant x #< (psuccessor # pconstant y)) === plift (pconstant x #<= pconstant y)
  , testProperty "psuccessorN 1 = psuccessor" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: PLifted a) ->
        plift (psuccessorN # 1 # pconstant x) === plift (psuccessor # pconstant x)
  , testProperty "psuccessorN n . psuccessorN m = psuccessorN (n + m)" . forAllShrinkShow arbitrary shrink show $
      \(x :: PLifted a, n :: Positive, m :: Positive) ->
        plift (psuccessorN # pconstant n # (psuccessorN # pconstant m # pconstant x))
          === plift (psuccessorN # (pconstant n + pconstant m) # pconstant x)
  ]

penumerableLaws ::
  forall (a :: S -> Type).
  ( PEnumerable a
  , Arbitrary (PLifted a)
  , Pretty (PLifted a)
  , Eq (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  ) =>
  [TestTree]
penumerableLaws =
  [ testProperty "ppredecessor . psuccessor = id" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: PLifted a) ->
        plift (ppredecessor #$ psuccessor # pconstant x) === plift (pconstant x)
  , testProperty "psuccessor . ppredecessor = id" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: PLifted a) ->
        plift (psuccessor #$ ppredecessor # pconstant x) === plift (pconstant x)
  , testProperty "ppredecessorN 1 = ppredecessor" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: PLifted a) ->
        plift (ppredecessorN # 1 # pconstant x) === plift (ppredecessor # pconstant x)
  , testProperty "ppredecessorN n . ppredecessorN m = ppredecessorN (n + m)" . forAllShrinkShow arbitrary shrink show $
      \(x :: PLifted a, n :: Positive, m :: Positive) ->
        plift (ppredecessorN # pconstant n # (ppredecessorN # pconstant m # pconstant x))
          === plift (ppredecessorN # (pconstant n + pconstant m) # pconstant x)
  ]

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
  testGroup (instanceOfType @(S -> Type) @a "Ledger Laws") . mconcat $
    [ punsafeLiftDeclLaws @a punsafeLiftDeclLawsName
    , pisDataLaws @a (typeName' False (typeRep @a)) -- it'll get wrapped in PAsData so not top level
    , ptryFromLaws @a
    ]
  where
    punsafeLiftDeclLawsName :: String
    punsafeLiftDeclLawsName =
      typeName @(S -> Type) @a
        <> " <-> "
        <> typeName @Type @(PLifted a)

checkLedgerPropertiesPCountable ::
  forall (a :: S -> Type).
  ( Typeable a
  , PCountable a
  , Arbitrary (PLifted a)
  , Pretty (PLifted a)
  , Eq (PLifted a)
  , Show (PLifted a)
  , PUnsafeLiftDecl a
  ) =>
  TestTree
checkLedgerPropertiesPCountable =
  testGroup (instanceOfType @(S -> Type) @a "PCountable") (pcountableLaws @a)

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
checkLedgerPropertiesPEnumerable =
  testGroup (instanceOfType @(S -> Type) @a "PEnumerable") (penumerableLaws @a)

checkHaskellEquivalent ::
  forall (haskellInput :: Type) (haskellOutput :: Type).
  ( haskellInput ~ PLifted (PConstanted haskellInput)
  , PConstantDecl haskellInput
  , Show haskellInput
  , Arbitrary haskellInput
  , haskellOutput ~ PLifted (PConstanted haskellOutput)
  , PConstantDecl haskellOutput
  , Show haskellOutput
  , Eq haskellOutput
  ) =>
  (haskellInput -> haskellOutput) ->
  ClosedTerm (PConstanted haskellInput :--> PConstanted haskellOutput) ->
  Property
checkHaskellEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink show $
    \(input :: haskellInput) -> goHaskell input === plift (goPlutarch # pconstant input)

ordHaskellEquivalents ::
  forall (haskellInput :: Type).
  ( Typeable haskellInput
  , Typeable (PConstanted haskellInput)
  , Ord haskellInput
  , haskellInput ~ PLifted (PConstanted haskellInput)
  , PPartialOrd (PConstanted haskellInput)
  , PConstantDecl haskellInput
  , Show haskellInput
  , Arbitrary haskellInput
  ) =>
  TestTree
ordHaskellEquivalents =
  testGroup
    ( mconcat
        [ instanceOfType @Type @haskellInput "Ord"
        , " <-> "
        , instanceOfType @(S -> Type) @(PConstanted haskellInput) "POrd"
        ]
    )
    [ testProperty "== = #==" $
        checkHaskellEquivalent @(haskellInput, haskellInput) @Bool
          (uncurry (==))
          (plam $ \pair -> (pfstBuiltin # pair) #== (psndBuiltin # pair))
    , testProperty "< = #<" $
        checkHaskellEquivalent @(haskellInput, haskellInput) @Bool
          (uncurry (<))
          (plam $ \pair -> (pfstBuiltin # pair) #< (psndBuiltin # pair))
    , testProperty "<= = #<=" $
        checkHaskellEquivalent @(haskellInput, haskellInput) @Bool
          (uncurry (<=))
          (plam $ \pair -> (pfstBuiltin # pair) #<= (psndBuiltin # pair))
    ]
