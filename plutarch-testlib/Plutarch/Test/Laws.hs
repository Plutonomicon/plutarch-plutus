{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Laws (
  checkLedgerPropertiesValue,
  checkLedgerPropertiesAssocMap,
  checkLedgerProperties,
  checkLedgerPropertiesPCountable,
  checkLedgerPropertiesPEnumerable,
  checkHaskellEquivalent,
  checkHaskellEquivalentN,
  ordHaskellEquivalents,
) where

import GHC.TypeError (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import Plutarch.Builtin (pforgetData)
import Plutarch.Enum (PCountable (psuccessor, psuccessorN), PEnumerable (ppredecessor, ppredecessorN))
import Plutarch.LedgerApi.V1 qualified as V1
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Positive (Positive)
import Plutarch.Prelude
import Plutarch.Test.Utils (instanceOfType, prettyEquals, prettyShow, typeName, typeName')
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

{- | Like `checkLedgerProperties` but specialized to `PValue`

This is an ugly kludge because PValue doesn't have a direct PData conversion,
and bringing one in would break too much other stuff to be worth it.

@since WIP
-}
checkLedgerPropertiesValue :: TestTree
checkLedgerPropertiesValue =
  testGroup "PValue" . mconcat $
    [ punsafeLiftDeclLaws @(V1.PValue V1.Unsorted V1.NoGuarantees) "PValue <-> Value"
    , pisDataLaws @(V1.PValue V1.Unsorted V1.NoGuarantees) "PValue"
    , ptryFromLawsValue
    ]

{- | Like `checkLedgerProperties` but specialized to `PMap`

Same as above

@since WIP
-}
checkLedgerPropertiesAssocMap :: TestTree
checkLedgerPropertiesAssocMap =
  testGroup "PMap" . mconcat $
    [ punsafeLiftDeclLaws @(V1.PMap V1.Unsorted PInteger PInteger) "PMap <-> AssocMap.Map"
    , pisDataLaws @(V1.PMap V1.Unsorted PInteger PInteger) "PMap"
    , ptryFromLawsAssocMap
    ]

-- | @since WIP
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

-- | @since WIP
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

-- | @since WIP
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

-- Solo from base has no `Arbitrary` instance
newtype QcSolo a = QcSolo a
  deriving newtype (Show, Eq, Pretty, Arbitrary)

newtype PQcSolo (a :: S -> Type) s = PQcSolo (Term s a)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PIntegral, PShow)

instance DerivePlutusType (PQcSolo a) where type DPTStrat _ = PlutusTypeNewtype

instance
  (PUnsafeLiftDecl a, PConstanted (PLifted a) ~ a, PConstanted (QcSolo (PLifted a)) ~ PQcSolo a) =>
  PUnsafeLiftDecl (PQcSolo a)
  where
  type PLifted (PQcSolo a) = QcSolo (PLifted a)

deriving via
  (DerivePConstantViaNewtype (QcSolo a) (PQcSolo (PConstanted a)) (PConstanted a))
  instance
    PConstant a => PConstantDecl (QcSolo a)

type family ToPlutarchFunction f where
  ToPlutarchFunction (a -> b -> c) = PConstanted a :--> ToPlutarchFunction (b -> c)
  ToPlutarchFunction (a -> b) = PConstanted a :--> PConstanted b
  ToPlutarchFunction a = TypeError (Text "Not a function: " :<>: ShowType a)

type family FunctionArgumentsToTuple f where
  FunctionArgumentsToTuple (a -> (b -> c)) = (a, FunctionArgumentsToTuple (b -> c))
  FunctionArgumentsToTuple (a -> _) = QcSolo a

type family FunctionResult f where
  FunctionResult (_ -> (b -> c)) = FunctionResult (b -> c)
  FunctionResult (_ -> b) = b

class ApplyTupleArgs f where
  applyTupleArgs :: f -> FunctionArgumentsToTuple f -> FunctionResult f

instance
  ( f ~ (a -> c -> d)
  , b ~ (c -> d)
  , FunctionResult b ~ FunctionResult (a -> b)
  , FunctionArgumentsToTuple f ~ (a, rest)
  , FunctionArgumentsToTuple b ~ rest
  , ApplyTupleArgs b
  ) =>
  ApplyTupleArgs (a -> c -> d)
  where
  applyTupleArgs f (a, rest) = applyTupleArgs (f a) rest

instance
  {-# OVERLAPPABLE #-}
  ( f ~ (a -> b)
  , FunctionArgumentsToTuple f ~ QcSolo a
  , FunctionResult f ~ b
  ) =>
  ApplyTupleArgs (a -> b)
  where
  applyTupleArgs f (QcSolo a) = f a

type family PFunctionArgumentsToTuple (f :: S -> Type) :: S -> Type where
  PFunctionArgumentsToTuple (a :--> (b :--> c)) =
    PBuiltinPair a (PFunctionArgumentsToTuple (b :--> c))
  PFunctionArgumentsToTuple (a :--> _) = PQcSolo a

type family PFunctionResult (f :: S -> Type) where
  PFunctionResult (_ :--> (b :--> c)) = PFunctionResult (b :--> c)
  PFunctionResult (_ :--> b) = b

class PApplyTupleArgs (f :: S -> Type) where
  papplyTupleArgs :: Term s f -> Term s (PFunctionArgumentsToTuple f) -> Term s (PFunctionResult f)

instance
  {-# OVERLAPPABLE #-}
  ( f ~ (a :--> b)
  , PFunctionArgumentsToTuple f ~ PQcSolo a
  , PFunctionResult f ~ b
  ) =>
  PApplyTupleArgs f
  where
  papplyTupleArgs f args = f # pto args

instance
  ( f ~ (a :--> c :--> d)
  , b ~ (c :--> d)
  , PFunctionResult b ~ PFunctionResult (c :--> d)
  , PFunctionArgumentsToTuple f ~ PBuiltinPair a rest
  , PApplyTupleArgs b
  ) =>
  PApplyTupleArgs (a :--> c :--> d)
  where
  papplyTupleArgs f args = papplyTupleArgs (f # (pfstBuiltin # args)) (psndBuiltin # args)

-- | @since WIP
checkHaskellEquivalentN ::
  forall f.
  ( Arbitrary (FunctionArgumentsToTuple f)
  , Pretty (FunctionArgumentsToTuple f)
  , Pretty (FunctionResult f)
  , Eq (FunctionResult f)
  , ApplyTupleArgs f
  , PApplyTupleArgs (ToPlutarchFunction f)
  , PLifted (PFunctionResult (ToPlutarchFunction f)) ~ FunctionResult f
  , FunctionArgumentsToTuple f ~ PLifted (PFunctionArgumentsToTuple (ToPlutarchFunction f))
  , PUnsafeLiftDecl (PFunctionArgumentsToTuple (ToPlutarchFunction f))
  , PUnsafeLiftDecl (PFunctionResult (ToPlutarchFunction f))
  ) =>
  f ->
  ClosedTerm (ToPlutarchFunction f) ->
  Property
checkHaskellEquivalentN goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \input -> applyTupleArgs goHaskell input `prettyEquals` plift (papplyTupleArgs goPlutarch (pconstant input))

-- | @since WIP
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

-- | @since WIP
ordHaskellEquivalents ::
  forall (haskellInput :: Type).
  ( Typeable haskellInput
  , Typeable (PConstanted haskellInput)
  , Ord haskellInput
  , haskellInput ~ PLifted (PConstanted haskellInput)
  , PPartialOrd (PConstanted haskellInput)
  , PConstantDecl haskellInput
  , Pretty haskellInput
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
    [ testProperty "== = #==" $ checkHaskellEquivalentN ((==) @haskellInput) (plam (#==))
    , testProperty "< = #<" $ checkHaskellEquivalentN ((<) @haskellInput) (plam (#<))
    , testProperty "<= = #<=" $ checkHaskellEquivalentN ((<=) @haskellInput) (plam (#<=))
    ]

-- Internal

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
