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
  checkHaskellOrdEquivalent,
  checkHaskellNumEquivalent,
  checkHaskellIntegralEquivalent,
  checkPLiftableLaws,
  checkPOrdLaws,
) where

import Control.Applicative ((<|>))
import Plutarch.Builtin (pforgetData)
import Plutarch.Enum (PCountable (psuccessor, psuccessorN), PEnumerable (ppredecessor, ppredecessorN))
import Plutarch.Internal.Lift (PLiftable (fromPlutarch, fromPlutarchRepr, toPlutarch, toPlutarchRepr))
import Plutarch.Internal.Numeric (
  PNum (pabs, pnegate, psignum, (#*), (#+), (#-)),
 )
import Plutarch.LedgerApi.V1 qualified as V1
import Plutarch.Positive (PPositive, Positive)
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (checkHaskellEquivalent, checkHaskellEquivalent2)
import Plutarch.Test.Utils (instanceOfType, precompileTerm, prettyEquals, prettyShow, typeName')
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans ()
import PlutusTx.AssocMap qualified as AssocMap
import Prettyprinter (Pretty (pretty))
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  NonZero (getNonZero),
  Property,
  forAllShrinkShow,
  oneof,
  (=/=),
  (===),
 )
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Type.Reflection (Typeable, typeRep)

{- | Verifies that the specified Plutarch type satisfies the 'POrd' laws for
mandatory methods.

@since WIP
-}
checkPOrdLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , PLiftable a
  , POrd a
  ) =>
  [TestTree]
checkPOrdLaws =
  [ testProperty "#<= is reflexive" leqReflexive
  , testProperty "#<= is transitive" leqTransitive
  , testProperty "#<= is total" leqTotal
  , testProperty "#< is irreflexive" ltIrreflexive
  , testProperty "#< is transitive" ltTransitive
  , testProperty "#< is trichotomous" ltTrichotomous
  , testProperty "#< is the equivalent strict order to #<=" ltEquivLeq
  ]
  where
    leqReflexive :: Property
    leqReflexive = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a) ->
      plift (pconstant @a x #<= pconstant x)
    -- We have to restate (x <= y && y <= z) -> x <= z, which gives (after some
    -- DeMorganing) x > y || y > z || x <= z
    leqTransitive :: Property
    leqTransitive = forAllShrinkShow arbitrary shrink prettyShow $ \(t :: Triplet (AsHaskell a)) ->
      let (x, y, z) = toTriple t
       in plift
            ( let liftedX = pconstant @a x
                  liftedY = pconstant y
                  liftedZ = pconstant z
               in (liftedX #> liftedY) #|| (liftedY #> liftedZ) #|| (liftedX #<= liftedZ)
            )
    leqTotal :: Property
    leqTotal = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a) ->
      plift
        ( let liftedX = pconstant @a x
              liftedY = pconstant y
           in (liftedX #<= liftedY) #|| (liftedY #<= liftedX)
        )
    ltIrreflexive :: Property
    ltIrreflexive = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a) ->
      plift (pnot #$ pconstant @a x #< pconstant x)
    -- We have to restate (x < y && y < z) -> x < z, which gives (after some
    -- DeMorganing) x >= y || y >= z || x < z
    ltTransitive :: Property
    ltTransitive = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a, z :: AsHaskell a) ->
      plift
        ( let liftedX = pconstant @a x
              liftedY = pconstant y
              liftedZ = pconstant z
           in (liftedX #>= liftedY) #|| (liftedY #>= liftedZ) #|| (liftedX #< liftedZ)
        )
    ltTrichotomous :: Property
    ltTrichotomous = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a) ->
      plift
        ( let liftedX = pconstant @a x
              liftedY = pconstant y
           in (liftedX #< liftedY) #|| (liftedY #< liftedX) #|| (liftedX #== liftedY)
        )
    ltEquivLeq :: Property
    ltEquivLeq = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a) ->
      plift (pconstant @a x #<= pconstant y)
        === plift
          ( let liftedX = pconstant @a x
                liftedY = pconstant y
             in (liftedX #< liftedY) #|| (liftedX #== liftedY)
          )

{- | Verifies that the specified Plutarch and Haskell types satisfy the laws of
'PLiftable'.

@since WIP
-}
checkPLiftableLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , Eq (AsHaskell a)
  , PLiftable a
  , Show (AsHaskell a)
  ) =>
  [TestTree]
checkPLiftableLaws =
  [ testProperty "fromPlutarch . toPlutarch = Right"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(x :: AsHaskell a) ->
        fromPlutarch @a (toPlutarch @a x) === Right x
  , testProperty "fromPlutarchRepr . toPlutarchRepr = Just"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(x :: AsHaskell a) ->
        fromPlutarchRepr @a (toPlutarchRepr @a x) === Just x
  , testProperty "plift . pconstant = id" . forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a) ->
      plift (pconstant @a x) `prettyEquals` x
  ]

{- | Like `checkLedgerProperties` but specialized to `PValue`

This is an ugly kludge because PValue doesn't have a direct PData conversion,
and bringing one in would break too much other stuff to be worth it.

@since WIP
-}
checkLedgerPropertiesValue :: TestTree
checkLedgerPropertiesValue =
  testGroup "PValue" . mconcat $
    [ pisDataLaws @(V1.PValue V1.Unsorted V1.NoGuarantees) "PValue"
    , ptryFromLawsValue
    , checkPLiftableLaws @(V1.PValue V1.Unsorted V1.NoGuarantees)
    ]

{- | Like `checkLedgerProperties` but specialized to `PMap`

Same as above

@since WIP
-}
checkLedgerPropertiesAssocMap :: TestTree
checkLedgerPropertiesAssocMap =
  testGroup "PMap" . mconcat $
    [ pisDataLaws @(V1.PMap V1.Unsorted PInteger PInteger) "PMap"
    , ptryFromLawsAssocMap
    , checkPLiftableLaws @(V1.PMap V1.Unsorted PInteger PInteger)
    ]

-- | @since WIP
checkLedgerProperties ::
  forall (a :: S -> Type).
  ( Typeable a
  , PLiftable a
  , PTryFrom PData a
  , Eq (AsHaskell a)
  , PIsData a
  , Plutus.ToData (AsHaskell a)
  , Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , Show (AsHaskell a)
  ) =>
  TestTree
checkLedgerProperties =
  testGroup (instanceOfType @(S -> Type) @a "Ledger Laws") . mconcat $
    [ pisDataLaws @a (typeName' False (typeRep @a)) -- it'll get wrapped in PAsData so not top level
    , ptryFromLaws @a
    , checkPLiftableLaws @a
    ]

-- | @since WIP
checkLedgerPropertiesPCountable ::
  forall (a :: S -> Type).
  ( Typeable a
  , PCountable a
  , Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , Eq (AsHaskell a)
  , Show (AsHaskell a)
  , PLiftable a
  ) =>
  TestTree
checkLedgerPropertiesPCountable =
  testGroup (instanceOfType @(S -> Type) @a "PCountable") (pcountableLaws @a)

-- | @since WIP
checkLedgerPropertiesPEnumerable ::
  forall (a :: S -> Type).
  ( Typeable a
  , PEnumerable a
  , Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , Eq (AsHaskell a)
  , PLiftable a
  ) =>
  TestTree
checkLedgerPropertiesPEnumerable =
  testGroup (instanceOfType @(S -> Type) @a "PEnumerable") (penumerableLaws @a)

-- | @since WIP
checkHaskellOrdEquivalent ::
  forall (plutarchInput :: S -> Type).
  ( PLiftable plutarchInput
  , Pretty (AsHaskell plutarchInput)
  , Arbitrary (AsHaskell plutarchInput)
  , Typeable (AsHaskell plutarchInput)
  , Ord (AsHaskell plutarchInput)
  , Typeable plutarchInput
  , POrd plutarchInput
  ) =>
  TestTree
checkHaskellOrdEquivalent =
  testGroup
    ( mconcat
        [ instanceOfType @Type @(AsHaskell plutarchInput) "Ord"
        , " <-> "
        , instanceOfType @(S -> Type) @plutarchInput "POrd"
        ]
    )
    [ testProperty "== = #==" $
        checkHaskellEquivalent2 ((==) @(AsHaskell plutarchInput)) (precompileTerm $ plam ((#==) @plutarchInput))
    , testProperty "< = #<" $
        checkHaskellEquivalent2 ((<) @(AsHaskell plutarchInput)) (precompileTerm $ plam ((#<) @plutarchInput))
    , testProperty "<= = #<=" $
        checkHaskellEquivalent2 ((<=) @(AsHaskell plutarchInput)) (precompileTerm $ plam ((#<=) @plutarchInput))
    ]

-- | @since WIP
checkHaskellIntegralEquivalent ::
  forall (plutarchInput :: S -> Type).
  ( PLiftable plutarchInput
  , Pretty (AsHaskell plutarchInput)
  , Arbitrary (AsHaskell plutarchInput)
  , Typeable (AsHaskell plutarchInput)
  , Integral (AsHaskell plutarchInput)
  , Typeable plutarchInput
  , PIntegral plutarchInput
  ) =>
  TestTree
checkHaskellIntegralEquivalent =
  testGroup
    ( mconcat
        [ instanceOfType @Type @(AsHaskell plutarchInput) "Integral"
        , " <-> "
        , instanceOfType @(S -> Type) @plutarchInput "PIntegral"
        ]
    )
    [ testIntegralEquivalent @plutarchInput "div = pdiv" div pdiv
    , testIntegralEquivalent @plutarchInput "mod = pmod" mod pmod
    , testIntegralEquivalent @plutarchInput "quot = pquot" quot pquot
    , testIntegralEquivalent @plutarchInput "rem = prem" rem prem
    ]

checkHaskellNumEquivalent ::
  forall (plutarchInput :: S -> Type).
  ( PLiftable plutarchInput
  , Pretty (AsHaskell plutarchInput)
  , Arbitrary (AsHaskell plutarchInput)
  , Eq (AsHaskell plutarchInput)
  , Typeable (AsHaskell plutarchInput)
  , Num (AsHaskell plutarchInput)
  , Typeable plutarchInput
  , PNum plutarchInput
  ) =>
  TestTree
checkHaskellNumEquivalent =
  testGroup
    ( mconcat
        [ instanceOfType @Type @(AsHaskell plutarchInput) "Num"
        , " <-> "
        , instanceOfType @(S -> Type) @plutarchInput "PNum"
        ]
    )
    [ testProperty "+ = #+" $ checkHaskellEquivalent2 @plutarchInput (+) (plam (#+))
    , testProperty "- = #-" $ checkHaskellEquivalent2 @plutarchInput (-) (plam (#-))
    , testProperty "* = #*" $ checkHaskellEquivalent2 @plutarchInput (*) (plam (#*))
    , testProperty "negate = pnegate" $ checkHaskellEquivalent @plutarchInput negate pnegate
    , testProperty "abs = pabs" $ checkHaskellEquivalent @plutarchInput abs pabs
    , testProperty "signum = psignum" $ checkHaskellEquivalent @plutarchInput signum psignum
    ]

-- Internal

-- | @since WIP
testIntegralEquivalent ::
  forall (plutarchInput :: S -> Type).
  ( Arbitrary (AsHaskell plutarchInput)
  , Pretty (AsHaskell plutarchInput)
  , Eq (AsHaskell plutarchInput)
  , PLiftable plutarchInput
  , Num (AsHaskell plutarchInput)
  ) =>
  TestName ->
  (AsHaskell plutarchInput -> AsHaskell plutarchInput -> AsHaskell plutarchInput) ->
  ClosedTerm (plutarchInput :--> plutarchInput :--> plutarchInput) ->
  TestTree
testIntegralEquivalent name goHaskell goPlutarch =
  testProperty name $
    forAllShrinkShow arbitrary shrink prettyShow $
      \(input1 :: AsHaskell plutarchInput, input2 :: NonZero (AsHaskell plutarchInput)) ->
        goHaskell input1 (getNonZero input2)
          `prettyEquals` plift (goPlutarch # pconstant input1 # pconstant (getNonZero input2))

pcountableLaws ::
  forall (a :: S -> Type).
  ( PCountable a
  , Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , Eq (AsHaskell a)
  , Show (AsHaskell a)
  , PLiftable a
  ) =>
  [TestTree]
pcountableLaws =
  [ testProperty "x /= psuccessor x" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (psuccessor # pconstant @a x) =/= x
  , testProperty "y < x = psuccessor y <= x" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a, y :: AsHaskell a) ->
        plift (pconstant @a y #< pconstant @a x) === plift ((psuccessor # pconstant @a y) #<= pconstant @a x)
  , testProperty "x < psuccessor y = x <= y" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a, y :: AsHaskell a) ->
        plift (pconstant @a x #< (psuccessor # pconstant @a y)) === plift (pconstant @a x #<= pconstant @a y)
  , testProperty "psuccessorN 1 = psuccessor" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (psuccessorN # 1 # pconstant @a x) === plift (psuccessor # pconstant @a x)
  , testProperty "psuccessorN n . psuccessorN m = psuccessorN (n + m)" . forAllShrinkShow arbitrary shrink show $
      \(x :: AsHaskell a, n :: Positive, m :: Positive) ->
        plift (psuccessorN # pconstant @PPositive n # (psuccessorN # pconstant @PPositive m # pconstant @a x))
          === plift (psuccessorN # (pconstant @PPositive n + pconstant @PPositive m) # pconstant @a x)
  ]

penumerableLaws ::
  forall (a :: S -> Type).
  ( PEnumerable a
  , Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , Eq (AsHaskell a)
  , PLiftable a
  ) =>
  [TestTree]
penumerableLaws =
  [ testProperty "ppredecessor . psuccessor = id" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (ppredecessor #$ psuccessor # pconstant @a x) `prettyEquals` plift (pconstant @a x)
  , testProperty "psuccessor . ppredecessor = id" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (psuccessor #$ ppredecessor # pconstant @a x) `prettyEquals` plift (pconstant @a x)
  , testProperty "ppredecessorN 1 = ppredecessor" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (ppredecessorN # 1 # pconstant @a x) `prettyEquals` plift (ppredecessor # pconstant @a x)
  , testProperty "ppredecessorN n . ppredecessorN m = ppredecessorN (n + m)" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a, n :: Positive, m :: Positive) ->
        plift (ppredecessorN # pconstant n # (ppredecessorN # pconstant m # pconstant @a x))
          `prettyEquals` plift (ppredecessorN # (pconstant n + pconstant m) # pconstant @a x)
  ]

-- pfromData . pdata = id
-- plift . pforgetData . pdata . pconstant = toData
-- plift . pfromData . punsafeCoerce @(PAsData X) . pconstant . toData = id
pisDataLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , PLiftable a
  , PIsData a
  , Eq (AsHaskell a)
  , Plutus.ToData (AsHaskell a)
  , Pretty (AsHaskell a)
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
        $ \(x :: AsHaskell a) ->
          plift (precompileTerm (plam (pfromData . pdata) # pconstant @a x)) `prettyEquals` x
    toDataProp :: TestTree
    toDataProp =
      testProperty "plift . pforgetData . pdata . pconstant = toData"
        . forAllShrinkShow arbitrary shrink prettyShow
        $ \(x :: AsHaskell a) ->
          plift (precompileTerm (plam (pforgetData . pdata)) # pconstant @a x) `prettyEquals` Plutus.toData x
    coerceProp :: TestTree
    coerceProp =
      testProperty coerceName
        . forAllShrinkShow arbitrary shrink prettyShow
        $ \(x :: AsHaskell a) ->
          plift (precompileTerm (plam (pfromData . punsafeCoerce @_ @_ @(PAsData a))) # pconstant @PData (Plutus.toData x)) `prettyEquals` x
    coerceName :: String
    coerceName = "plift . pfromData . punsafeCoerce @(PAsData " <> tyName <> ") . pconstant . toData = id"

-- ptryFrom should successfully parse a toData of a type
ptryFromLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , PLiftable a
  , Eq (AsHaskell a)
  , PTryFrom PData a
  , Plutus.ToData (AsHaskell a)
  , Pretty (AsHaskell a)
  ) =>
  [TestTree]
ptryFromLaws = [pDataAgreementProp]
  where
    pDataAgreementProp :: TestTree
    pDataAgreementProp = testProperty "can parse toData of original"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(x :: AsHaskell a) ->
        plift (precompileTerm (plam $ \d -> ptryFrom @a d fst) # (pconstant @PData . Plutus.toData $ x))
          `prettyEquals` x

-- This is an ugly kludge because PValue doesn't have a direct PData conversion,
-- and bringing one in would break too much other stuff to be worth it.
ptryFromLawsValue :: [TestTree]
ptryFromLawsValue = [pDataAgreementProp]
  where
    pDataAgreementProp :: TestTree
    pDataAgreementProp = testProperty "can parse toData of original"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(v :: PLA.Value) ->
        plift (precompileTerm (plam $ \d -> pfromData . ptryFrom @(PAsData (V1.PValue V1.Unsorted V1.NoGuarantees)) d $ fst) # pconstant @PData (Plutus.toData v))
          `prettyEquals` v

-- Same as before
ptryFromLawsAssocMap :: [TestTree]
ptryFromLawsAssocMap = [pDataAgreementProp]
  where
    pDataAgreementProp :: TestTree
    pDataAgreementProp = testProperty "can parse toData of original"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(v :: AssocMap.Map Integer Integer) ->
        plift (precompileTerm (plam $ \d -> pfromData . ptryFrom @(PAsData (V1.PMap V1.Unsorted PInteger PInteger)) d $ fst) # pconstant @PData (Plutus.toData v))
          `prettyEquals` v

-- Helpers

-- Effectively (,,), but with a 50% chance to generate three copies of the same
-- thing. This ensures transitivity tests aren't vacuously true.
data Triplet (a :: Type)
  = AllSame a
  | AllDifferent a a a
  deriving stock (Eq, Show)

instance Pretty a => Pretty (Triplet a) where
  {-# INLINEABLE pretty #-}
  pretty = pretty . toTriple

instance Arbitrary1 Triplet where
  {-# INLINEABLE liftArbitrary #-}
  liftArbitrary gen = oneof [AllSame <$> gen, AllDifferent <$> gen <*> gen <*> gen]
  {-# INLINEABLE liftShrink #-}
  liftShrink shr = \case
    AllSame x -> AllSame <$> shr x
    AllDifferent x y z ->
      (AllDifferent <$> shr x <*> pure y <*> pure z)
        <|> (AllDifferent x <$> shr y <*> pure z)
        <|> (AllDifferent x y <$> shr z)

instance Arbitrary a => Arbitrary (Triplet a) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = liftArbitrary arbitrary
  {-# INLINEABLE shrink #-}
  shrink = liftShrink shrink

toTriple :: forall (a :: Type). Triplet a -> (a, a, a)
toTriple = \case
  AllSame x -> (x, x, x)
  AllDifferent x y z -> (x, y, z)
