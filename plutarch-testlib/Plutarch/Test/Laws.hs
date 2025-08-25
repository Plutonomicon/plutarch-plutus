{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Laws (
  AsTagTest (tagNum),
  checkLedgerPropertiesValue,
  checkLedgerPropertiesAssocMap,
  checkLedgerProperties,
  checkLedgerPropertiesPCountable,
  checkLedgerPropertiesPEnumerable,
  checkHaskellOrdEquivalent,
  checkHaskellNumEquivalent,
  checkPLiftableLaws,
  checkPLiftableLawsForDeriveTags,
  checkPOrdLaws,
  checkPAdditiveSemigroupLaws,
  checkPAdditiveMonoidLaws,
  checkPAdditiveGroupLaws,
  checkPSemigroupLaws,
  checkPMonoidLaws,
) where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Plutarch.LedgerApi.V1 qualified as V1
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (checkHaskellEquivalent, checkHaskellEquivalent2)
import Plutarch.Test.Utils (instanceOfType, precompileTerm, prettyEquals, prettyShow, typeName')
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V1.Orphans ()
import Prettyprinter (Pretty (pretty))
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  Property,
  forAllShrinkShow,
  oneof,
  (=/=),
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Type.Reflection (Typeable, typeRep)

{- | Verifies that the specified Plutarch type satisfies the 'PSemigroup' laws
for mandatory methods.

@since 1.0.0
-}
checkPSemigroupLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , PSemigroup a
  , PEq a
  , PLiftable a
  ) =>
  TestTree
checkPSemigroupLaws =
  testGroup
    "PSemigroup"
    [ testProperty "#<> is associative" psemiAssociative
    ]
  where
    psemiAssociative :: Property
    psemiAssociative = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y, z) ->
      plift
        ( precompileTerm (plam $ \arg1 arg2 arg3 -> (arg1 #<> (arg2 #<> arg3)) #== ((arg1 #<> arg2) #<> arg3))
            # pconstant @a x
            # pconstant y
            # pconstant z
        )

{- | Verifies that the specified Plutarch type satisfies the 'PMonoid' laws for
mandatory methods.

@since 1.0.0
-}
checkPMonoidLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , PMonoid a
  , PEq a
  , PLiftable a
  ) =>
  TestTree
checkPMonoidLaws =
  testGroup
    "PMonoid"
    [ testProperty "pmempty is a left identity for #<>" pmemptyLeftId
    , testProperty "pmemoty is a right identity for #<>" pmemptyRightId
    ]
  where
    pmemptyLeftId :: Property
    pmemptyLeftId = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a) ->
      plift (precompileTerm (plam $ \arg1 -> (arg1 #<> pmempty) #== arg1) # pconstant @a x)
    pmemptyRightId :: Property
    pmemptyRightId = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a) ->
      plift (precompileTerm (plam $ \arg1 -> (pmempty #<> arg1) #== arg1) # pconstant @a x)

{- | Verifies that the specified Plutarch type satisfies the
'PAdditiveSemigroup' laws for mandatory methods.

@since 1.0.0
-}
checkPAdditiveSemigroupLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , PAdditiveSemigroup a
  , PEq a
  , PLiftable a
  ) =>
  TestTree
checkPAdditiveSemigroupLaws =
  testGroup
    "PAdditiveSemigroup"
    [ testProperty "#+ is commutative" plusSymmetric
    , testProperty "#+ is associative" plusAssociative
    ]
  where
    plusSymmetric :: Property
    plusSymmetric = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a) ->
      plift (precompileTerm (plam $ \arg1 arg2 -> (arg1 #+ arg2) #== (arg2 #+ arg1)) # pconstant @a x # pconstant @a y)
    plusAssociative :: Property
    plusAssociative = forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a, y, z) ->
        plift
          ( precompileTerm (plam $ \arg1 arg2 arg3 -> ((arg1 #+ arg2) #+ arg3) #== (arg1 #+ (arg2 #+ arg3)))
              # pconstant @a x
              # pconstant y
              # pconstant z
          )

{- | Verifies that the specified Plutarch type satisfies the 'PAdditiveMonoid'
laws for mandatory methods.

@since 1.0.0
-}
checkPAdditiveMonoidLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , PAdditiveMonoid a
  , PEq a
  , PLiftable a
  ) =>
  TestTree
checkPAdditiveMonoidLaws =
  testGroup
    "PAdditiveMonoid"
    [ testProperty "pzero is the identity for #+" pzeroIdentity
    , testProperty "pzero does not scale" pzeroScale
    ]
  where
    pzeroIdentity :: Property
    pzeroIdentity = forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) -> plift (precompileTerm (plam $ \arg1 -> (arg1 #+ pzero) #== arg1) # pconstant @a x)
    pzeroScale :: Property
    pzeroScale = forAllShrinkShow arbitrary shrink prettyShow $
      \(p :: Positive) -> plift (precompileTerm (plam $ \arg1 -> pscalePositive (pzero @a) arg1 #== pzero) # pconstant @PPositive p)

{- | Verifies that the specified Plutarch type satisfies the 'PAdditiveGroup'
laws for mandatory methods.

@since 1.0.0
-}
checkPAdditiveGroupLaws ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , PAdditiveGroup a
  , PEq a
  , PLiftable a
  ) =>
  TestTree
checkPAdditiveGroupLaws =
  testGroup
    "PAdditiveGroup"
    [ testProperty "pnegate is an additive inverse" pnegateAdditiveInverse
    , testProperty "pnegate is self-inverting" pnegateSelfInverting
    , testProperty "x #- x = pzero" pminusSelf
    , testProperty "pnegate is difference from pzero" pnegatePZeroConsistency1
    , testProperty "x #- y = x #+ pnegate y" pnegatePZeroConsistency2
    ]
  where
    pnegateAdditiveInverse :: Property
    pnegateAdditiveInverse = forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (precompileTerm (plam $ \arg1 -> ((pnegate # arg1) #+ arg1) #== pzero) # pconstant @a x)
    pnegateSelfInverting :: Property
    pnegateSelfInverting = forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (precompileTerm (plam $ \arg1 -> (pnegate #$ pnegate # arg1) #== arg1) # pconstant @a x)
    pminusSelf :: Property
    pminusSelf = forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (precompileTerm (plam $ \arg1 -> (arg1 #- arg1) #== pzero) # pconstant @a x)
    pnegatePZeroConsistency1 :: Property
    pnegatePZeroConsistency1 = forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a) ->
        plift (precompileTerm (plam $ \arg1 -> (pnegate # arg1) #== (pzero #- arg1)) # pconstant @a x)
    pnegatePZeroConsistency2 :: Property
    pnegatePZeroConsistency2 = forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a, y) ->
        plift
          ( precompileTerm (plam $ \arg1 arg2 -> (arg1 #- arg2) #== (arg1 #+ (pnegate # arg2)))
              # pconstant @a x
              # pconstant y
          )

{- | Verifies that the specified Plutarch type satisfies the 'POrd' laws for
mandatory methods.

@since 1.0.0
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
      plift (precompileTerm (plam $ \arg1 -> arg1 #<= arg1) # pconstant @a x)
    -- We have to restate (x <= y && y <= z) -> x <= z, which gives (after some
    -- DeMorganing) x > y || y > z || x <= z
    leqTransitive :: Property
    leqTransitive = forAllShrinkShow arbitrary shrink prettyShow $ \(t :: Triplet (AsHaskell a)) ->
      let (x, y, z) = toTriple t
       in plift
            ( precompileTerm (plam $ \arg1 arg2 arg3 -> (arg1 #> arg2) #|| (arg2 #> arg3) #|| (arg1 #<= arg3))
                # pconstant @a x
                # pconstant y
                # pconstant z
            )
    leqTotal :: Property
    leqTotal = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a) ->
      plift
        ( precompileTerm (plam $ \arg1 arg2 -> (arg1 #<= arg2) #|| (arg2 #<= arg1))
            # pconstant @a x
            # pconstant y
        )
    ltIrreflexive :: Property
    ltIrreflexive = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a) ->
      plift (precompileTerm (plam $ \arg1 -> pnot #$ arg1 #< arg1) # pconstant @a x)
    -- We have to restate (x < y && y < z) -> x < z, which gives (after some
    -- DeMorganing) x >= y || y >= z || x < z
    ltTransitive :: Property
    ltTransitive = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a, z :: AsHaskell a) ->
      plift
        ( precompileTerm (plam $ \arg1 arg2 arg3 -> (arg1 #>= arg2) #|| (arg2 #>= arg3) #|| (arg1 #< arg3))
            # pconstant @a x
            # pconstant y
            # pconstant z
        )
    ltTrichotomous :: Property
    ltTrichotomous = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a) ->
      plift
        ( precompileTerm (plam $ \arg1 arg2 -> (arg1 #< arg2) #|| (arg2 #< arg1) #|| (arg1 #== arg2))
            # pconstant @a x
            # pconstant y
        )
    ltEquivLeq :: Property
    ltEquivLeq = forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a, y :: AsHaskell a) ->
      plift (precompileTerm (plam $ \arg1 arg2 -> arg1 #<= arg2) # pconstant @a x # pconstant y)
        === plift
          ( precompileTerm (plam $ \arg1 arg2 -> (arg1 #< arg2) #|| (arg1 #== arg2))
              # pconstant @a x
              # pconstant y
          )

{- | Verifies that the specified Plutarch and Haskell types satisfy the laws of
'PLiftable'.

@since 1.0.0
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
  [ testProperty "plutToRepr . reprToPlut = Right"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(x :: AsHaskell a) ->
        (reprToHask @a <$> plutToRepr @a (reprToPlut (haskToRepr @a x))) === (reprToHask @a <$> Right (haskToRepr @a x))
  , testProperty "reprToHask . haskToRepr = Right"
      . forAllShrinkShow arbitrary shrink prettyShow
      $ \(x :: AsHaskell a) ->
        reprToHask @a (haskToRepr @a x) === Right x
  , testProperty "plift . pconstant = id" . forAllShrinkShow arbitrary shrink prettyShow $ \(x :: AsHaskell a) ->
      plift (pconstant @a x) `prettyEquals` x
  ]

class AsTagTest a where
  tagNum :: a -> Integer

-- | @since 3.5.0
checkPLiftableLawsForDeriveTags ::
  forall (a :: S -> Type).
  ( Arbitrary (AsHaskell a)
  , Pretty (AsHaskell a)
  , PLiftable a
  , PlutusRepr a ~ Integer
  , AsTagTest (AsHaskell a)
  ) =>
  TestTree
checkPLiftableLawsForDeriveTags =
  testProperty "verify tag id" . forAllShrinkShow arbitrary shrink prettyShow $
    ( \(x :: AsHaskell a) ->
        tagNum x === haskToRepr @a x
    )

{- | Like `checkLedgerProperties` but specialized to `PValue`

This is an ugly kludge because PValue doesn't have a direct PData conversion,
and bringing one in would break too much other stuff to be worth it.

@since 1.0.0
-}
checkLedgerPropertiesValue :: TestTree
checkLedgerPropertiesValue =
  testGroup "PValue" . mconcat $
    [ pisDataLaws @(V1.PValue V1.Unsorted V1.NoGuarantees) "PValue"
    , checkPLiftableLaws @(V1.PValue V1.Unsorted V1.NoGuarantees)
    ]

{- | Like `checkLedgerProperties` but specialized to `PMap`

Same as above

@since 1.0.0
-}
checkLedgerPropertiesAssocMap :: TestTree
checkLedgerPropertiesAssocMap =
  testGroup "PMap" . mconcat $
    [ pisDataLaws @(V1.PMap V1.Unsorted PInteger PInteger) "PMap"
    , checkPLiftableLaws @(V1.PMap V1.Unsorted PInteger PInteger)
    ]

-- | @since 1.0.0
checkLedgerProperties ::
  forall (a :: S -> Type).
  ( Typeable a
  , PLiftable a
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
    , checkPLiftableLaws @a
    ]

-- | @since 1.0.0
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

-- | @since 1.0.0
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

-- | @since 1.0.0
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

checkHaskellNumEquivalent ::
  forall (plutarchInput :: S -> Type).
  ( PLiftable plutarchInput
  , Pretty (AsHaskell plutarchInput)
  , Arbitrary (AsHaskell plutarchInput)
  , Eq (AsHaskell plutarchInput)
  , Typeable (AsHaskell plutarchInput)
  , Num (AsHaskell plutarchInput)
  , Typeable plutarchInput
  , PIntegralDomain plutarchInput
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
        plift (psuccessorN # pone # pconstant @a x) === plift (psuccessor # pconstant @a x)
  , testProperty "psuccessorN n . psuccessorN m = psuccessorN (n + m)" . forAllShrinkShow arbitrary shrink show $
      \(x :: AsHaskell a, n :: Positive, m :: Positive) ->
        plift (psuccessorN # pconstant @PPositive n # (psuccessorN # pconstant @PPositive m # pconstant @a x))
          === plift (psuccessorN # (pconstant @PPositive n #+ pconstant @PPositive m) # pconstant @a x)
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
        plift (ppredecessorN # pone # pconstant @a x) `prettyEquals` plift (ppredecessor # pconstant @a x)
  , testProperty "ppredecessorN n . ppredecessorN m = ppredecessorN (n + m)" . forAllShrinkShow arbitrary shrink prettyShow $
      \(x :: AsHaskell a, n :: Positive, m :: Positive) ->
        plift (ppredecessorN # pconstant n # (ppredecessorN # pconstant m # pconstant @a x))
          `prettyEquals` plift (ppredecessorN # (pconstant n #+ pconstant m) # pconstant @a x)
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
          plift (precompileTerm (plam (pfromData . punsafeCoerce @(PAsData a))) # pconstant @PData (Plutus.toData x)) `prettyEquals` x
    coerceName :: String
    coerceName = "plift . pfromData . punsafeCoerce @(PAsData " <> tyName <> ") . pconstant . toData = id"

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
