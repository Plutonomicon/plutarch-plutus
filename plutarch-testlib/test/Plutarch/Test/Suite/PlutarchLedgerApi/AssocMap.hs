module Plutarch.Test.Suite.PlutarchLedgerApi.AssocMap (
  assocMapBenches,
  tests,
) where

import Data.Bifunctor (bimap)
import Data.Ix (range)
import Data.Kind (Type)
import Plutarch.LedgerApi.AssocMap (
  BothPresentHandler (..),
  KeyGuarantees (Sorted, Unsorted),
  MergeHandler (..),
  OnePresentHandler (..),
  PMap,
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (pmaybeToMaybeData)
import Plutarch.Maybe (pjust, pmapMaybe, pnothing)
import Plutarch.Prelude
import Plutarch.Test.Bench (bcompare, bench)
import Plutarch.Test.Laws (checkLedgerPropertiesAssocMap)
import Plutarch.Test.QuickCheck (checkHaskellEquivalent2, propEval, propEvalEqual)
import Plutarch.Test.Unit (testEvalEqual)
import Plutarch.Test.Utils (fewerTests, prettyEquals, prettyShow)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Orphans (UnsortedAssocMap, getUnsortedAssocMap)
import PlutusTx.AssocMap qualified as PlutusMap
import Prettyprinter (Pretty)
import Test.QuickCheck (Arbitrary, arbitrary, shrink)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (Property, forAllShrinkShow, testProperty)

assocMapBenches :: [TestTree]
assocMapBenches =
  [ testGroup
      "union"
      [ bench
          "punionResolvingCollisionsWith (optimized)"
          ( AssocMap.punionResolvingCollisionsWith
              # plam (#+)
              # assocMapFixture0
              # assocMapFixture1
          )
      , bcompare "$(NF-1) == \"union\" && $NF == \"punionResolvingCollisionsWith (optimized)\"" $
          bench
            "non-optimized"
            ( let
                mergeHandler =
                  MergeHandler
                    { mhBothPresent = HandleOrDropBoth $ plam (\_ x y -> pjust #$ x #+ y)
                    , mhLeftPresent = HandleOrDropOne $ plam (\_ x -> pjust # x)
                    , mhRightPresent = HandleOrDropOne $ plam (\_ y -> pjust # y)
                    }
               in
                AssocMap.zipWithBuilder mergeHandler
                  # assocMapFixture0
                  # assocMapFixture1
            )
      ]
  , testGroup
      "intersection"
      [ bench
          "pintersectionWith (optimized)"
          ( AssocMap.pintersectionWith
              # plam (#+)
              # assocMapFixture0
              # assocMapFixture1
          )
      , bcompare "$(NF-1) == \"intersection\" && $NF == \"pintersectionWith (optimized)\"" $
          bench
            "non-optimized"
            ( let
                mergeHandler =
                  MergeHandler
                    { mhBothPresent = HandleOrDropBoth $ plam (\_ x y -> pjust #$ x #+ y)
                    , mhLeftPresent = HandleOrDropOne $ plam (\_ _ -> pnothing)
                    , mhRightPresent = HandleOrDropOne $ plam (\_ _ -> pnothing)
                    }
               in
                AssocMap.zipWithBuilder mergeHandler
                  # assocMapFixture0
                  # assocMapFixture1
            )
      ]
  , testGroup
      "difference"
      [ bench
          "pdifference (optimized)"
          (AssocMap.pdifference # assocMapFixture0 # assocMapFixture1)
      , bcompare "$(NF-1) == \"difference\" && $NF == \"pdifference (optimized)\"" $
          bench
            "non-optimized"
            ( let
                mergeHandler =
                  MergeHandler
                    { mhBothPresent = HandleOrDropBoth $ plam (\_ _ _ -> pnothing)
                    , mhLeftPresent = HandleOrDropOne $ plam (\_ x -> pjust # x)
                    , mhRightPresent = HandleOrDropOne $ plam (\_ _ -> pnothing)
                    }
               in
                AssocMap.zipWithBuilder mergeHandler
                  # assocMapFixture0
                  # assocMapFixture1
            )
      ]
  ]

mkAssocMapFixture :: forall (s :: S). [Integer] -> Term s (PMap 'Sorted PInteger PInteger)
mkAssocMapFixture =
  AssocMap.psortedMapFromFoldable @PInteger @PInteger @[]
    . fmap (\x -> (pconstant x, pconstant x))

assocMapFixture0 :: forall (s :: S). Term s (PMap 'Sorted PInteger PInteger)
assocMapFixture0 = mkAssocMapFixture $ range (0, 99)

assocMapFixture1 :: forall (s :: S). Term s (PMap 'Sorted PInteger PInteger)
assocMapFixture1 = mkAssocMapFixture $ range (20, 44) <> range (60, 84) <> range (100, 200)

tests :: TestTree
tests =
  testGroup
    "AssocMap"
    [ checkLedgerPropertiesAssocMap
    , testGroup
        "pcheckBinRel"
        [ testEvalEqual
            "pcheckBinRel (#<=) 0 [(0, 1)] [(0, 1), (1, 1)] = True"
            ( AssocMap.pcheckBinRel
                # plam ((#<=) @PInteger)
                # pconstant @PInteger 0
                # AssocMap.psortedMapFromFoldable @PInteger @PInteger @[] [(pconstant 0, pconstant 1)]
                # AssocMap.psortedMapFromFoldable @PInteger @PInteger @[] [(pconstant 0, pconstant 1), (pconstant 1, pconstant 1)]
            )
            (pconstant @PBool True)
        , testEvalEqual
            "pcheckBinRel (#<) 0 [(0, 1)] [(0, 1), (1, 1)] = False"
            ( AssocMap.pcheckBinRel
                # plam ((#<) @PInteger)
                # pconstant @PInteger 0
                # AssocMap.psortedMapFromFoldable @PInteger @PInteger @[] [(pconstant 0, pconstant 1)]
                # AssocMap.psortedMapFromFoldable @PInteger @PInteger @[] [(pconstant 0, pconstant 1), (pconstant 1, pconstant 1)]
            )
            (pconstant @PBool False)
        , testEvalEqual
            "pcheckBinRel (#<) 0 [(0, 1)] [(0, 2), (1, 1)] = True"
            ( AssocMap.pcheckBinRel
                # plam ((#<) @PInteger)
                # pconstant @PInteger 0
                # AssocMap.psortedMapFromFoldable @PInteger @PInteger @[] [(pconstant 0, pconstant 1)]
                # AssocMap.psortedMapFromFoldable @PInteger @PInteger @[] [(pconstant 0, pconstant 2), (pconstant 1, pconstant 1)]
            )
            (pconstant @PBool True)
        ]
    , propEval "Ledger AssocMap is sorted (sanity check for punsafeCoerce below)" $
        \(m :: PlutusMap.Map Integer Integer) -> AssocMap.passertSorted # pconstant @(PMap 'Unsorted PInteger PInteger) m
    , adjustOption (fewerTests 4) $
        propEval "passertSorted . psortedMapFromFoldable" $
          \(m :: UnsortedAssocMap Integer Integer) ->
            AssocMap.passertSorted @PInteger @PInteger
              #$ AssocMap.psortedMapFromFoldable
                (map (bimap pconstant pconstant) $ PlutusMap.toList $ getUnsortedAssocMap m)
    , testProperty "null = pnull" $ checkHaskellUnsortedPMapEquivalent PlutusMap.null AssocMap.pnull
    , testProperty "lookup = plookup" $
        checkHaskellUnsortedPMapEquivalent2
          PlutusMap.lookup
          (plam $ \k m -> pmaybeToMaybeData #$ AssocMap.plookup # k # m)
    , testProperty "lookup = plookupData" $
        checkHaskellUnsortedPMapEquivalent2
          PlutusMap.lookup
          ( plam $ \k m ->
              pmaybeToMaybeData
                #$ (pmapMaybe # plam pfromData)
                #$ AssocMap.plookupData
                # pdata k
                # m
          )
    , testProperty "singleton = psingleton" $
        checkHaskellEquivalent2 @PInteger @PInteger @(PMap 'Unsorted PInteger PInteger)
          PlutusMap.singleton
          (plam $ \k v -> AssocMap.pforgetSorted $ AssocMap.psingleton # k # v)
    , propEvalEqual @(Integer, Integer)
        "plookup k (psingleton k v) = PJust v"
        (\(k, v) -> AssocMap.plookup # pconstant @PInteger k #$ AssocMap.psingleton # pconstant k # pconstant @PInteger v)
        (\(_, v) -> pjust # pconstant v)
    , testProperty "foldl . toList = pfoldlWithKey" $
        forAllShrinkShow arbitrary shrink show $
          \(a :: Integer, m :: PlutusMap.Map Integer Integer) ->
            foldl (\acc (k, v) -> acc + k + v) a (PlutusMap.toList m)
              `prettyEquals` plift
                ( AssocMap.pfoldlWithKey
                    # plam (\acc k v -> acc + k + v)
                    # pconstant a
                    # (AssocMap.passertSorted # pconstant @(PMap 'Unsorted PInteger PInteger) m)
                )
    , testProperty "all = pall" $
        checkHaskellUnsortedPMapEquivalent (PlutusMap.all even) (AssocMap.pall # peven)
    , testProperty "any = pany" $
        checkHaskellUnsortedPMapEquivalent (any even . PlutusMap.elems) (AssocMap.pany # peven)
    , testProperty "insert = pinsert" $
        checkHaskellSortedPMapEquivalent2 @(PBuiltinPair PInteger PInteger) @(PMap 'Unsorted PInteger PInteger)
          (\(k, v) m -> PlutusMap.insert k v m)
          ( plam
              ( \kv m ->
                  AssocMap.pforgetSorted $
                    AssocMap.pinsert # (pfstBuiltin # kv) # (psndBuiltin # kv) # m
              )
          )
    , testProperty "delete = pdelete" $
        checkHaskellSortedPMapEquivalent2 @PInteger @(PMap 'Unsorted PInteger PInteger)
          PlutusMap.delete
          (plam (\k m -> AssocMap.pforgetSorted $ AssocMap.pdelete # k # m))
    , testProperty "unionWith (+) = punionResolvingCollisionsWith (#+)" $
        forAllShrinkShow arbitrary shrink show $
          \(m1 :: PlutusMap.Map Integer Integer, m2 :: PlutusMap.Map Integer Integer) ->
            PlutusMap.unionWith (+) m1 m2
              `prettyEquals` plift @(PMap 'Unsorted PInteger PInteger)
                ( AssocMap.pforgetSorted $
                    AssocMap.punionResolvingCollisionsWith
                      # plam (#+)
                      # punsafeCoerce (pconstant @(PMap 'Unsorted PInteger PInteger) m1)
                      # punsafeCoerce (pconstant @(PMap 'Unsorted PInteger PInteger) m2)
                )
    , testProperty "unionWith (-) = punionResolvingCollisionsWith (#-)" $
        forAllShrinkShow arbitrary shrink show $
          \(m1 :: PlutusMap.Map Integer Integer, m2 :: PlutusMap.Map Integer Integer) ->
            PlutusMap.unionWith (-) m1 m2
              `prettyEquals` plift @(PMap 'Unsorted PInteger PInteger)
                ( AssocMap.pforgetSorted $
                    AssocMap.punionResolvingCollisionsWith
                      # plam (#-)
                      # punsafeCoerce (pconstant @(PMap 'Unsorted PInteger PInteger) m1)
                      # punsafeCoerce (pconstant @(PMap 'Unsorted PInteger PInteger) m2)
                )
    , testProperty "mapMaybe mkEven = pmapMaybe pmkEven" $
        forAllShrinkShow arbitrary shrink show $
          \(m :: PlutusMap.Map Integer Integer) ->
            PlutusMap.mapMaybe mkEven m
              `prettyEquals` plift @(PMap 'Unsorted PInteger PInteger)
                (AssocMap.pforgetSorted $ AssocMap.pmapMaybe # pmkEven # punsafeCoerce (pconstant @(PMap 'Unsorted PInteger PInteger) m))
    ]

checkHaskellUnsortedPMapEquivalent ::
  forall (plutarchOutput :: S -> Type).
  ( PLiftable plutarchOutput
  , Pretty (AsHaskell plutarchOutput)
  , Eq (AsHaskell plutarchOutput)
  ) =>
  (PlutusMap.Map Integer Integer -> AsHaskell plutarchOutput) ->
  (forall (s :: S). Term s (PMap 'Unsorted PInteger PInteger :--> plutarchOutput)) ->
  Property
checkHaskellUnsortedPMapEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input :: UnsortedAssocMap Integer Integer) -> goHaskell (getUnsortedAssocMap input) `prettyEquals` plift (goPlutarch # pconstant (getUnsortedAssocMap input))

checkHaskellUnsortedPMapEquivalent2 ::
  forall (plutarchInput :: S -> Type) (plutarchOutput :: S -> Type).
  ( PLiftable plutarchInput
  , Pretty (AsHaskell plutarchInput)
  , Arbitrary (AsHaskell plutarchInput)
  , PLiftable plutarchOutput
  , Pretty (AsHaskell plutarchOutput)
  , Eq (AsHaskell plutarchOutput)
  ) =>
  (AsHaskell plutarchInput -> PlutusMap.Map Integer Integer -> AsHaskell plutarchOutput) ->
  (forall (s :: S). Term s (plutarchInput :--> PMap 'Unsorted PInteger PInteger :--> plutarchOutput)) ->
  Property
checkHaskellUnsortedPMapEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: AsHaskell plutarchInput, input2 :: UnsortedAssocMap Integer Integer) ->
      goHaskell input1 (getUnsortedAssocMap input2)
        `prettyEquals` plift (goPlutarch # pconstant input1 # pconstant (getUnsortedAssocMap input2))

checkHaskellSortedPMapEquivalent2 ::
  forall (plutarchInput :: S -> Type) (plutarchOutput :: S -> Type).
  ( PLiftable plutarchInput
  , Pretty (AsHaskell plutarchInput)
  , Arbitrary (AsHaskell plutarchInput)
  , PLiftable plutarchOutput
  , Pretty (AsHaskell plutarchOutput)
  , Eq (AsHaskell plutarchOutput)
  ) =>
  (AsHaskell plutarchInput -> PlutusMap.Map Integer Integer -> AsHaskell plutarchOutput) ->
  (forall (s :: S). Term s (plutarchInput :--> PMap 'Sorted PInteger PInteger :--> plutarchOutput)) ->
  Property
checkHaskellSortedPMapEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: AsHaskell plutarchInput, input2 :: PlutusMap.Map Integer Integer) ->
      goHaskell input1 input2
        `prettyEquals` plift (goPlutarch # pconstant input1 # punsafeCoerce (pconstant @(PMap 'Unsorted PInteger PInteger) input2))

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0

mkEven :: Integer -> Maybe Integer
mkEven n
  | even n = Just n
  | otherwise = Nothing

pmkEven :: forall (s :: S). Term s (PInteger :--> PMaybe PInteger)
pmkEven = plam $ \n -> pif (peven # n) (pjust # n) pnothing
