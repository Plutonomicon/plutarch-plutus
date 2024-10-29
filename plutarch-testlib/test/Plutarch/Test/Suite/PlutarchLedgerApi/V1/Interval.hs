module Plutarch.Test.Suite.PlutarchLedgerApi.V1.Interval (tests) where

import Plutarch.LedgerApi.Interval (
  PInterval,
  pafter,
  palways,
  pbefore,
  pcontains,
  pfrom,
  phull,
  pintersection,
  pinterval,
  pmember,
  psingleton,
  pto,
 )
import Plutarch.LedgerApi.V1 (PPosixTime)
import Plutarch.Prelude hiding (psingleton, pto)
import Plutarch.Test.Equivalent (checkHaskellEquivalent)
import Plutarch.Test.Golden (goldenEval, goldenEvalEqual, goldenGroup, plutarchGolden)
import Plutarch.Test.Laws (checkLedgerProperties)
import Plutarch.Test.Utils (fewerTests)
import PlutusLedgerApi.V1 (POSIXTime)
import PlutusLedgerApi.V1.Interval (contains, hull, intersection, member)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (arbitrary, forAllShrinkShow, shrink, testProperty)

tests :: TestTree
tests =
  testGroup
    "Interval"
    [ plutarchGolden
        "Goldens"
        "extra.intervalutils"
        [ goldenGroup
            "constants"
            [ goldenEval "always" (palways @PInteger)
            ]
        , goldenGroup
            "contains"
            [ goldenEvalEqual "in interval" (pcontains # i2 # i4) (pcon PTrue)
            , goldenEvalEqual "out interval" (pcontains # i4 # i2) (pcon PFalse)
            , goldenEvalEqual "always" (pcontains # palways @PInteger # i1) (pcon PTrue)
            ]
        , goldenGroup
            "member"
            [ goldenEvalEqual "[b,c], a < b" (pmember # pconstantData 1 # i3) (pcon PFalse)
            , goldenEvalEqual "[b,c], a = b" (pmember # pconstantData 2 # i3) (pcon PTrue)
            , goldenEvalEqual "[b,c], a > b, a < c" (pmember # pconstantData 3 # i3) (pcon PTrue)
            , goldenEvalEqual "[b,c], a = c" (pmember # pconstantData 4 # i3) (pcon PTrue)
            , goldenEvalEqual "[b,c], a > c" (pmember # pconstantData 5 # i3) (pcon PFalse)
            ]
        , let theHull :: Term s (PInterval PInteger)
              theHull = phull # (psingleton # pconstantData 3) # (psingleton # pconstantData 5)
           in goldenGroup
                "hull"
                [ goldenEvalEqual "hull 3 5 contains 3 5" (pcontains # theHull # i2) (pcon PTrue)
                , goldenEvalEqual "2 not member of hull 3 5" (pmember # pconstantData 2 # theHull) (pcon PFalse)
                , goldenEvalEqual "6 not member of hull 3 5" (pmember # pconstantData 6 # theHull) (pcon PFalse)
                ]
        , goldenGroup
            "intersection"
            [ goldenEvalEqual
                "intersection [2,4] [3,5] contains [3,4]"
                (pcontains # (pintersection # i3 # i2) # i5)
                (pcon PTrue)
            , goldenEvalEqual
                "intersection [3,5] [2,4] contains [3,4]"
                (pcontains # (pintersection # i2 # i3) # i5)
                (pcon PTrue)
            ]
        ]
    , checkLedgerProperties @(PInterval PPosixTime)
    , adjustOption (fewerTests 4) $
        testGroup
          "Properties"
          [ testGroup
              "member"
              [ testProperty "a is a member of [b, c] iff b <= a and a <= c" $
                  forAllShrinkShow arbitrary shrink show checkMember
              ]
          , testGroup
              "always"
              [ testProperty "always contains everything" $
                  forAllShrinkShow arbitrary shrink show checkAlways
              ]
          , testGroup
              "hull"
              [ testProperty "hull of a and b contains a and b" $
                  forAllShrinkShow arbitrary shrink show checkHull
              ]
          , testGroup
              "intersection"
              [ testProperty "intersection of a and b is contained in a and b" $
                  forAllShrinkShow arbitrary shrink show checkIntersection
              ]
          , testGroup
              "contains"
              [ testProperty "contains on bounded intervals" $
                  forAllShrinkShow arbitrary shrink show checkBoundedContains
              , testProperty "contains on unbounded (from above) intervals" $
                  forAllShrinkShow arbitrary shrink show checkUnboundedUpperContains
              , testProperty "contains on unbounded (from below) intervals" $
                  forAllShrinkShow arbitrary shrink show checkUnboundedLowerContains
              ]
          , testGroup
              "before"
              [ testProperty "a is before [b, c] iff a < b" $
                  forAllShrinkShow arbitrary shrink show checkBefore
              ]
          , testGroup
              "after"
              [ testProperty "a is after [b, c] iff c < a" $
                  forAllShrinkShow arbitrary shrink show checkAfter
              ]
          , testGroup
              "Haskell equivalents"
              [ testProperty "contains = pcontains" $
                  checkHaskellEquivalent (contains @POSIXTime) pcontains
              , testProperty "member = pmember" $
                  checkHaskellEquivalent
                    (member @POSIXTime)
                    (plam $ \value interval -> pmember # pdata value # interval)
              , testProperty "intersection = pintersection" $
                  checkHaskellEquivalent (intersection @POSIXTime) pintersection
              , testProperty "hull = phull" $
                  checkHaskellEquivalent (hull @POSIXTime) phull
              ]
          ]
    ]

i1 :: Term s (PInterval PInteger)
i1 = mkInterval 1 2

i2 :: Term s (PInterval PInteger)
i2 = mkInterval 3 5

i3 :: Term s (PInterval PInteger)
i3 = mkInterval 2 4

i4 :: Term s (PInterval PInteger)
i4 = mkInterval 4 4

i5 :: Term s (PInterval PInteger)
i5 = mkInterval 3 4

checkMember :: Integer -> Integer -> Integer -> Bool
checkMember a b c = actual == expected
  where
    i :: Term s (PInterval PInteger)
    i = mkInterval b c

    actual = plift $ pmember # pconstantData a # i
    expected = (min b c <= a) && (a <= max b c)

checkAlways :: Integer -> Integer -> Bool
checkAlways a b = plift $ pcontains # palways # i
  where
    i :: Term s (PInterval PInteger)
    i = mkInterval a b

checkHull :: Integer -> Integer -> Integer -> Integer -> Bool
checkHull a b c d = plift $ (pcontains # i3 # i1) #&& (pcontains # i3 # i2)
  where
    i1 :: Term s (PInterval PInteger)
    i1 = mkInterval a b

    i2 :: Term s (PInterval PInteger)
    i2 = mkInterval c d

    i3 :: Term s (PInterval PInteger)
    i3 = phull # i1 # i2

checkIntersection :: Integer -> Integer -> Integer -> Integer -> Bool
checkIntersection a b c d = plift $ (pcontains # i1 # i3) #&& (pcontains # i2 # i3)
  where
    i1 :: Term s (PInterval PInteger)
    i1 = mkInterval a b

    i2 :: Term s (PInterval PInteger)
    i2 = mkInterval c d

    i3 :: Term s (PInterval PInteger)
    i3 = pintersection # i1 # i2

checkBoundedContains :: Integer -> Integer -> Integer -> Integer -> Bool
checkBoundedContains a b c d = actual == expected
  where
    i1 :: Term s (PInterval PInteger)
    i1 = mkInterval a b
    i2 :: Term s (PInterval PInteger)
    i2 = mkInterval c d

    expected :: Bool
    expected = (min a b <= min c d) && (max c d <= max a b)

    actual' :: ClosedTerm PBool
    actual' = pcontains # i1 # i2
    actual = plift actual'

checkUnboundedUpperContains :: Integer -> Integer -> Integer -> Bool
checkUnboundedUpperContains a b c = actual == expected
  where
    i1 :: Term s (PInterval PInteger)
    i1 = pfrom # pconstantData a
    i2 :: Term s (PInterval PInteger)
    i2 = mkInterval b c

    expected :: Bool
    expected = a <= min b c

    actual' :: ClosedTerm PBool
    actual' = pcontains # i1 # i2
    actual = plift actual'

checkUnboundedLowerContains :: Integer -> Integer -> Integer -> Bool
checkUnboundedLowerContains a b c = actual == expected
  where
    i1 :: Term s (PInterval PInteger)
    i1 = pto # pconstantData a
    i2 :: Term s (PInterval PInteger)
    i2 = mkInterval b c

    expected :: Bool
    expected = a >= max b c

    actual' :: ClosedTerm PBool
    actual' = pcontains # i1 # i2
    actual = plift actual'

checkBefore :: Integer -> Integer -> Integer -> Bool
checkBefore a b c = actual == expected
  where
    i :: Term s (PInterval PInteger)
    i = mkInterval b c

    expected :: Bool
    expected = a < min b c

    actual' :: ClosedTerm PBool
    actual' = pbefore # pconstant a # i
    actual = plift actual'

checkAfter :: Integer -> Integer -> Integer -> Bool
checkAfter a b c = actual == expected
  where
    i :: Term s (PInterval PInteger)
    i = mkInterval b c

    expected :: Bool
    expected = max b c < a

    actual' :: ClosedTerm PBool
    actual' = pafter # pconstant a # i
    actual = plift actual'

mkInterval :: forall s. Integer -> Integer -> Term s (PInterval PInteger)
mkInterval a' b' = pinterval # pconstantData a # pconstantData b
  where
    a = min a' b'
    b = max a' b'
