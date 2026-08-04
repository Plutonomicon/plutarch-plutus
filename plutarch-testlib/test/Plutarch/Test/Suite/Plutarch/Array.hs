{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

-- Needed because ImpredicativeTypes is too stupid to figure out
-- closed-Term-producing functions
{-# HLINT ignore "Avoid lambda" #-}

module Plutarch.Test.Suite.Plutarch.Array (tests) where

import Plutarch.Array (
  PPullArray,
  pdropArray,
  pfoldArray,
  pfromArray,
  pfromList,
  pgenerate,
  pimapArray,
  piota,
  pizipWithArray,
  pmapArray,
  ppullArrayToList,
  ppullArrayToSOPList,
  prfoldArray,
  ptakeArray,
  pzipWithArray,
 )
import Plutarch.Evaluate (evalTerm')
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.Prelude
import Plutarch.Test.Methods (ppowPositiveBetter, pscalePositiveBetter)
import Plutarch.Test.QuickCheck (propEvalEqual)
import Plutarch.Unsafe (punsafeCoerce)
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import "plutarch-testlib" Plutarch.Test.Golden (goldenEval, plutarchGolden)

tests :: TestTree
tests =
  testGroup
    "PPullArray"
    [ plutarchGolden
        "Goldens"
        "array"
        [ goldenEval "piota" sample
        , goldenEval "pgenerate" sample2
        , goldenEval "pfromArray" (pfromArray . pconstant @(PArray PInteger) $ [1, 2 .. 10])
        , goldenEval "pfromList" (pfromList . pconstant @(PBuiltinList PInteger) $ [1, 2 .. 10])
        , goldenEval "pmapArray" (pmapArray plusTen sample)
        , goldenEval "pimapArray" (pimapArray (plam pscaleNatural) sample)
        , goldenEval "ptakeArray" (ptakeArray (unsafeNat 4) sample)
        , goldenEval "pdropArray" (pdropArray (unsafeNat 4) sample)
        , goldenEval "pzipWithArray (different)" (pzipWithArray (plam $ flip pscaleNatural) sample sample2)
        , goldenEval "pzipWithArray (same, no plet)" (pzipWithArray (plam (#*)) sample sample)
        , goldenEval "pzipWithArray (same, plet)" (plet sample $ \sample' -> pzipWithArray (plam (#*)) sample' sample')
        , goldenEval "pizipWithArray" (pizipWithArray (plam $ \i x y -> pscaleNatural (y #+ i) x) sample sample2)
        , goldenEval "pfoldArray" (pfoldArray (plam $ \acc x -> acc #- x) 0 sample2)
        , goldenEval "prfoldArray" (prfoldArray (plam $ \acc x -> acc #- x) 0 sample2)
        , goldenEval "ppullArrayToList" (ppullArrayToList sample)
        , goldenEval "ppullArrayToSOPList" (ppullArrayToSOPList sample)
        ]
    , plutarchGolden
        "Goldens"
        "array-fusion"
        [ goldenEval "pmap then pimap" (pimapArray (plam pscaleNatural) . pmapArray plusTen $ sample)
        , goldenEval "pmap then ptake" (ptakeArray (unsafeNat 4) . pmapArray plusTen $ sample)
        , goldenEval "pmap then pzipWith" (pzipWithArray (plam $ flip pscaleNatural) (pmapArray plusTen sample) (pmapArray (plam (+ 10)) sample2))
        , goldenEval "pmap then pfold" (pfoldArray (plam $ \acc x -> acc #- x) 0 . pmapArray (plam (+ 10)) $ sample2)
        ]
    , testGroup
        "Properties"
        [ propEvalEqual
            "fold direction doesn't matter with commutative monoid"
            (\arr -> pfoldArray (plam (#+)) 0 $ pfromArray $ pconstant @(PArray PInteger) arr)
            (\arr -> prfoldArray (plam (#+)) 0 $ pfromArray $ pconstant @(PArray PInteger) arr)
        , propEvalEqual
            "map f . map g = map (f . g)"
            (\arr -> ppullArrayToList $ pmapArray (plam $ \x -> (x + 1) * 2) $ pfromArray $ pconstant @(PArray PInteger) arr)
            (\arr -> ppullArrayToList $ pmapArray (plam (* 2)) $ pmapArray (plam (+ 1)) $ pfromArray $ pconstant @(PArray PInteger) arr)
        , propEvalEqual
            "imap f (generate n g) = zipWith f (generate n id) (generate n g)"
            (\n -> ppullArrayToList $ pimapArray (plam (#-)) $ pgenerate (pconstant n) (plam (* 2)))
            (\n -> ppullArrayToList $ pzipWithArray (plam (#-)) (pgenerate (pconstant n) (plam id)) (pgenerate (pconstant n) (plam (* 2))))
        , propEvalEqual
            "take n . take m = take (min n m)"
            (\(arr, n, m) -> ppullArrayToList $ ptakeArray (pmin (pconstant n) (pconstant m)) $ pfromArray $ pconstant @(PArray PInteger) arr)
            (\(arr, n, m) -> ppullArrayToList $ ptakeArray (pconstant n) $ ptakeArray (pconstant m) $ pfromArray $ pconstant @(PArray PInteger) arr)
        , propEvalEqual
            "drop n . drop m = drop (n + m)"
            (\(arr, n, m) -> ppullArrayToList $ pdropArray (pconstant n #+ pconstant m) $ pfromArray $ pconstant @(PArray PInteger) arr)
            (\(arr, n, m) -> ppullArrayToList $ pdropArray (pconstant n) $ pdropArray (pconstant m) $ pfromArray $ pconstant @(PArray PInteger) arr)
        , propEvalEqual
            "zipWith f (generate n g) (generate m h) = generate (min n m) (\\i -> f (g i) (h i))"
            (\(n, m) -> ppullArrayToList $ pzipWithArray (plam (#-)) (pgenerate (pconstant n) (plam (+ 1))) $ pgenerate (pconstant m) (plam (* 2)))
            (\(n, m) -> ppullArrayToList $ pgenerate (pmin (pconstant n) (pconstant m)) (plam $ \i -> (i + 1) #- (i * 2)))
        ]
    , testGroup
        "Methods"
        [ pscalePositiveBetter (evalTerm' NoTracing sample2) (punsafeCoerce @_ @PInteger 10)
        , ppowPositiveBetter (evalTerm' NoTracing sample2) (punsafeCoerce @_ @PInteger 10)
        ]
    ]

-- Helpers

len :: forall (s :: S). Term s PNatural
len = unsafeNat 10

unsafeNat :: forall (s :: S). Term s PInteger -> Term s PNatural
unsafeNat = punsafeCoerce @PNatural @PInteger

sample :: forall (s :: S). Term s (PPullArray PNatural)
sample = piota len

sample2 :: forall (s :: S). Term s (PPullArray PInteger)
sample2 = pgenerate len (plam $ \x -> x #* 2)

plusTen :: forall (s :: S). Term s (PNatural :--> PNatural)
plusTen = plam $ \x -> x #+ len
