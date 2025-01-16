module Plutarch.Bench.Unroll (unrollBenches) where

import Plutarch.Prelude

import Plutarch.Test.Bench (bench)
import Test.Tasty (TestTree, testGroup)

regularLength :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
regularLength = pfix # plam go #$ 0
  where
    go :: Term s (PInteger :--> list a :--> PInteger) -> Term s (PInteger :--> list a :--> PInteger)
    go self = plam $ \n -> pelimList (\_ xs -> self # (n + 1) # xs) n

unrollLengthBoundNoPreCompute :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthBoundNoPreCompute = punrollBound 20 (const $ plam $ \_ _ -> -1) go () # 0
  where
    go ::
      (() -> Term s (PInteger :--> list a :--> PInteger)) ->
      () ->
      Term s (PInteger :--> list a :--> PInteger)
    go self () = plam $ \n -> pelimList (\_ xs -> self () # (n + 1) # xs) n

unrollLengthBound :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthBound = punrollBound 20 (const $ plam $ const -1) go 0
  where
    go ::
      (Integer -> Term s (list a :--> PInteger)) ->
      Integer ->
      Term s (list a :--> PInteger)
    go self n = plam $ pelimList (\_ xs -> self (n + 1) # xs) (pconstant n)

unrollLengthUnbound :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthUnbound = punrollUnbound 20 go # 0
  where
    go :: Term s (PInteger :--> list a :--> PInteger) -> Term s (PInteger :--> list a :--> PInteger)
    go self = plam $ \n -> pelimList (\_ xs -> self # (n + 1) # xs) n

unrollLengthUnboundWhole :: forall list a s. PIsListLike list a => Term s (list a :--> PInteger)
unrollLengthUnboundWhole = punrollUnboundWhole 20 go #$ 0 -- pfix # plam go #$ 0
  where
    go :: Term s (PInteger :--> list a :--> PInteger) -> Term s (PInteger :--> list a :--> PInteger)
    go self = plam $ \n -> pelimList (\_ xs -> self # (n + 1) # xs) n

unrollBenches :: [TestTree]
unrollBenches =
  [ testGroup
      "Unrolled 20 times, running 5 recursions"
      [ bench "length, punrollUnbound" (unrollLengthUnbound # pconstant @(PBuiltinList PInteger) [1 .. 5])
      , bench "length, punrollUnboundWhole" (unrollLengthUnboundWhole # pconstant @(PBuiltinList PInteger) [1 .. 5])
      , bench "length, punrollBound" (unrollLengthBound # pconstant @(PBuiltinList PInteger) [1 .. 5])
      , bench "length, no unroll" (regularLength # pconstant @(PBuiltinList PInteger) [1 .. 5])
      ]
  , testGroup
      "Unrolled 20 times, running 40 recursions"
      [ bench "length, punrollUnbound" (unrollLengthUnbound # pconstant @(PBuiltinList PInteger) [1 .. 40])
      , bench "length, punrollUnboundWhole" (unrollLengthUnboundWhole # pconstant @(PBuiltinList PInteger) [1 .. 40])
      , bench "length, punrollBound" (unrollLengthBound # pconstant @(PBuiltinList PInteger) [1 .. 40])
      , bench "length, no unroll" (regularLength # pconstant @(PBuiltinList PInteger) [1 .. 40])
      ]
  , testGroup
      "punrollBound pre-computation"
      [ bench "precompute constant values" (unrollLengthBound # pconstant @(PBuiltinList PInteger) [1 .. 40])
      , bench "no precompute constant values" (unrollLengthBoundNoPreCompute # pconstant @(PBuiltinList PInteger) [1 .. 40])
      ]
  ]
