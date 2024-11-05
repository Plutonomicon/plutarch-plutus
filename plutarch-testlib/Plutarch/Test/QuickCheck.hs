{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Test.QuickCheck (
  propEval,
  propEvalFail,
  propCompileFail,
  propEvalEqual,
  checkHaskellEquivalent,
  checkHaskellEquivalent2,

  -- * PFun
  PFun (PFun),
  pattern PFn,
  pattern PFn2,
  pattern PFn3,
) where

import Data.List (find)
import Data.Text qualified as Text
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Prelude hiding (Positive)
import Plutarch.Test.Unit (TermResult (Evaluated, FailedToCompile, FailedToEvaluate), evalTermResult)
import Plutarch.Test.Utils (prettyEquals, prettyShow)
import Prettyprinter (Pretty)
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Gen,
  Negative (Negative),
  NonNegative (NonNegative),
  NonPositive (NonPositive),
  NonZero (NonZero),
  Positive (Positive),
  Property,
  applyFun,
  applyFun2,
  arbitrary,
  counterexample,
  forAllShrinkShow,
  property,
  shrink,
  (===),
 )
import Test.QuickCheck.Function (Fun (Fun), functionElements)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.QuickCheck (Function, testProperty)

{- | Like `Plutarch.Test.Unit.testEvalFail` but generate terms

@since WIP
-}
propEvalFail ::
  (Arbitrary a, Show a) =>
  TestName ->
  (a -> ClosedTerm b) ->
  TestTree
propEvalFail name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate _ _ -> property True
      Evaluated script _ -> counterexample ("Evaluated, but expected failure: " <> script) False

{- | Like `Plutarch.Test.Unit.testCompileFail` but generate terms

@since WIP
-}
propCompileFail ::
  (Arbitrary a, Show a) =>
  TestName ->
  (a -> ClosedTerm b) ->
  TestTree
propCompileFail name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile _ -> property True
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated script _ -> counterexample ("Evaluated, but expected failure: " <> script) False

{- | Like `Plutarch.Test.Unit.testEvalEqual` but generate terms

@since WIP
-}
propEvalEqual ::
  (Arbitrary a, Show a) =>
  TestName ->
  -- | Actual
  (a -> ClosedTerm b) ->
  -- | Expected
  (a -> ClosedTerm b) ->
  TestTree
propEvalEqual name mkTerm mkExpected =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated actual _ -> case evalTermResult NoTracing (mkExpected input) of
        FailedToCompile err -> counterexample ("Failed to compile expected term: " <> Text.unpack err) False
        FailedToEvaluate err _ -> counterexample ("Failed to evaluate expected term: " <> show err) False
        Evaluated expected _ -> actual === expected

{- | Like `Plutarch.Test.Unit.testEval` but generate terms

@since WIP
-}
propEval :: (Arbitrary a, Show a) => TestName -> (a -> ClosedTerm b) -> TestTree
propEval name mkTerm =
  testProperty name $ forAllShrinkShow arbitrary shrink show $ \(input :: a) ->
    case evalTermResult NoTracing (mkTerm input) of
      FailedToCompile err -> counterexample ("Failed to compile: " <> Text.unpack err) False
      FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
      Evaluated _ _ -> property True

-- | @since WIP
checkHaskellEquivalent ::
  forall (plutarchInput :: S -> Type) (plutarchOutput :: S -> Type).
  ( PUnsafeLiftDecl plutarchInput
  , Pretty (PLifted plutarchInput)
  , Arbitrary (PLifted plutarchInput)
  , PUnsafeLiftDecl plutarchOutput
  , Pretty (PLifted plutarchOutput)
  , Eq (PLifted plutarchOutput)
  ) =>
  (PLifted plutarchInput -> PLifted plutarchOutput) ->
  ClosedTerm (plutarchInput :--> plutarchOutput) ->
  Property
checkHaskellEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input :: PLifted haskellInput) -> goHaskell input `prettyEquals` plift (goPlutarch # pconstant input)

-- | @since WIP
checkHaskellEquivalent2 ::
  forall (plutarchInput1 :: S -> Type) (plutarchInput2 :: S -> Type) (plutarchOutput :: S -> Type).
  ( PUnsafeLiftDecl plutarchInput1
  , Pretty (PLifted plutarchInput1)
  , Arbitrary (PLifted plutarchInput1)
  , PUnsafeLiftDecl plutarchInput2
  , Pretty (PLifted plutarchInput2)
  , Arbitrary (PLifted plutarchInput2)
  , PUnsafeLiftDecl plutarchOutput
  , Pretty (PLifted plutarchOutput)
  , Eq (PLifted plutarchOutput)
  ) =>
  (PLifted plutarchInput1 -> PLifted plutarchInput2 -> PLifted plutarchOutput) ->
  ClosedTerm (plutarchInput1 :--> plutarchInput2 :--> plutarchOutput) ->
  Property
checkHaskellEquivalent2 goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \(input1 :: PLifted haskellInput1, input2 :: PLifted haskellInput2) ->
      goHaskell input1 input2 `prettyEquals` plift (goPlutarch # pconstant input1 # pconstant input2)

-- * PFun

type PFun :: Type -> (S -> Type) -> Type -> Type
data PFun ah ap r = PFun (ClosedTerm (ap :--> PConstanted r)) (Fun ah r) r [(ah, r)]

instance (Show ah, Show r) => Show (PFun ah ap r) where
  show (PFun _ fh d t) = show fh <> " (" <> show t <> ") " <> show d

applyFunH :: PFun ah ap r -> (ah -> r)
applyFunH (PFun _ fh _ _) = applyFun fh

applyFunP :: PIsData ap => PFun ah (PAsData ap) r -> ClosedTerm (ap :--> PConstanted r)
applyFunP (PFun fp _ _ _) = plam $ \a -> fp # pdata a

{-# COMPLETE PFn #-}
pattern PFn ::
  forall (xh :: Type) (xp :: S -> Type) (r :: Type).
  PIsData xp =>
  (xh -> r) ->
  ClosedTerm (xp :--> PConstanted r) ->
  (PFun xh (PAsData xp) r, PFun xh (PAsData xp) r)
pattern PFn fh fp <- (applyFunH -> fh, applyFunP -> fp)

applyFunH2 :: PFun (xh, yh) ap r -> (xh -> yh -> r)
applyFunH2 (PFun _ fh _ _) = applyFun2 fh

applyFunP2 ::
  (PIsData xp, PIsData yp) =>
  PFun ah (PBuiltinPair (PAsData xp) (PAsData yp)) r ->
  ClosedTerm (xp :--> yp :--> PConstanted r)
applyFunP2 (PFun fp _ _ _) = plam $ \x y -> fp #$ ppairDataBuiltin # pdata x # pdata y

{-# COMPLETE PFn2 #-}
pattern PFn2 ::
  forall (xh :: Type) (xp :: S -> Type) (yh :: Type) (yp :: S -> Type) (r :: Type).
  (PIsData xp, PIsData yp) =>
  (xh -> yh -> r) ->
  ClosedTerm (xp :--> (yp :--> PConstanted r)) ->
  (PFun (xh, yh) (PBuiltinPair (PAsData xp) (PAsData yp)) r, PFun (xh, yh) (PBuiltinPair (PAsData xp) (PAsData yp)) r)
pattern PFn2 fh fp <- (applyFunH2 -> fh, applyFunP2 -> fp)

applyPFun3 ::
  forall (xh :: Type) (xp :: S -> Type) (yh :: Type) (yp :: S -> Type) (zh :: Type) (zp :: S -> Type) (r :: Type).
  (PIsData xp, PIsData yp, PIsData zp) =>
  PFun (xh, (yh, zh)) (PBuiltinPair (PAsData xp) (PAsData (PBuiltinPair (PAsData yp) (PAsData zp)))) r ->
  ( ClosedTerm (xp :--> (yp :--> (zp :--> PConstanted r)))
  , xh -> yh -> zh -> r
  )
applyPFun3 (PFun fp fh _ _) = (plam $ \x y z -> fp #$ ppairDataBuiltin # pdata x # pdata (ppairDataBuiltin # pdata y # pdata z), \x y z -> applyFun fh (x, (y, z)))

{-# COMPLETE PFn3 #-}
pattern PFn3 ::
  forall (xh :: Type) (xp :: S -> Type) (yh :: Type) (yp :: S -> Type) (zh :: Type) (zp :: S -> Type) (r :: Type).
  (PIsData xp, PIsData yp, PIsData zp) =>
  (xh -> yh -> zh -> r) ->
  ClosedTerm (xp :--> (yp :--> (zp :--> PConstanted r))) ->
  PFun (xh, (yh, zh)) (PBuiltinPair (PAsData xp) (PAsData (PBuiltinPair (PAsData yp) (PAsData zp)))) r
pattern PFn3 fh fp <- (applyPFun3 -> (fp, fh))

-- TODO: Figure out shrinking
-- NOTE: Doesn't work because covered input set is different
-- instance
--   forall (ah :: Type) (ap :: S -> Type) (c :: Type).
--   ( Arbitrary ah
--   , CoArbitrary ah
--   , Function ah
--   , Eq ah
--   , ap ~ PAsData (PConstanted ah)
--   , PEq ap
--   , PLifted (PConstanted ah) ~ ah
--   , PConstantDecl ah
--   , PIsData (PConstanted ah)
--   , Arbitrary c
--   , CoArbitrary c
--   , Function c
--   , PConstantDecl c
--   , PLifted (PConstanted c) ~ c
--   ) =>
--   Arbitrary (PFun ah ap c)
--   where
--   arbitrary = do
--     fun@(Fun qcF@(_, def, _) _) <- arbitrary
--     inputs <- arbitrary
--     let
--       Fn f = fun
--       table = map (\x -> (plift (pdata (pconstant x)), f x)) inputs
--     pure $
--       PFun
--         (plookupDef # pconstant def # pconstant table)
--         (Fun qcF f)

-- instance
--   {-# OVERLAPPING #-}
--   forall (ah :: Type) (x :: Type) (y :: Type) (ap :: S -> Type) (xp :: S -> Type) (yp :: S -> Type) (r :: Type).
--   ( Arbitrary ah
--   , CoArbitrary ah
--   , Function ah
--   , Eq ah
--   , (x, y) ~ ah
--   , ap ~ PConstanted ah
--   , PEq ap
--   , PLifted (PConstanted ah) ~ ah
--   , PConstantDecl ah
--   , Arbitrary r
--   , CoArbitrary r
--   , Function r
--   , PConstantDecl r
--   , PLifted (PConstanted r) ~ r
--   , xp ~ PConstanted x
--   , PIsData xp
--   , PUnsafeLiftDecl xp
--   , yp ~ PConstanted y
--   , PIsData yp
--   , PUnsafeLiftDecl yp
--   ) =>
--   Arbitrary (PFun (x, y) (PBuiltinPair (PAsData xp) (PAsData yp)) r)
--   where
--   arbitrary = do
--     fun@(Fun qcF@(_, def, _) _) <- arbitrary
--     inputs <- arbitrary
--     let
--       Fn f = fun
--       table = map (\a@(x, y) -> ((plift (pdata (pconstant x)), plift (pdata (pconstant y))), f a)) inputs
--     pure $
--       PFun
--         (plookupDef # pconstant def # pconstant table)
--         (Fun qcF f)

instance
  {-# OVERLAPPING #-}
  forall (ah :: Type) (x :: Type) (y :: Type) (z :: Type) (ap :: S -> Type) (xp :: S -> Type) (yp :: S -> Type) (zp :: S -> Type) (r :: Type).
  ( Arbitrary ah
  , CoArbitrary ah
  , Function ah
  , Eq ah
  , (x, (y, z)) ~ ah
  , ap ~ PConstanted ah
  , PLifted (PConstanted ah) ~ ah
  , PConstantDecl ah
  , Arbitrary r
  , PConstantDecl r
  , PLifted (PConstanted r) ~ r
  , xp ~ PConstanted x
  , PIsData xp
  , PUnsafeLiftDecl xp
  , yp ~ PConstanted y
  , PIsData yp
  , PUnsafeLiftDecl yp
  , zp ~ PConstanted z
  , PIsData zp
  , PUnsafeLiftDecl zp
  ) =>
  Arbitrary (PFun (x, (y, z)) (PBuiltinPair (PAsData xp) (PAsData (PBuiltinPair (PAsData yp) (PAsData zp)))) r)
  where
  arbitrary = do
    Fun (_, _, notShrunk) _ <- arbitrary :: Gen (Fun () ())
    tableH <- arbitrary :: Gen [((x, (y, z)), r)]
    def <- arbitrary :: Gen r
    let
      fh inp = maybe def snd (find ((inp ==) . fst) tableH)
      fqc = functionElements (map fst tableH) fh

      tableP =
        map
          ( \((x :: x, (y :: y, z :: z)), r :: r) ->
              (
                ( plift (pdata (pconstant x))
                , plift
                    ( pdata
                        ( pconstant
                            ( plift (pdata (pconstant y))
                            , plift (pdata (pconstant z))
                            )
                        )
                    )
                )
              , r
              )
          )
          tableH
    pure $
      PFun
        (plookupDef # pconstant def # pconstant tableP)
        (Fun (fqc, def, notShrunk) fh)
        def
        tableH

  shrink (PFun _ f def' tableH') = do
    Fun (_, _, shrunk) _ <- shrink f
    def <- shrink def'
    tableH <- shrink tableH'
    let
      fh inp = maybe def snd (find ((inp ==) . fst) tableH)
      fqc = functionElements (map fst tableH) fh

      tableP =
        map
          ( \((x :: x, (y :: y, z :: z)), r :: r) ->
              (
                ( plift (pdata (pconstant x))
                , plift
                    ( pdata
                        ( pconstant
                            ( plift (pdata (pconstant y))
                            , plift (pdata (pconstant z))
                            )
                        )
                    )
                )
              , r
              )
          )
          tableH
    pure $
      PFun
        (plookupDef # pconstant def # pconstant tableP)
        (Fun (fqc, def, shrunk) fh)
        def
        tableH

plookupDef ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S) list.
  (PEq a, PIsListLike list (PBuiltinPair a b)) =>
  Term s (b :--> list (PBuiltinPair a b) :--> a :--> b)
plookupDef =
  phoistAcyclic $
    plam $ \b xs k ->
      pmatch (pfind # plam (\p -> pfstBuiltin # p #== k) # xs) $ \case
        PNothing -> b
        PJust p -> psndBuiltin # p

-- * Orphans

-- | @since WIP
deriving newtype instance Pretty a => Pretty (Positive a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (Negative a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (NonZero a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (NonNegative a)

-- | @since WIP
deriving newtype instance Pretty a => Pretty (NonPositive a)
