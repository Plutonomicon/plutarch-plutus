{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Equivalent (checkHaskellEquivalent) where

import GHC.TypeError (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import Plutarch.Test.Utils (prettyEquals, prettyShow)
import PlutusLedgerApi.V1.Orphans ()
import Prettyprinter (Pretty)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  forAllShrinkShow,
 )

{- | Check that given Haskell and Plutarch functions produce the same output for the same input.
It works for functions with any number of arguments

All Haskell arguments must have `PLifted` Plutarch equivalent.

 @since WIP
-}
checkHaskellEquivalent ::
  forall (f :: Type).
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
checkHaskellEquivalent goHaskell goPlutarch =
  forAllShrinkShow arbitrary shrink prettyShow $
    \input -> applyTupleArgs goHaskell input `prettyEquals` plift (papplyTupleArgs goPlutarch (pconstant input))

-- * Haskell level

type family ToPlutarchFunction f where
  ToPlutarchFunction (a -> b -> c) = PConstanted a :--> ToPlutarchFunction (b -> c)
  ToPlutarchFunction (a -> b) = PConstanted a :--> PConstanted b
  ToPlutarchFunction a = TypeError (Text "Not a function: " :<>: ShowType a)

-- | Convert function arguments to a nested tuple (e.g. from @(a -> b -> c -> d)@ to @(a, (b, (c, d)))@ )
type family FunctionArgumentsToTuple f where
  FunctionArgumentsToTuple (a -> (b -> c)) = (a, FunctionArgumentsToTuple (b -> c))
  FunctionArgumentsToTuple (a -> _) = a

-- | Get final result of the function (e.g. result of @(a -> b -> c -> d)@ is @d@)
type family FunctionResult f where
  FunctionResult (_ -> (b -> c)) = FunctionResult (b -> c)
  FunctionResult (_ -> b) = b

-- | Convert n-ary Haskell function to unary function that takes a nested tuple of arguments
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
  , FunctionArgumentsToTuple f ~ a
  , FunctionResult f ~ b
  ) =>
  ApplyTupleArgs (a -> b)
  where
  applyTupleArgs f = f

-- * Plutarch level

-- | Like `FunctionArgumentsToTuple` but on Plutarch level and using `PBuiltinPair`
type family PFunctionArgumentsToTuple (f :: S -> Type) :: S -> Type where
  PFunctionArgumentsToTuple (a :--> (b :--> c)) =
    PBuiltinPair a (PFunctionArgumentsToTuple (b :--> c))
  PFunctionArgumentsToTuple (a :--> _) = a

-- | Like `FunctionResult` but on Plutarch level
type family PFunctionResult (f :: S -> Type) where
  PFunctionResult (_ :--> (b :--> c)) = PFunctionResult (b :--> c)
  PFunctionResult (_ :--> b) = b

-- | Convert n-ary Plutarch function to unary function that takes a nested PBuiltinPair of arguments
class PApplyTupleArgs (f :: S -> Type) where
  papplyTupleArgs :: Term s f -> Term s (PFunctionArgumentsToTuple f) -> Term s (PFunctionResult f)

instance
  {-# OVERLAPPABLE #-}
  ( f ~ (a :--> b)
  , PFunctionArgumentsToTuple f ~ a
  , PFunctionResult f ~ b
  ) =>
  PApplyTupleArgs f
  where
  papplyTupleArgs f args = f # args

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
