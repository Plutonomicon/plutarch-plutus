{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.Primitive.Liftable (
  LiftError (..),
  PLiftable (..),
  pconstant,
  plift,
  PLiftableDirect (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.Evaluate (EvalError, peval)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeCoerce, punsafeConstant)
import PlutusCore qualified as PLC
import PlutusCore.Builtin (GEqL, geqL)

-- | @since wip
data LiftError
  = DidNotEvaluate EvalError
  | NotAConstant
  | WrongConstantType
  | UnexpectedNegative Integer
  | ByteOutOfBounds Integer

{- | = Laws

1. @'reprToHask' '.' 'haskToRepr'@ @=@ @'Right'@
2. @'plutToRepr' '.' 'reprToPlut'@ @=@ @'Right'@

@since wip
-}
class
  (PLC.DefaultUni `PLC.Contains` PAsPlutus a, GEqL PLC.DefaultUni (PAsPlutus a)) =>
  -- Note (Koz, 26/06/2026): The second constraint here is needed to make
  -- `plutToRepr` work. The _short_ answer is that the machinery required to
  -- convert `Term` constants back into their (wrapped!) Haskell values involves
  -- a heterogenously-typed proof object comparison, for which there exists a
  -- dedicated, specialized type class (`GEqL` above). As this typeclass is
  -- standalone, and the type class universe is open, GHC _cannot_ infer the
  -- `GEqL` constraint from the containment constraint (despite the fact that it
  -- always holds and always should). While we could defer this problem to the
  -- specific instances that require it, doing so would create far too much
  -- typechecker noise, so we put it here.
  PLiftable (a :: S -> Type)
  where
  type PAsHaskell a :: Type
  type PAsPlutus a :: Type
  haskToRepr :: PAsHaskell a -> PAsPlutus a
  reprToHask :: PAsPlutus a -> Either LiftError (PAsHaskell a)
  reprToPlut :: forall (s :: S). PAsPlutus a -> Term s a
  plutToRepr :: (forall (s :: S). Term s a) -> Either LiftError (PAsPlutus a)

-- | @since wip
pconstant ::
  forall (a :: S -> Type) (s :: S).
  PLiftable a =>
  PAsHaskell a ->
  Term s a
pconstant = reprToPlut . haskToRepr @a

-- | @since wip
plift ::
  forall (a :: S -> Type).
  PLiftable a =>
  (forall (s :: S). Term s a) ->
  PAsHaskell a
plift t = case plutToRepr @a t of
  Left err -> error $ "plift failed: " <> showLiftError err
  Right res -> case reprToHask @a res of
    Left err -> error $ "plift failed: " <> showLiftError err
    Right res' -> res'

-- | @since wip
newtype PLiftableDirect (a :: S -> Type) (h :: Type) (s :: S) = PLiftableDirect (a s)

-- | @since wip
instance (PLC.DefaultUni `PLC.Contains` h, GEqL PLC.DefaultUni h) => PLiftable (PLiftableDirect a h) where
  type PAsHaskell (PLiftableDirect a h) = h
  type PAsPlutus (PLiftableDirect a h) = h
  haskToRepr = id
  reprToHask = Right
  reprToPlut x = punsafeCoerce . punsafeConstant $ PLC.someValue @h x
  plutToRepr t = case peval (punsafeCoerce @_ @a t) of
    Left err -> Left . DidNotEvaluate $ err
    -- Note (Koz, 26/06/2026): This sludge requires further explanation. When we
    -- get a constant back from the evaluator, we actually get two things:
    --
    -- 1. The constant itself (which is a Haskell value of some type); and
    -- 2. A proof of membership of said constant in the universe.
    --
    -- To ensure that what we have makes sense, we have to first verify that our
    -- proof of membership is what we expect: namely, that we got a proof
    -- specifically that `h` (and not some other type) is a member of the
    -- default universe. Doing this involves the use of `geqL`, as there is no
    -- guarantee that the proof will carry the same tag of what it's for as `h`.
    Right (Left c) -> case c of
      PLC.Some (PLC.ValueOf actualProof x) -> do
        let expectedProof = PLC.knownUni @_ @PLC.DefaultUni @h
        case geqL expectedProof actualProof of
          -- As the proofs match, in this branch, we know that x :: h
          PLC.EvaluationSuccess PLC.Refl -> pure x
          PLC.EvaluationFailure -> Left WrongConstantType
    Right (Right _) -> Left NotAConstant

-- Helpers

showLiftError :: LiftError -> String
showLiftError = \case
  DidNotEvaluate err -> "failed to evaluate: " <> show err
  NotAConstant -> "did not get a constant"
  WrongConstantType -> "got a constant of the wrong type"
  UnexpectedNegative i -> "got a negative value when we expected a non-negative: " <> show i
  ByteOutOfBounds i -> "cannot fit into a byte: " <> show i
