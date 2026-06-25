{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.Primitive.Liftable (
  LiftError (..),
  PLiftable (..),
  pconstant,
  plift,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import PlutusCore qualified as PLC

-- | @since wip
data LiftError = LiftError

{- | = Laws

1. @'reprToHask' '.' 'haskToRepr'@ @=@ @'Right'@
2. @'plutToRepr' '.' 'reprToPlut'@ @=@ @'Right'@

@since wip
-}
class PLC.DefaultUni `PLC.Contains` PAsPlutus a => PLiftable (a :: S -> Type) where
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
    Left _ -> error "plift failed: Plutus representation does not correspond to Haskell value"
    Right res' -> res'

-- Helpers

showLiftError :: LiftError -> String
showLiftError = \case
  LiftError -> "something broke"
