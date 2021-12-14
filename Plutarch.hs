{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch (
  (PI.:-->),
  PI.PDelayed,
  PI.Term,
  PI.plam',
  PI.plet,
  PI.papp,
  PI.pdelay,
  PI.pforce,
  PI.phoistAcyclic,
  PI.perror,
  PI.punsafeCoerce,
  PI.punsafeBuiltin,
  PI.punsafeConstant,
  PI.compile,
  PI.ClosedTerm,
  PlutusType (..),
  printTerm,
  (£$),
  (£),
  pinl,
  pcon,
  pmatch,
  punsafeFrom,
  pto,
  pfix,
  POpaque (..),
  popaque,
  punsafeFromOpaque,
  plam,
) where

import Data.Kind (Type)
import Plutarch.Internal (ClosedTerm, Term, compile, papp, phoistAcyclic, plam', punsafeCoerce, (:-->))
import qualified Plutarch.Internal as PI
import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore.Pretty (prettyPlcReadableDebug)

-- TODO: Heavily improve. It's unreadable right now.
-- We could convert the de Bruijn indices into names.
-- show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
printTerm :: ClosedTerm a -> String
printTerm term = show . prettyPlcReadableDebug . (\(Script s) -> s) $ compile term

(£) :: Term s (a :--> b) -> Term s a -> Term s b
(£) = papp
infixl 8 £

(£$) :: Term s (a :--> b) -> Term s a -> Term s b
(£$) = papp
infixr 0 £$

-- TODO: Improve type inference when using plam

class PLam (s :: k) (b :: Type) where
  type PLamOut b :: (k -> Type)
  plam :: forall (a :: k -> Type). (Term s a -> b) -> Term s (a :--> PLamOut b)

instance PLam s (Term s b) where
  type PLamOut (Term s b) = b
  plam :: forall a. (Term s a -> Term s b) -> Term s (a :--> b)
  plam = plam'

instance PLam s c => PLam s (Term s b -> c) where
  type PLamOut (Term s b -> c) = b :--> PLamOut c
  plam :: forall a. (Term s a -> Term s b -> c) -> Term s (a :--> b :--> PLamOut c)
  plam f = plam' $ \x -> plam (f x)

pinl :: Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v

class PlutusType (a :: k -> Type) where
  -- `b' :: k'` causes GHC to fail type checking at various places
  -- due to not being able to expand the type family.
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c

pcon :: PlutusType a => a s -> Term s a
pcon = punsafeCoerce . pcon'

pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
pmatch x f = pmatch' (punsafeCoerce x) f

punsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
punsafeFrom = punsafeCoerce

pto :: Term s a -> (forall b. Term s (PInner a b))
pto = punsafeCoerce

data POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

popaque :: Term s a -> Term s POpaque
popaque = punsafeCoerce

punsafeFromOpaque :: Term s POpaque -> Term s a
punsafeFromOpaque = punsafeCoerce

pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam $ \f ->
      (plam $ \(x :: Term s POpaque) -> f £ (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) £ x £ v))
        £ punsafeCoerce (plam $ \(x :: Term s POpaque) -> f £ (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) £ x £ v))
