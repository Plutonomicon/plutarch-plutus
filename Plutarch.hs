{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

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
  printScript,
  (#$),
  (#),
  pinl,
  PCon (..),
  PMatch (..),
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

printScript :: Script -> String
printScript = show . prettyPlcReadableDebug . (\(Script s) -> s)

-- TODO: Heavily improve. It's unreadable right now.
-- We could convert the de Bruijn indices into names.
-- show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
printTerm :: ClosedTerm a -> String
printTerm term = printScript $ compile term

(#) :: Term s (a :--> b) -> Term s a -> Term s b
(#) = papp
infixl 8 #

(#$) :: Term s (a :--> b) -> Term s a -> Term s b
(#$) = papp
infixr 0 #$

class PLamN a b | a -> b where
  plam :: a -> b

-- FIXME: This piece of code doesn't work unless you do (_ :: Term _ _)
{-
f :: Term s ((a :--> b) :--> b :--> b)
f = plam $ \f _ -> f # perror
-}

instance {-# INCOHERENT #-} (a' ~ Term s a, b' ~ Term s b) => PLamN (a' -> b') (Term s (a :--> b)) where
  plam = plam'

instance {-# INCOHERENT #-} (a' ~ Term s a, b' ~ Term s b, c' ~ Term s c) => PLamN (a' -> b' -> c') (Term s (a :--> b :--> c)) where
  plam f = plam' $ \x -> plam (f x)

instance {-# INCOHERENT #-} (a' ~ Term s a, b' ~ Term s b, c' ~ Term s c, d' ~ Term s d) => PLamN (a' -> b' -> c' -> d') (Term s (a :--> b :--> c :--> d)) where
  plam f = plam' $ \x -> plam (f x)

instance {-# INCOHERENT #-} (a' ~ Term s a, b' ~ Term s b, c' ~ Term s c, d' ~ Term s d, e' ~ Term s e) => PLamN (a' -> b' -> c' -> d' -> e') (Term s (a :--> b :--> c :--> d :--> e)) where
  plam f = plam' $ \x -> plam (f x)

instance {-# INCOHERENT #-} (a' ~ Term s a, b' ~ Term s b, c' ~ Term s c, d' ~ Term s d, e' ~ Term s e, f' ~ Term s f) => PLamN (a' -> b' -> c' -> d' -> e' -> f') (Term s (a :--> b :--> c :--> d :--> e :--> f)) where
  plam f = plam' $ \x -> plam (f x)

instance {-# INCOHERENT #-} (a' ~ Term s a, b' ~ Term s b, c' ~ Term s c, d' ~ Term s d, e' ~ Term s e, f' ~ Term s f, g' ~ Term s g) => PLamN (a' -> b' -> c' -> d' -> e' -> f' -> g') (Term s (a :--> b :--> c :--> d :--> e :--> f :--> g)) where
  plam f = plam' $ \x -> plam (f x)

pinl :: Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v

class (PCon a, PMatch a) => PlutusType (a :: k -> Type) where
  -- `b' :: k'` causes GHC to fail type checking at various places
  -- due to not being able to expand the type family.
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c

instance {-# OVERLAPPABLE #-} PlutusType a => PMatch a where
  pmatch x f = pmatch' (punsafeCoerce x) f

instance PlutusType a => PCon a where
  pcon = punsafeCoerce . pcon'

class PCon a where
  pcon :: a s -> Term s a

class PMatch a where
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b

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
      (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))
        # punsafeCoerce (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))
