{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Other (
  (PI.:-->),
  PI.ClosedTerm,
  PI.compile,
  PI.Dig,
  PI.hashTerm,
  PI.papp,
  PI.pdelay,
  PI.PDelayed,
  PI.perror,
  PI.pforce,
  PI.phoistAcyclic,
  PI.plam',
  PI.plet,
  PI.Term,
  PI.S,
  PI.PType,
  PlutusType (..),
  printTerm,
  printScript,
  (#$),
  (#),
  pinl,
  PCon (..),
  PMatch (..),
  pto,
  pfix,
  POpaque (..),
  popaque,
  plam,
  DerivePNewtype (DerivePNewtype),
) where

import Data.Coerce (Coercible, coerce)
import Plutarch.Internal (ClosedTerm, PType, Term, compile, phoistAcyclic, punsafeCoerce, (:-->))
import qualified Plutarch.Internal as PI
import Plutarch.Internal.PLam (pinl, plam, (#), (#$))
import Plutarch.Internal.PlutusType (PCon (pcon), PMatch (pmatch), PlutusType (PInner, pcon', pmatch'))
import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore.Pretty (prettyPlcReadableDebug)

-- | Prettyprint a compiled Script via the PLC pretty printer
printScript :: Script -> String
printScript = show . prettyPlcReadableDebug . (\(Script s) -> s)

{- | Prettyprint a Term via the PLC pretty printer

  TODO: Heavily improve. It's unreadable right now.

  We could convert the de Bruijn indices into names with:

  > show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
-}
printTerm :: ClosedTerm a -> String
printTerm term = printScript $ compile term

{- |
  Safely coerce from a Term to it's 'PInner' representation.
-}
pto :: Term s a -> (forall b. Term s (PInner a b))
pto x = punsafeCoerce x

-- | An Arbitrary Term with an unknown type
data POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

-- | Erase the type of a Term
popaque :: Term s a -> Term s POpaque
popaque = punsafeCoerce

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
  >  Term s PInteger ->
  >  Term s (a :--> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs
-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam $ \f ->
      (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))
        # punsafeCoerce (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))

{- | Facilitates deriving 'PlutusType' and 'PIsData' for newtypes.

For any newtype represented as-
> newtype PFoo (s :: S) = PFoo (Term s PBar)

where 'PBar' has a 'PIsData' instance, you can derive 'PlutusType' and 'PIsData' using-
> deriving (PlutusType, PIsData) via (DerivePNewtype PFoo PBar)

This will make 'PFoo' simply be represnted as 'PBar' under the hood.
-}
type role DerivePNewtype representational representational nominal

newtype DerivePNewtype (a :: PType) (b :: PType) (s :: PI.S) = DerivePNewtype (a s)

instance (forall (s :: PI.S). Coercible (a s) (Term s b)) => PlutusType (DerivePNewtype a b) where
  type PInner (DerivePNewtype a b) _ = b
  pcon' (DerivePNewtype t) = ptypeInner t
  pmatch' x f = f . DerivePNewtype $ ptypeOuter x

instance Semigroup (Term s b) => Semigroup (Term s (DerivePNewtype a b)) where
  x <> y = punsafeDowncast $ pto x <> pto y

instance Monoid (Term s b) => Monoid (Term s (DerivePNewtype a b)) where
  mempty = punsafeDowncast $ mempty @(Term s b)

instance Num (Term s b) => Num (Term s (DerivePNewtype a b)) where
  x + y = punsafeDowncast $ pto x + pto y
  x - y = punsafeDowncast $ pto x - pto y
  x * y = punsafeDowncast $ pto x * pto y
  abs x = punsafeDowncast $ abs $ pto x
  negate x = punsafeDowncast $ negate $ pto x
  signum x = punsafeDowncast $ signum $ pto x
  fromInteger x = punsafeDowncast $ fromInteger @(Term s b) x

ptypeInner :: forall (x :: PType) y s. Coercible (x s) (Term s y) => x s -> Term s y
ptypeInner = coerce

ptypeOuter :: forall (x :: PType) y s. Coercible (x s) (Term s y) => Term s y -> x s
ptypeOuter = coerce

punsafeDowncast :: (forall b. Term s (PInner a b)) -> Term s a
punsafeDowncast x = PI.punsafeCoerce x
