{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
import Data.Kind
import Data.SOP
import GHC.TypeLits
import Generics.SOP (Generic (Code, to), from)
import Plutarch.Internal (ClosedTerm, PType, Term, compile, phoistAcyclic, punsafeCoerce, (:-->))
import qualified Plutarch.Internal as PI
import Plutarch.Internal.Generic
import Plutarch.Internal.PLam (pinl, plam, (#), (#$))
import Plutarch.Internal.PlutusType (PCon (pcon), PMatch (pmatch), PlutusType (PInner, pcon', pmatch'))
import Plutarch.Internal.TypeFamily
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
newtype DerivePNewtype (a :: PType) (s :: PI.S) = DerivePNewtype (a s)

instance
  ( s ~ 'PI.SI
  , PCode s a ~ '[ '[SingletonSum (PCode s a)]]
  , PGeneric s a
  ) =>
  PlutusType (DerivePNewtype a)
  where
  -- TODO: reject ADTs isomorphic to newtypes
  type PInner (DerivePNewtype a) _ = SingletonSum (PCode 'PI.SI a)
  pcon' (DerivePNewtype t) = undefined -- innerTerm t
  pmatch' x f = f $ DerivePNewtype $ undefined -- outerTerm x

data InterpDerivStrat = PNewtype
class DerivingStrat strat a => DerivePlutusType (strat :: InterpDerivStrat) a | a -> strat
class DerivingStrat strat a where
  type DerivedPInner a (b' :: PType) :: PType
  derivedPcon' :: forall s b. a s -> Term s (DerivedPInner a b)
  derivedPmatch' :: forall s b. (Term s (DerivedPInner a b)) -> (a s -> Term s b) -> Term s b

instance
  ( PGeneric s a
  , SingletonSumC (PCode s a)
  , SingletonSum (PCode 'PI.SI a) ~ SingletonSum (PCode s a)
  ) =>
  DerivingStrat 'PNewtype a
  where
  type DerivedPInner a _ = (SingletonSum (PCode 'PI.SI a))
  derivedPcon' ::
    forall s b.
    a s ->
    Term s (DerivedPInner a b)
  derivedPcon' t = singletonSum @(PCode s a) $ gpfrom t
  derivedPmatch' x f = f undefined
-- derivedPmatch' x f = f $ gpto $ unSingletonSum x

class SingletonSumC (code :: [[PType]]) where
  type SingletonSum code :: PType
  singletonSum :: forall s. SOP (Term s) code -> Term s (SingletonSum code)
  unSingletonSum :: forall s. Term s (SingletonSum code) -> SOP (Term s) code

instance SingletonSumC '[ '[x]] where
  type SingletonSum '[ '[x]] = x
  singletonSum t = hd $ unZ $ unSOP t
  unSingletonSum t = SOP $ Z $ t :* Nil

{-
data Bar s
newtype Foo s = Foo (Term s Bar)

deriving instance
  ( PlutusType a
  , PGeneric s a
  , PCode s a ~ '[ '[SingletonSum (PCode s a)]]
  ) =>
  DerivePlutusType 'PNewtype a
instance PlutusType a where
  type PInner a b = DerivedPInner a b

x :: Term s Foo
x = pcon $ Foo undefined
-}

anySTerm :: forall s1 s2 a. Term s1 a -> Term s2 a
anySTerm (PI.Term x) = PI.Term x

{-
instance Semigroup (Term s (SingletonSum (PCode 'PI.SI a))) => Semigroup (Term s (DerivePNewtype a)) where
  x <> y = punsafeFrom $ pto x <> pto y

instance Monoid (Term s (SingletonSum (PCode 'PI.SI a))) => Monoid (Term s (DerivePNewtype a)) where
  mempty = punsafeFrom $ mempty @(Term s (SingletonSum (PCode 'PI.SI a)))

instance Num (Term s (SingletonSum (PCode 'PI.SI a))) => Num (Term s (DerivePNewtype a)) where
  x + y = punsafeFrom $ pto x + pto y
  x - y = punsafeFrom $ pto x - pto y
  x * y = punsafeFrom $ pto x * pto y
  abs x = punsafeFrom $ abs $ pto x
  negate x = punsafeFrom $ negate $ pto x
  signum x = punsafeFrom $ signum $ pto x
  fromInteger x = punsafeFrom $ fromInteger @(Term s ((SingletonSum (PCode 'PI.SI a)))) x

punsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
punsafeFrom x = PI.punsafeCoerce x

-}