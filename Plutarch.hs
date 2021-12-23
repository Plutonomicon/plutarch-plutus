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
  printScript,
  (#$),
  (#),
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
  High precedence infixl synonym of 'papp', to be used like 
  function juxtaposition. e.g.:

  >>> f # x # y
  f x y

-}
(#) :: Term s (a :--> b) -> Term s a -> Term s b
(#) = papp
infixl 8 #

{- | 
  Low precedence infixr synonym of 'papp', to be used like
  '$', in combination with '#'. e.g.:

  >>> f # x #$ g # y # z
  f x (g y z)

-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
(#$) = papp
infixr 0 #$

-- TODO: Improve type inference when using plam
class PLam (s :: k) (b :: Type) where
  type PLamOut b :: (k -> Type)

  {- | 
    Lambda abstraction.

    The 'PLamOut' type family on the return type allows 
    currying to work as expected for any number of arguments.

    > id :: Term s (a :--> a)
    > id = plam (\x -> x)

    > const :: Term s (a :--> b :-> a)
    > const = plam (\x y -> x)
   
  -}
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

{- |

  The 'PlutusType' class allows encoding Haskell data-types as plutus terms
  via constructors and destructors.

  A simple example, encoding a Sum type as an Enum via Integers.

  > data AB (s :: k) = A | B
  >
  > instance PlutusType AB where
  >   type PInner AB _ = PInteger
  >   
  >   pcon' A = 0
  >   pcon' B = 1
  >
  >   pmatch' x f = 
  >     pif (x #== 0) (f A) (f B)
  > 

  Instead of using `pcon'` and `pmatch'` directly,
  use 'pcon' and 'pmatch', to hide the `PInner` type:

  > swap :: Term s AB -> Term s AB
  > swap x = pmatch x $ \case
  >  A -> pcon B
  >  B -> pcon A
  
-}
class PlutusType (a :: k -> Type) where
  -- `b' :: k'` causes GHC to fail type checking at various places
  -- due to not being able to expand the type family.
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c

-- | Construct a Plutus Term via a Haskell value
pcon :: PlutusType a => a s -> Term s a
pcon = punsafeCoerce . pcon'

-- | Pattern match over a Term via a Haskell function
pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
pmatch x f = pmatch' (punsafeCoerce x) f

{- | 
  Unsafely coerce from the 'PInner' representation of a Term,
  assuming that the value is a safe construction of the Term.
-}
punsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
punsafeFrom = punsafeCoerce

{- | 
  Safely coerce from a Term to it's 'PInner' representation.
-}
pto :: Term s a -> (forall b. Term s (PInner a b))
pto = punsafeCoerce

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
  Unsafely coerce from an Opaque term to another type.
-}
punsafeFromOpaque :: Term s POpaque -> Term s a
punsafeFromOpaque = punsafeCoerce

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

-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam $ \f ->
      (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))
        # punsafeCoerce (plam $ \(x :: Term s POpaque) -> f # (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) # x # v))
