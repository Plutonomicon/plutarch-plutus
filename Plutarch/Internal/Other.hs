{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Other (
  printTerm,
  printScript,
  pto,
  pfix,
  POpaque (..),
  popaque,
) where

import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Plutarch.Internal (ClosedTerm, Config, Term, compile, phoistAcyclic, plam', punsafeCoerce, (#), (:-->))
import Plutarch.Internal.PlutusType (
  PContravariant',
  PCovariant',
  PInner,
  PVariant',
  PlutusType,
  pcon',
  pmatch',
 )
import Plutarch.Script (Script (Script))
import PlutusCore.Pretty (prettyPlcReadableDebug)

-- | Prettyprint a compiled Script via the PLC pretty printer
printScript :: Script -> String
printScript = show . prettyPlcReadableDebug . (\(Script s) -> s)

{- | Prettyprint a Term via the PLC pretty printer

  TODO: Heavily improve. It's unreadable right now.

  We could convert the de Bruijn indices into names with:

  > show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
-}
printTerm :: HasCallStack => Config -> ClosedTerm a -> String
printTerm config term = printScript $ either (error . T.unpack) id $ compile config term

{- |
  Safely coerce from a Term to it's 'PInner' representation.
-}
pto :: Term s a -> Term s (PInner a)
pto = punsafeCoerce

-- | An Arbitrary Term with an unknown type
newtype POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque = POpaque
  type PCovariant' POpaque = ()
  type PContravariant' POpaque = ()
  type PVariant' POpaque = ()
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
    plam' $ \f ->
      plam' (\(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
        # punsafeCoerce (plam' $ \(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
