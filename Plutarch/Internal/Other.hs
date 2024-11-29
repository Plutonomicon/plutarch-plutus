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
import Plutarch.Builtin.Opaque
import Plutarch.Internal.PlutusType (
  PContravariant',
  PCovariant',
  PInner,
  PVariant',
  PlutusType,
  pcon',
  pmatch',
 )
import Plutarch.Internal.Term (
  ClosedTerm,
  Config,
  Term,
  compile,
  phoistAcyclic,
  plam',
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Script (Script (Script))
import PlutusCore.Pretty (prettyPlcReadable)

-- | Prettyprint a compiled Script via the PLC pretty printer
printScript :: Script -> String
printScript = show . prettyPlcReadable . (\(Script s) -> s)

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
