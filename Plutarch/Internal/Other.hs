{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Other (
  printTerm,
  printScript,
  pto,
  Flip,
) where

import Data.Kind (Type)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Plutarch.Internal.PlutusType (
  PInner,
 )
import Plutarch.Internal.Term (
  Config,
  S,
  Term,
  compile,
  punsafeCoerce,
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
printTerm :: forall (a :: S -> Type). HasCallStack => Config -> (forall (s :: S). Term s a) -> String
printTerm config term = printScript $ either (error . T.unpack) id $ compile config term

{- |
  Safely coerce from a Term to it's 'PInner' representation.
-}
pto :: Term s a -> Term s (PInner a)
pto = punsafeCoerce

{- | Type level flip operation, reversing the order of arguments
Commonly used in Plutarch to get the PTryFromExcess associated type of PTryFrom for a Plutarch type
@since 1.12.0
-}
newtype Flip (f :: k1 -> k2 -> Type) (a :: k2) (b :: k1) = Flip (f b a)
  deriving stock (Generic)
