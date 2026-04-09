{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Internal.Other (
  printTerm,
  printScript,
  Flip,
) where

import Control.Monad.State.Strict (evalStateT)
import Data.Kind (Type)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Plutarch.Internal.Term (
  Config,
  S,
  Term,
  compile,
 )
import Plutarch.Script (Script (Script))
import PlutusCore qualified as PLC
import PlutusCore.Pretty (prettyPlcReadable)
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.String (renderString)
import UntypedPlutusCore qualified as UPLC

-- | Prettyprint a compiled Script via the PLC pretty printer
printScript :: Script -> String
printScript =
  renderString
    . layoutSmart defaultLayoutOptions
    . prettyPlcReadable
    . mkNames
    . UPLC.termMapNames UPLC.fakeNameDeBruijn
    . programToTerm
    . (\(Script s) -> s)
  where
    programToTerm ::
      UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
    programToTerm (UPLC.Program _ _ t) = t
    mkNames ::
      UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      UPLC.Term PLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
    mkNames t = case flip evalStateT UPLC.initSimplifierTrace . PLC.runQuoteT . UPLC.unDeBruijnTerm $ t of
      -- This is essentially impossible
      Left _ -> error "printScript: could not generate names. This should never happen."
      Right res -> res

{- | Prettyprint a Term via the PLC pretty printer

  TODO: Heavily improve. It's unreadable right now.

  We could convert the de Bruijn indices into names with:

  > show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
-}
printTerm :: forall (a :: S -> Type). HasCallStack => Config -> (forall (s :: S). Term s a) -> String
printTerm config term = printScript $ either (error . T.unpack) id $ compile config term

{- | Type level flip operation, reversing the order of arguments
Commonly used in Plutarch to get the PTryFromExcess associated type of PTryFrom for a Plutarch type

@since 1.12.0
-}
newtype Flip (f :: k1 -> k2 -> Type) (a :: k2) (b :: k1) = Flip (f b a)
  deriving stock (Generic)
