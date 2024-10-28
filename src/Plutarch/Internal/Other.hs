{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Other (
  printTerm,
  printScript,
) where

import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Plutarch.Internal.Term (
  ClosedTerm,
  Config,
  compile,
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
