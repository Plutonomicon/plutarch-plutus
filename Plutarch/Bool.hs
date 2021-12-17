module Plutarch.Bool (PBool (..), PEq (..), POrd (..), pif, pif') where

import Plutarch (PlutusType (PInner, pcon', pmatch'), punsafeBuiltin, punsafeConstant)
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PBool s = PTrue | PFalse

instance PlutusType PBool where
  type PInner PBool _ = PBool
  pcon' PTrue = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
  pcon' PFalse = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool False
  pmatch' b f = pforce $ pif' £ b £ pdelay (f PTrue) £ pdelay (f PFalse)

class PEq t where
  (£==) :: Term s t -> Term s t -> Term s PBool

infix 4 £==

class POrd t where
  (£<=) :: Term s t -> Term s t -> Term s PBool
  (£<) :: Term s t -> Term s t -> Term s PBool

infix 4 £<=
infix 4 £<

{- | Strict version of 'pif'.
 Emits slightly less code.
-}
pif' :: Term s (PBool :--> a :--> a :--> a)
pif' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

-- | Lazy if-then-else.
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif b case_true case_false = pmatch b $ \case
  PTrue -> case_true
  PFalse -> case_false
