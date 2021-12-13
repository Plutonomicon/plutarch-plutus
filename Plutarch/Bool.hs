module Plutarch.Bool (PBool(..), PEq(..), POrd(..), pif) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pcon', pmatch'), punsafeConstant, punsafeBuiltin, POpaque)
import qualified PlutusCore as PLC

data PBool s = PTrue | PFalse

instance PlutusType PBool where
  type PInner PBool _ = POpaque
  pcon' PTrue = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
  pcon' PFalse = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool False
  pmatch' b f = pforce $ (punsafeBuiltin PLC.IfThenElse) £ b £ (pdelay $ f PTrue) £ (pdelay $ f PFalse)

class PEq t where
  (£==) :: Term s t -> Term s t -> Term s PBool

infix 4 £==

class POrd t where
  (£<=) :: Term s t -> Term s t -> Term s PBool
  (£<) :: Term s t -> Term s t -> Term s PBool

infix 4 £<=
infix 4 £<

pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif b case_true case_false = pmatch b $ \case
  PTrue -> case_true
  PFalse -> case_false
