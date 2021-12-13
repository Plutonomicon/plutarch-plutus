module Plutarch.Bool (PBool(..), PEq(..), POrd(..), pIf) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pCon', pMatch'), Constant(CBool), pUnsafeConstant, pUnsafeBuiltin, POpaque)
import qualified PlutusCore as PLC

data PBool s = PTrue | PFalse

instance PlutusType PBool where
  type PInner PBool _ = POpaque
  pCon' PTrue = pUnsafeConstant . PLC.Some $ PLC.ValueOf CBool True
  pCon' PFalse = pUnsafeConstant . PLC.Some $ PLC.ValueOf CBool False
  pMatch' b f = pForce $ (pUnsafeBuiltin PLC.IfThenElse) £ b £ (pDelay $ f PTrue) £ (pDelay $ f PFalse)

class PEq t where
  (£==) :: Term s t -> Term s t -> Term s PBool

infix 4 £==

class POrd t where
  (£<=) :: Term s t -> Term s t -> Term s PBool
  (£<) :: Term s t -> Term s t -> Term s PBool

infix 4 £<=
infix 4 £<

pIf :: Term s PBool -> Term s a -> Term s a -> Term s a
pIf b case_true case_false = pMatch b $ \case
  PTrue -> case_true
  PFalse -> case_false
