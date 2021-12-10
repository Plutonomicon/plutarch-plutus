module Plutarch.Bool (PBool(..), PEq(..), POrd(..), pIf) where

import Plutarch.Prelude
import Plutarch.Opaque (POpaque)
import Plutarch (PlutusType(PInner, pCon', pMatch'), Constant(CBool))
import qualified PlutusCore as PLC

data PBool = PTrue | PFalse

instance PlutusType PBool where
  type PInner PBool _ = POpaque
  pCon' PTrue = pUnsafeConstant . PLC.Some $ PLC.ValueOf CBool True
  pCon' PFalse = pUnsafeConstant . PLC.Some $ PLC.ValueOf CBool False
  pMatch' :: Term POpaque -> (PBool -> Term b) -> Term b
  pMatch' b f = pForce $ (pUnsafeBuiltin PLC.IfThenElse) £ b £ (pDelay $ f PTrue) £ (pDelay $ f PFalse)

class PEq t where
  (£==) :: Term t -> Term t -> Term PBool

infix 4 £==

class POrd t where
  (£<=) :: Term t -> Term t -> Term PBool
  (£<) :: Term t -> Term t -> Term PBool

infix 4 £<=
infix 4 £<

pIf :: Term PBool -> Term a -> Term a -> Term a
pIf b case_true case_false = pMatch b $ \case
  PTrue -> case_true
  PFalse -> case_false
