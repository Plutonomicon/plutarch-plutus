module Plutarch.Bool (PBool (..), PEq (..), POrd (..), pif, pif', pnot, (#&&), (#||)) where

import Plutarch (PlutusType (PInner, pcon', pmatch'), punsafeBuiltin, punsafeConstant)
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PBool s = PTrue | PFalse

instance PlutusType PBool where
  type PInner PBool _ = PBool
  pcon' PTrue = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
  pcon' PFalse = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool False
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool

infix 4 #==

class POrd t where
  (#<=) :: Term s t -> Term s t -> Term s PBool
  (#<) :: Term s t -> Term s t -> Term s PBool

infix 4 #<=
infix 4 #<

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

-- | Boolean negation for 'PBool' terms.
pnot :: Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x -> pif x (pcon PFalse) $ pcon PTrue

-- | Lazily evaluated boolean and for 'PBool' terms.
infixr 3 #&&

(#&&) :: Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pif x (pif y (pcon PTrue) $ pcon PFalse) $ pcon PFalse

-- | Lazily evaluated boolean or for 'PBool' terms.
infixr 2 #||

(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pif x (pcon PTrue) $ pif y (pcon PTrue) $ pcon PFalse
