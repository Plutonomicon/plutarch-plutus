module Plutarch.BuiltinHList (PBuiltinHList (..), phhead, phtail) where

import Plutarch (punsafeBuiltin)
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PBuiltinHList (as :: [k -> Type]) (s :: k) where
  PBuiltinHNil :: PBuiltinHList '[] s
  PBuiltinHCons :: Term s a -> Term s (PBuiltinHList as) -> PBuiltinHList (a : as) s

{-
instance PlutusType (PBuiltinHList as) where
  type PInner (PBuiltinHList _) _ = POpaque

  -- FIXME: Are built-in lists heterogeneous or homogeneous?
  pcon' PBuiltinHNil = punsafeConstant . PLC.Some $ PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniData) []
  pcon' (PBuiltinHCons x xs) = (phoistAcyclic $ pforce $ punsafeBuiltin PLC.MkCons) £ x £ xs

  -- FIXME: we shouldn't actually use this because it's an HList
  pmatch' l f =
    pforce $
      plet l $ \l ->
        (pforce . pforce $ punsafeBuiltin PLC.ChooseList)
          £ l
          -- FIXME: Can we not use unsafeCoerce here? Very ugly.
          £ (pdelay $ f (unsafeCoerce PBuiltinHNil)) -- nil case
          £ ( pdelay $ -- cons case
                f $
                  unsafeCoerce $
                    PBuiltinHCons
                      ((phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList) £ l)
                      ((phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList) £ l)
            )
-}

phhead :: Term s (PBuiltinHList (a : as) :--> a)
phhead = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

phtail :: Term s (PBuiltinHList (a : as) :--> PBuiltinHList as)
phtail = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList
