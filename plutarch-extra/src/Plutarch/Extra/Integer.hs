module Plutarch.Extra.Integer (
  podd,
  peven,
) where

import Plutarch.Prelude

podd :: Term s (PInteger :--> PBool)
podd = phoistAcyclic $ plam $ \n -> (pmod # n # 2) #== 1

peven :: Term s (PInteger :--> PBool)
peven = phoistAcyclic $ plam $ \n -> (pmod # n # 2) #== 0
