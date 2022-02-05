module Plutarch.Api.V1.Tuple (PTuple, ptuple) where

import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (PLabeledType ((:=)))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import qualified PlutusCore.Default as PLC

type PTuple a b =
  PDataSum
    '[ '[ "_0" ':= a
        , "_1" ':= b
        ]
     ]

ptuple :: Term s (PAsData a :--> PAsData b :--> PTuple a b)
ptuple = phoistAcyclic $
  plam $ \x y ->
    let target :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
        target = pconstrBuiltin # 0 #$ pcons # pforgetData x #$ pcons # pforgetData y # pnil
     in punsafeCoerce target

pconstrBuiltin :: Term s (PInteger :--> PBuiltinList PData :--> PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
pconstrBuiltin = punsafeBuiltin $ PLC.MkCons
