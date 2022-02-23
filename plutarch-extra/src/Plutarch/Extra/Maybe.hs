{-# OPTIONS_GHC -Wno-orphans #-}

-- Orphans aren't great but
-- this should be merged to plutarch eventually anyway

module Plutarch.Extra.Maybe (
  pfromJust,
  maybeToRight,
) where

import Plutarch.Prelude

import Plutarch.Builtin (
  pasConstr,
  pforgetData,
 )
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)

import qualified PlutusCore as PLC

-- TODO
-- Once we move to newer versions of plutarch
-- this should instead go through the API PMaybe
instance PIsData a => PIsData (PMaybe a) where
  pfromData mad' =
    phoistAcyclic
      ( plam $ \mad ->
          plet (pasConstr # pforgetData mad) $ \pair ->
            pif
              (pfstBuiltin # pair #== 0)
              (pcon PNothing)
              ( pcon $
                  PJust
                    ( pfromData
                        ( (punsafeCoerce :: Term s PData -> Term s (PAsData a)) $
                            phead #$ psndBuiltin # pair
                        )
                    )
              )
      )
      # mad'

  pdata ma' =
    phoistAcyclic
      ( plam $ \ma -> pmatch ma $ \case
          PJust a ->
            (punsafeCoerce :: Term s PData -> Term s (PAsData (PMaybe a))) $
              pconstrData # 1 #$ psingleton # punsafeCoerce (pdata a)
          PNothing ->
            (punsafeCoerce :: Term s PData -> Term s (PAsData (PMaybe a))) $
              pconstrData # 0 # pnil
      )
      # ma'

instance POrd a => POrd (PMaybe a) where
  ma' #< mb' =
    phoistAcyclic
      ( plam $ \ma mb ->
          pmatch ma $ \case
            PNothing -> pmatch mb $ \case
              PNothing -> pcon PFalse
              PJust _ -> pcon PTrue
            PJust a -> pmatch mb $ \case
              PNothing -> pcon PFalse
              PJust b -> a #< b
      )
      # ma'
      # mb'

  ma' #<= mb' =
    phoistAcyclic
      ( plam $ \ma mb ->
          pmatch ma $ \case
            PNothing -> pcon PTrue
            PJust a -> pmatch mb $ \case
              PNothing -> pcon PFalse
              PJust b -> a #<= b
      )
      # ma'
      # mb'

-- Lifted from the plutarch Guide
-- seemingly not in current plutarch
pconstrData :: Term s (PInteger :--> PBuiltinList PData :--> PData)
pconstrData = punsafeBuiltin PLC.ConstrData

pfromJust :: Term s (PMaybe a :--> a)
pfromJust = phoistAcyclic $
  plam $ \x -> pmatch x $ \case
    PJust x' -> x'
    PNothing -> perror

maybeToRight :: Term s (b :--> PMaybe a :--> PEither b a)
maybeToRight = phoistAcyclic $
  plam $ \b ma ->
    pmatch ma $ \case
      PJust a -> pcon $ PRight a
      PNothing -> pcon $ PLeft b
