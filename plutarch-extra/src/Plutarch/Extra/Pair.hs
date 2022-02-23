module Plutarch.Extra.Pair (
  pfst,
  psnd,
  pfirst,
  psecond,
  pairFromBuiltin,
  pairToBuiltin,
) where

import Plutarch.Prelude

import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.Monadic (tmatch)

pairToBuiltin :: (PIsData a, PIsData b) => Term s (PPair a b :--> PBuiltinPair (PAsData a) (PAsData b))
pairToBuiltin = phoistAcyclic $
  plam $ \p -> unTermCont $ do
    PPair a b <- tmatch p
    pure $ ppairDataBuiltin # pdata a # pdata b

pairFromBuiltin :: (PIsData a, PIsData b) => Term s (PBuiltinPair (PAsData a) (PAsData b) :--> PPair a b)
pairFromBuiltin = phoistAcyclic $ plam $ \p -> pcon $ PPair (pfromData $ pfstBuiltin # p) (pfromData $ psndBuiltin # p)

pfst :: Term s (PPair a b :--> a)
pfst = phoistAcyclic $ plam $ \p -> pmatch p $ \(PPair a _) -> a

psnd :: Term s (PPair a b :--> b)
psnd = phoistAcyclic $ plam $ \p -> pmatch p $ \(PPair _ b) -> b

pfirst :: Term s ((a :--> a') :--> PPair a b :--> PPair a' b)
pfirst = plam $ \f p -> unTermCont $ do
  PPair a b <- tmatch p
  pure $ pcon $ PPair (f # a) b

psecond :: Term s ((b :--> b') :--> PPair a b :--> PPair a b')
psecond = plam $ \f p -> (`runTermCont` id) $ do
  PPair a b <- tmatch p
  pure $ pcon $ PPair a (f # b)
