module Plutarch.Extra.Maybe (
  pfromJust,
  maybeToRight,
) where

import Plutarch.Prelude

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
