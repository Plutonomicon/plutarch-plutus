module Plutarch.Extra.Maybe (
  -- * Utility functions for working with 'PMaybe'
  pfromJust,
  ptraceIfNothing,
  pisJust,
  pmaybe,
  pfromMaybe,
  pjust,
  pnothing,

  -- * TermCont-based combinators
  pexpectJustC,

  -- * Assertions
  passertPJust,
) where

import Plutarch.Prelude

--------------------------------------------------------------------------------
-- Utility functions for working with 'PMaybe'.

-- | Extracts the element out of a 'PJust' and throws an error if its argument is 'PNothing'.
pfromJust ::
  forall (a :: PType) (s :: S).
  Term s (PMaybe a :--> a)
pfromJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> ptraceError "pfromJust: found PNothing"
    PJust x -> x

-- | Extracts the element out of a 'PJust' and throws a custom error if it's given a 'PNothing'.
ptraceIfNothing ::
  forall (a :: PType) (s :: S).
  -- | The custom error message.
  Term s PString ->
  Term s (PMaybe a) ->
  Term s a
ptraceIfNothing err t = pmatch t $ \case
  PNothing -> ptraceError err
  PJust x -> x

-- | Yields true if the given 'PMaybe' value is of form @'PJust' _@.
pisJust ::
  forall (a :: PType) (s :: S).
  Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $
  plam $ \v' ->
    pmatch v' $ \case
      PJust _ -> pconstant True
      _ -> pconstant False

-- | Extract a 'PMaybe' by providing a default value in case of 'PJust'.
pfromMaybe ::
  forall (a :: PType) (s :: S).
  Term s (a :--> PMaybe a :--> a)
pfromMaybe = phoistAcyclic $
  plam $ \e a -> pmatch a $ \case
    PJust a' -> a'
    PNothing -> e

-- | Construct a 'PJust' value.
pjust :: forall (a :: PType) (s :: S). Term s (a :--> PMaybe a)
pjust = phoistAcyclic $ plam $ pcon . PJust

-- | Construct a 'PNothing' value.
pnothing :: forall (a :: PType) (s :: S). Term s (PMaybe a)
pnothing = phoistAcyclic $ pcon PNothing

{- | Given a default value, a function and a 'PMaybe' value, yields the default
      value if the 'PMaybe' value is 'PNothing' and applies the function to the
      value stored in the 'PJust' otherwise.
-}
pmaybe ::
  forall (b :: PType) (a :: PType) (s :: S).
  Term s (b :--> (a :--> b) :--> PMaybe a :--> b)
pmaybe = phoistAcyclic $
  plam $ \d f -> flip pmatch $ \case
    PJust v -> f # v
    _ -> d

-- | Escape with a particular value on expecting 'Just'. For use in monadic context.
pexpectJustC ::
  forall (a :: PType) (r :: PType) (s :: S).
  Term s r ->
  Term s (PMaybe a) ->
  TermCont @r s (Term s a)
pexpectJustC escape ma = tcont $ \f ->
  pmatch ma $ \case
    PJust v -> f v
    PNothing -> escape

-- | Extract the value stored in a PMaybe container. If there's no value, throw an error with the given message.
passertPJust :: forall (a :: PType) (s :: S). Term s (PString :--> PMaybe a :--> a)
passertPJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PJust v -> v
    _ -> ptraceError emsg
