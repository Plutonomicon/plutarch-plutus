module Plutarch.Maybe (
  -- * Type
  PMaybe (..),

  -- * Functions

  -- ** Introduction
  pjust,
  pnothing,

  -- ** Predicates
  pisJust,

  -- ** Eliminators
  pfromJust,
  ptraceIfNothing,
  pfromMaybe,
  pmaybe,
  passertPJust,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch (
  DPTStrat,
  DerivePlutusType,
  PlutusType,
  PlutusTypeScott,
  S,
  Term,
  pcon,
  phoistAcyclic,
  plam,
  pmatch,
  (#),
  type (:-->),
 )
import Plutarch.Bool (PBool, PEq)
import Plutarch.Lift (pconstant)
import Plutarch.Show (PShow)
import Plutarch.String (PString)
import Plutarch.Trace (ptraceInfoError)

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: S -> Type) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PMaybe a) where type DPTStrat _ = PlutusTypeScott

-- | Extracts the element out of a 'PJust' and throws an error if its argument is 'PNothing'.
pfromJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybe a :--> a)
pfromJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> ptraceInfoError "pfromJust: found PNothing"
    PJust x -> x

{- | Extracts the element out of a 'PJust' and throws a custom error if it's given a 'PNothing'.

@since WIP
-}
ptraceIfNothing ::
  forall (a :: S -> Type) (s :: S).
  -- | The custom error message.
  Term s PString ->
  Term s (PMaybe a) ->
  Term s a
ptraceIfNothing err t = pmatch t $ \case
  PNothing -> ptraceInfoError err
  PJust x -> x

{- | Yields true if the given 'PMaybe' value is of form @'PJust' _@.

@since WIP
-}
pisJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $
  plam $ \v' ->
    pmatch v' $ \case
      PJust _ -> pconstant True
      _ -> pconstant False

{- | Extract a 'PMaybe' by providing a default value in case of 'PJust'.

@since WIP
-}
pfromMaybe ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PMaybe a :--> a)
pfromMaybe = phoistAcyclic $
  plam $ \e a -> pmatch a $ \case
    PJust a' -> a'
    PNothing -> e

{- | Construct a 'PJust' value.

@since WIP
-}
pjust ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> PMaybe a)
pjust = phoistAcyclic $ plam $ pcon . PJust

{- | Construct a 'PNothing' value.

@since WIP
-}
pnothing ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybe a)
pnothing = phoistAcyclic $ pcon PNothing

{- | Given a default value, a function and a 'PMaybe' value, yields the default
value if the 'PMaybe' value is 'PNothing' and applies the function to the
value stored in the 'PJust' otherwise.

@since WIP
-}
pmaybe ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S).
  Term s (b :--> (a :--> b) :--> PMaybe a :--> b)
pmaybe = phoistAcyclic $
  plam $ \d f -> flip pmatch $ \case
    PJust v -> f # v
    _ -> d

{- | Extract the value stored in a 'PMaybe' container. If there's no value,
throw an error with the given message.

@since WIP
-}
passertPJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PString :--> PMaybe a :--> a)
passertPJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PJust v -> v
    _ -> ptraceInfoError emsg
