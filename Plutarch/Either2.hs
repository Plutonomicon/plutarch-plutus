{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# Options_GHC -w #-}

module Plutarch.Either2 where

import Data.Kind 
import Plutarch.Core
import Plutarch.PType

type  PEitherC :: PDSLKind -> (PType -> PType -> PType) -> Constraint
class PEitherC edsl pEither where
  pLeft 
    :: -- PHasRepr a 
    -- => PHasRepr b 
    -- => 
       Term edsl a 
    -> Term edsl (pEither a b)
  pRight 
    :: -- PHasRepr a 
    -- => PHasRepr b 
    -- => 
       Term edsl b 
    -> Term edsl (pEither a b)

  pEither 
    :: Term edsl (a #-> res) 
    -> Term edsl (b #-> res)
    -> Term edsl (pEither a b #-> res)

-- instance 
--   (forall a b. IsPType edsl a => IsPType edsl b => PConstructable' edsl (PEither a b))
--   => 

-- -- (forall a b. (IsPType edsl a, IsPType edsl b) => PConstructable edsl (PEither a b), IsPTypeBackend edsl (PEither a b)) => 
--   PEitherC edsl PEither where

--   pLeft :: forall a b. Term edsl a -> Term edsl (PEither a b)
--   pLeft (Term as) = pcon (PLeft as) where

--     u a = pcon (PLeft a)

-- import GHC.Generics (Generic)
-- import Plutarch (
--   DPTStrat,
--   DerivePlutusType,
--   PType,
--   PlutusType,
--   PlutusTypeScott,
--   S,
--   Term,
--  )
-- import Plutarch.Bool (PEq)
-- import Plutarch.Show (PShow)

-- data PEither (a :: PType) (b :: PType) (s :: S)
--   = PLeft (Term s a)
--   | PRight (Term s b)
--   deriving stock (Generic)
--   deriving anyclass (PlutusType, PEq, PShow)
-- instance DerivePlutusType (PEither a b) where type DPTStrat _ = PlutusTypeScott
