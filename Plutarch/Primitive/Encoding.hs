module Plutarch.Primitive.Encoding (
  Encoding (..),
  PEncodingRep,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Data (PAsData)

-- | @since wip
data Encoding
  = SOPE
  | DataPlutusE
  | DataListE
  | EnumE
  | MSE
  deriving stock
    ( -- | @since wip
      Eq
    , -- | @since wip
      Show
    )

-- | @since wip
type family PEncodingRep (enc :: Encoding) (s :: S) (a :: S -> Type) :: Type where
  PEncodingRep 'SOPE s a = Term s a
  PEncodingRep 'DataPlutusE s a = Term s (PAsData a)
  PEncodingRep 'DataListE s a = Term s (PAsData a)
  PEncodingRep 'EnumE s a = Term s a
  PEncodingRep 'MSE s a = Term s a
