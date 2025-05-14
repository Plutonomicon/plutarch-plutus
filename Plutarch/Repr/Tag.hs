{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Force PlutusType constraint when using fake strategy for wrappers
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Repr.Tag (
  PTag (..),
  TagLiftHelper (..),
  DeriveTagPlutusType (..),
  DeriveTagPLiftable (..),
) where

import Data.Proxy (Proxy (Proxy))

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Exts (Any)
import GHC.Generics qualified as GHC
import GHC.TypeError (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeLits (type (+))
import Generics.SOP (
  All,
  Code,
  I,
  K (K),
  NP (Nil, (:*)),
  NS (S, Z),
  SOP (SOP),
 )
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Integer (PInteger)

import Plutarch.Builtin.Opaque (popaque)
import Plutarch.Internal.Lift (
  LiftError (OtherLiftError),
  PLiftable (AsHaskell, PlutusRepr, haskToRepr, plutToRepr, reprToHask, reprToPlut),
  PLifted (PLifted),
  pconstant,
 )
import Plutarch.Internal.PlutusType (
  DeriveFakePlutusType (DeriveFakePlutusType),
  PContravariant',
  PCovariant',
  PInner,
  PVariant',
  PlutusType,
  pcon',
  pmatch',
 )
import Plutarch.Internal.Term (S, Term)
import Plutarch.Repr.Internal (groupHandlers)
import Plutarch.Repr.Newtype (DeriveNewtypePlutusType (DeriveNewtypePlutusType))
import Plutarch.TermCont (pletC, unTermCont)

-- | @since 1.10.0
newtype PTag (struct :: [S -> Type]) (s :: S) = PTag
  { unPTag :: Term s PInteger
  -- ^ @since 1.10.0
  }
  deriving stock (GHC.Generic)

-- | @since 1.10.0
instance SOP.Generic (PTag struct s)

-- | @since 1.10.0
deriving via DeriveNewtypePlutusType (PTag struct) instance PlutusType (PTag struct)

-- | @since IWP
newtype DeriveTagPlutusType (a :: S -> Type) s = DeriveTagPlutusType
  { unDeriveTagPlutusType :: a s
  -- ^ @since WIP
  }

{- | This derives tag-only PlutusType automatically. Resulted instances will use `PInteger` as underlying type, making this much more efficient than using regular Data/Scott/SOP based encoding. As name suggests, types with no-argument constructors can use this.

Example:
@@
data PFoo s = A | B | C | D | E
  deriving stock (GHC.Generic, Show)
  deriving anyclass (PEq, PIsData)
  deriving (PlutusType) via DeriveTagPlutusType PFoo

instance SOP.Generic (PFoo s)
@@

@since 1.10.0
-}
instance (forall s. TagTypeConstraints s a struct) => PlutusType (DeriveTagPlutusType a) where
  type PInner (DeriveTagPlutusType a) = PInteger
  type PCovariant' (DeriveTagPlutusType a) = (PCovariant' a)
  type PContravariant' (DeriveTagPlutusType a) = (PContravariant' a)
  type PVariant' (DeriveTagPlutusType a) = (PVariant' a)
  pcon' :: forall s. DeriveTagPlutusType a s -> Term s (PInner (DeriveTagPlutusType a))
  pcon' (DeriveTagPlutusType x) =
    pconstant @PInteger $ toInteger $ SOP.hindex $ SOP.from x

  pmatch' :: forall s b. Term s (PInner (DeriveTagPlutusType a)) -> (DeriveTagPlutusType a s -> Term s b) -> Term s b
  pmatch' tag f = unTermCont $ do
    -- plet here because tag might be a big computation
    tag' <- pletC tag
    let
      g :: SOP I (Code (a s)) -> Term s b
      g = f . DeriveTagPlutusType . SOP.to

      go :: forall x xs. IsEmpty x => TagMatchHandler s b xs -> TagMatchHandler s b (x ': xs)
      go (TagMatchHandler rest) =
        TagMatchHandler $ \idx handle ->
          K (idx, handle $ SOP $ Z Nil) :* rest (idx + 1) (\(SOP x) -> handle $ SOP $ S x)

      handlers' :: TagMatchHandler s b struct
      handlers' = SOP.cpara_SList (Proxy @IsEmpty) (TagMatchHandler $ \_ _ -> Nil) go

      handlers :: [(Integer, Term s b)]
      handlers = SOP.hcollapse $ unTagMatchHandler handlers' 0 g

    pure $ groupHandlers handlers tag'

-- | @since 1.10.0
newtype TagLiftHelper r struct = TagLiftHelper
  { unTagLiftHelper :: Integer -> (SOP I struct -> r) -> r
  -- ^ @since 1.10.0
  }

-- Helpers

class x ~ '[] => IsEmpty (x :: [k])

instance x ~ '[] => IsEmpty '[]

type family TagTypePrettyError' n (xs :: [[Type]]) :: Bool where
  TagTypePrettyError' n ('[] ': rest) = TagTypePrettyError' (n + 1) rest
  TagTypePrettyError' n (invalid ': _) =
    TypeError
      ( 'Text "DeriveTagPlutusType only supports constructors without arguments. However, at constructor #"
          ':<>: 'ShowType n
          ':<>: 'Text ", I got:"
          ':$$: 'ShowType invalid
      )
  TagTypePrettyError' _ '[] = 'True

type TagTypePrettyError struct = TagTypePrettyError' 1 struct ~ 'True

class (SOP.Generic (a s), TagTypePrettyError (Code (a s)), Code (a s) ~ struct, All IsEmpty struct) => TagTypeConstraints s a struct | s a -> struct
instance (SOP.Generic (a s), TagTypePrettyError (Code (a s)), Code (a s) ~ struct, All IsEmpty struct) => TagTypeConstraints s a struct

newtype TagMatchHandler s b struct = TagMatchHandler
  { unTagMatchHandler :: Integer -> (SOP I struct -> Term s b) -> NP (K (Integer, Term s b)) struct
  }

-- | @since WIP
newtype DeriveTagPLiftable (a :: S -> Type) (h :: Type) s = DeriveTagPLiftable
  { unDeriveTagPLiftable :: a s
  -- ^ @since WIP
  }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic)
  deriving
    ( -- | @since WIP
      PlutusType
    )
    via (DeriveFakePlutusType (DeriveTagPLiftable a h))

instance
  ( PlutusType a
  , SOP.Generic h
  , TagTypeConstraints Any a (Code h)
  ) =>
  PLiftable (DeriveTagPLiftable a h)
  where
  type AsHaskell (DeriveTagPLiftable a h) = h
  type PlutusRepr (DeriveTagPLiftable a h) = Integer

  haskToRepr = toInteger . SOP.hindex . SOP.from

  reprToHask idx =
    let
      go :: IsEmpty x => TagLiftHelper r xs -> TagLiftHelper r (x ': xs)
      go (TagLiftHelper rest) =
        TagLiftHelper $ \n f ->
          if idx == n
            then f $ SOP $ Z Nil
            else rest (n + 1) \(SOP s) -> f $ SOP $ S s

      helper :: TagLiftHelper (Maybe h) (Code h)
      helper = SOP.cpara_SList (Proxy @IsEmpty) (TagLiftHelper \_ _ -> Nothing) go
     in
      maybe (Left (OtherLiftError "Invalid index")) Right $ unTagLiftHelper helper 0 (Just <$> SOP.to)

  -- NOTE: Do we need index boudns checking in these two?

  reprToPlut idx = PLifted $ popaque $ pconstant @PInteger idx

  plutToRepr p = plutToRepr @PInteger $ coerce p
