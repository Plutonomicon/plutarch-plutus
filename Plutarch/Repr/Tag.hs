{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.Tag (
  PTag (..),
  DeriveAsTag (..),
  TagLiftHelper (..),
) where

import Data.Proxy (Proxy (Proxy))

-- parts have been commented. They will have to be restored after PLiftalbe rework
-- import Data.Coerce (coerce)
import Data.Kind (Type)

-- import GHC.Exts (Any)
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

-- import Plutarch.Builtin.Opaque (POpaque, popaque)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.PlutusType (
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
import Plutarch.Repr.Newtype (DeriveAsNewtype (DeriveAsNewtype))
import Plutarch.TermCont (pletC, unTermCont)

-- | @since WIP
newtype PTag (struct :: [S -> Type]) (s :: S) = PTag
  { unPTag :: Term s PInteger
  -- ^ @since WIP
  }
  deriving stock (GHC.Generic)

-- | @since WIP
instance SOP.Generic (PTag struct s)

-- | @since WIP
deriving via DeriveAsNewtype (PTag struct) instance PlutusType (PTag struct)

-- | @since WIP
newtype DeriveAsTag (a :: S -> Type) s = DeriveAsTag
  { unDeriveAsTag :: a s
  -- ^ @since WIP
  }

{- | This derives tag-only PlutusType automatically. Resulted instances will use `PInteger` as underlying type, making this much more efficient than using regular Data/Scott/SOP based encoding. As name suggests, types with no-argument constructors can use this.

Example:
@@
data PFoo s = A | B | C | D | E
  deriving stock (GHC.Generic, Show)
  deriving anyclass (PEq, PIsData)
  deriving (PlutusType, PLiftable)
    via DeriveAsTag PFoo

instance SOP.Generic (PFoo s)
@@

@since WIP
-}
instance (forall s. TagTypeConstraints s a struct) => PlutusType (DeriveAsTag a) where
  type PInner (DeriveAsTag a) = PInteger
  type PCovariant' (DeriveAsTag a) = (PCovariant' a)
  type PContravariant' (DeriveAsTag a) = (PContravariant' a)
  type PVariant' (DeriveAsTag a) = (PVariant' a)
  pcon' :: forall s. DeriveAsTag a s -> Term s (PInner (DeriveAsTag a))
  pcon' (DeriveAsTag x) =
    pconstant @PInteger $ toInteger $ SOP.hindex $ SOP.from x

  pmatch' :: forall s b. Term s (PInner (DeriveAsTag a)) -> (DeriveAsTag a s -> Term s b) -> Term s b
  pmatch' tag f = unTermCont $ do
    -- plet here because tag might be a big computation
    tag' <- pletC tag
    let
      g :: SOP I (Code (a s)) -> Term s b
      g = f . DeriveAsTag . SOP.to

      go :: forall x xs. IsEmpty x => TagMatchHandler s b xs -> TagMatchHandler s b (x ': xs)
      go (TagMatchHandler rest) =
        TagMatchHandler $ \idx handle ->
          K (idx, handle $ SOP $ Z Nil) :* rest (idx + 1) (\(SOP x) -> handle $ SOP $ S x)

      handlers' :: TagMatchHandler s b struct
      handlers' = SOP.cpara_SList (Proxy @IsEmpty) (TagMatchHandler $ \_ _ -> Nil) go

      handlers :: [(Integer, Term s b)]
      handlers = SOP.hcollapse $ unTagMatchHandler handlers' 0 g

    pure $ groupHandlers handlers tag'

-- | @since WIP
newtype TagLiftHelper r struct = TagLiftHelper
  { unTagLiftHelper :: Integer -> (SOP I struct -> r) -> r
  -- ^ @since WIP
  }

-- Helpers

class x ~ '[] => IsEmpty (x :: [k])

instance x ~ '[] => IsEmpty '[]

type family TagTypePrettyError' n (xs :: [[Type]]) :: Bool where
  TagTypePrettyError' n ('[] ': rest) = TagTypePrettyError' (n + 1) rest
  TagTypePrettyError' n (invalid ': _) =
    TypeError
      ( 'Text "DeriveAsTag only supports constructors without arguments. However, at constructor #"
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

-- instance
--   ( PlutusType (DeriveAsTag a)
--   , SOP.Generic (a Any)
--   , TagTypeConstraints Any a struct
--   ) =>
--   PLiftable (DeriveAsTag a)
--   where
--   type AsHaskell (DeriveAsTag a) = a Any
--   type PlutusRepr (DeriveAsTag a) = Integer
--   toPlutarchRepr = toInteger . hindex . from
--   toPlutarch = PLifted . popaque . pconstant @PInteger . toPlutarchRepr @(DeriveAsTag a)
--   fromPlutarchRepr idx =
--     let
--       go :: IsEmpty x => TagLiftHelper r xs -> TagLiftHelper r (x ': xs)
--       go (TagLiftHelper rest) =
--         TagLiftHelper $ \n f ->
--           if idx == n
--             then f $ SOP $ Z Nil
--             else rest (n + 1) \(SOP s) -> f $ SOP $ S s

--       helper :: TagLiftHelper (Maybe (a Any)) (Code (a Any))
--       helper = cpara_SList (Proxy @IsEmpty) (TagLiftHelper \_ _ -> Nothing) go
--      in
--       unTagLiftHelper helper 0 (Just <$> to)
--   fromPlutarch t = do
--     idx <- fromPlutarch @PInteger $ coerce t
--     case fromPlutarchRepr @(DeriveAsTag a) idx of
--       Nothing -> Left TagTypeInvalidIndex
--       Just x -> pure x
