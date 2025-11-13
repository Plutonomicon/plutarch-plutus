{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.Tag (
  PTag (..),
  DeriveAsTag (..),
  TagLiftHelper (..),
) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
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
  DeriveNewtypePlutusType (DeriveNewtypePlutusType),
  PlutusType (PInner, pcon', pmatch'),
 )
import Plutarch.Internal.Term (
  RawTerm (RCase),
  S,
  Term (Term),
  TermResult (TermResult),
  asRawTerm,
 )

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

-- | @since 1.10.0
newtype DeriveAsTag (a :: S -> Type) s = DeriveAsTag
  { unDeriveAsTag :: a s
  -- ^ @since 1.10.0
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

@since 1.10.0
-}
instance (forall (s :: S). TagTypeConstraints s a struct) => PlutusType (DeriveAsTag a) where
  type PInner (DeriveAsTag a) = PInteger
  pcon' :: forall (s :: S). DeriveAsTag a s -> Term s PInteger
  pcon' (DeriveAsTag x) = pconstant . toInteger . SOP.hindex . SOP.from $ x
  pmatch' :: forall (s :: S) (b :: S -> Type). Term s PInteger -> (DeriveAsTag a s -> Term s b) -> Term s b
  pmatch' tag f = Term $ \level -> do
    TermResult rawTag depsTag <- asRawTerm tag level
    rawHandlerTRs <- traverse (`asRawTerm` level) handlers
    let (rawHandlers, depsHandlers) = unzip . fmap (\(TermResult x y) -> (x, y)) $ rawHandlerTRs
    let allDeps = depsTag <> mconcat depsHandlers
    pure . TermResult (RCase rawTag rawHandlers) $ allDeps
    where
      handlers :: [Term s b]
      handlers = SOP.hcollapse . unTagMatchHandler handlers' $ toHandler
      toHandler :: SOP I (Code (a s)) -> Term s b
      toHandler = f . DeriveAsTag . SOP.to
      handlers' :: TagMatchHandler s b struct
      handlers' = SOP.cpara_SList (Proxy @IsEmpty) (TagMatchHandler (const Nil)) go
      go :: forall x xs. IsEmpty x => TagMatchHandler s b xs -> TagMatchHandler s b (x ': xs)
      go (TagMatchHandler rest) = TagMatchHandler $ \handle ->
        K (handle . SOP $ Z Nil) :* rest (\(SOP x) -> handle . SOP $ S x)

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
      ( 'Text "DeriveAsTag only supports constructors without arguments. However, at constructor #"
          ':<>: 'ShowType n
          ':<>: 'Text ", I got:"
          ':$$: 'ShowType invalid
      )
  TagTypePrettyError' _ '[] = 'True

type TagTypePrettyError struct = TagTypePrettyError' 1 struct ~ 'True

class (SOP.Generic (a s), TagTypePrettyError (Code (a s)), Code (a s) ~ struct, All IsEmpty struct) => TagTypeConstraints (s :: S) (a :: S -> Type) (struct :: [[Type]]) | s a -> struct
instance (SOP.Generic (a s), TagTypePrettyError (Code (a s)), Code (a s) ~ struct, All IsEmpty struct) => TagTypeConstraints s a struct

newtype TagMatchHandler s b struct = TagMatchHandler
  { unTagMatchHandler :: (SOP I struct -> Term s b) -> NP (K (Term s b)) struct
  }

instance
  ( PlutusType (DeriveAsTag a)
  , SOP.Generic (a Any)
  , TagTypeConstraints Any a struct
  ) =>
  PLiftable (DeriveAsTag a)
  where
  type AsHaskell (DeriveAsTag a) = a Any
  type PlutusRepr (DeriveAsTag a) = Integer

  haskToRepr = toInteger . SOP.hindex . SOP.from

  reprToHask idx =
    let
      go :: IsEmpty x => TagLiftHelper r xs -> TagLiftHelper r (x ': xs)
      go (TagLiftHelper rest) =
        TagLiftHelper $ \n f ->
          if idx == n
            then f $ SOP $ Z Nil
            else rest (n + 1) \(SOP s) -> f $ SOP $ S s

      helper :: TagLiftHelper (Maybe (a Any)) (Code (a Any))
      helper = SOP.cpara_SList (Proxy @IsEmpty) (TagLiftHelper \_ _ -> Nothing) go
     in
      maybe (Left (OtherLiftError "Invalid index")) Right $ unTagLiftHelper helper 0 (Just <$> SOP.to)

  -- NOTE: Do we need index bounds checking in these two?

  reprToPlut idx = PLifted $ popaque $ pconstant @PInteger idx

  plutToRepr p = plutToRepr @PInteger $ coerce p
