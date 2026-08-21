{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoPartialTypeSignatures #-}
-- Note (Koz, 25/08/2025): Needed to ensure that `pparseData` doesn't get used
-- on a type that doesn't have a sensible `PAsData`.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Internal.Parse (
  -- * Type class
  PValidateData (..),

  -- * Function
  pparseData,

  -- * Helper deriving newtype
  Don'tValidate (..),
  DeriveNewtypePValidateData (..),
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Any)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool, pif)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList (PCons, PNil),
  PBuiltinPair (PBuiltinPair),
  PData,
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pasMap,
  pheadBuiltin,
  pheadTailBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque, popaque)
import Plutarch.Internal.Case (punsafeCase)
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.IsData (PIsData, pfromData)
import Plutarch.Internal.Numeric (PPositive)
import Plutarch.Internal.Ord ((#<=))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType (PInner, pcon', pmatch'),
  pmatch,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  plet,
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Repr.Data (
  DeriveAsDataRec,
  DeriveAsDataStruct,
  PInnermostIsDataDataRepr,
 )
import Plutarch.Repr.Internal (UnTermRec, UnTermStruct)
import Plutarch.Repr.Tag (DeriveAsTag)

{- | Describes a @Data@ encoded Plutarch type that requires some additional
validation to ensure its structure is indeed what we expect. This is
especially useful for datums or other user-supplied arguments, since these
can be malformed.

= Why the CPS

'pwithValidated' is written in continuation-passing style (or CPS) for
reasons of efficiency. As 'pwithValidated' is meant to check structure (and
nothing more), our first instinct would be to write something like

@pwithValidated :: Term s PData -> Term s PBool@

or

@pwithValidated :: Term s PData -> Term s PUnit@

and rely on 'perror' to sort things out. However, constructing either 'PUnit'
or 'PBool' isn't free, and ultimately, this value ends up unused. At the same
time, we want to ensure that the validation specified in 'pwithValidated' is
actually performed, which neither of the above signatures can promise
anything about.

CPS solves both of these problems. Since the result of 'pwithValidated' is
technically a function that /must/ behave the same no matter what type of @r@
it operates over, we can't do anything except potentially mess with the
argument 'PData' or error out, which means we don't need to allocate any
\'result value\'. Furthermore, by working in CPS, we ensure that any
validation defined in 'pwithValidated' must happen, even if the 'PData' (or
whatever it's supposed to be) is never handled or forced.

= Important note

It is essential practice to document what /exactly/ any given instance of
'PValidateData' checks. Each instance should specify this: all the instances
provided by Plutarch and its related libraries follow this rule.

@since 1.12.0
-}
class PValidateData (a :: S -> Type) where
  pwithValidated ::
    forall (s :: S).
    Term s PData ->
    (forall (r :: S -> Type). Term s r -> Term s r)

{- | Given a 'PData', check that it is, indeed, structured as @a@ expects. If it
is, return that same 'PData' \'rewrapped\' into 'PAsData' @a@.

This helper exists to avoid having to work in CPS when writing regular code.
It is kept out of 'PValidateData' for efficiency and safety reasons.

@since 1.12.0
-}
pparseData ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, PValidateData a) =>
  Term s PData ->
  Term s (PAsData a)
pparseData opq = pwithValidated @a opq . punsafeCoerce $ opq

{- | Checks (and does) nothing.

@since 1.12.0
-}
deriving via (Don'tValidate PData) instance PValidateData PData

{- | Checks that we have an @I@.

@since 1.12.0
-}
instance PValidateData PInteger where
  pwithValidated opq = plet (pasInt # opq) . const

{- | Checks that we have a positive @I@.

@since 1.13.0
-}
instance PValidateData PPositive where
  pwithValidated opq x =
    plet (pfromData $ pparseData @PInteger opq) $ \n ->
      pif (n #<= 0) perror x

{- | Checks that we have a @B@.

@since 1.12.0
-}
instance PValidateData PByteString where
  pwithValidated opq = plet (pasByteStr # opq) . const

{- | Checks that we have a @Constr@ with either @0@ or @1@ as its tag. The
second field of @Constr@ is not checked at all.

@since 1.12.0
-}
instance PValidateData PBool where
  -- Note (Koz, 24/11/2025): This slightly weird implementation relies on `Case`
  -- over `Integer` treating the first 'arm' of the match as `0`, the second as
  -- `1`, and so on. Since we error on anything other than those two, we can use
  -- this for speed.
  pwithValidated opq x =
    punsafeCase
      (pmatch (pasConstr # opq) $ \(PBuiltinPair i _) -> i)
      [ popaque x
      , popaque x
      ]

{- | Checks that we have a @Constr@ with a second field of at least length 2.
Furthermore, checks that the first element validates as per @a@, while the
second element validates as per @b@. The @Constr@ tag is not checked at all.

@since 1.12.0
-}
instance (PValidateData a, PValidateData b) => PValidateData (PBuiltinPair (PAsData a) (PAsData b)) where
  pwithValidated opq x = pmatch (pasConstr # opq) $ \(PBuiltinPair _ p) ->
    pheadTailBuiltin p $ \fstOne rest ->
      plet (pheadBuiltin # rest) $ \sndOne ->
        pwithValidated @a fstOne . pwithValidated @b sndOne $ x

{- | Checks that we have a @Constr@ with a second field of at least length 2.
The @Constr@ tag, or the elements, are not checked at all.

@since 1.12.0
-}
instance PValidateData (PBuiltinPair PData PData) where
  pwithValidated opq x = pmatch (pasConstr # opq) $ \(PBuiltinPair _ p) ->
    pheadTailBuiltin p $ \_ rest ->
      plet (pheadBuiltin # rest) $ const x

{- | Checks that we have a @List@. Furthermore, checks that every element
validates as per @a@.

@since 1.12.0
-}
instance {-# OVERLAPPABLE #-} PValidateData a => PValidateData (PBuiltinList a) where
  pwithValidated opq x = plet (pasList # opq) $ \ell ->
    phoistAcyclic (pfix $ plam . go) # ell # x
    where
      go ::
        forall (r :: S -> Type) (s :: S).
        Term s (PBuiltinList PData :--> r :--> r) ->
        Term s (PBuiltinList PData) ->
        Term s r ->
        Term s r
      go self ell done = pmatch ell $ \case
        PNil -> done
        PCons h t -> self # t # pwithValidated @a h done

{- | Checks that we have a @Map@. Furthermore, checks that every key-value pair
validates as per @a@ and @b@. Takes precedence over the overlapping
@PValidateData (PBuiltinList a)@ instance.

@since 1.13.0
-}
instance
  {-# OVERLAPPING #-}
  (PValidateData a, PValidateData b) =>
  PValidateData (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b)))
  where
  pwithValidated opq x = plet (pasMap # opq) $ \mp ->
    phoistAcyclic (pfix $ plam . go) # mp # x
    where
      go ::
        forall (r :: S -> Type) (s :: S).
        Term s (PBuiltinList (PBuiltinPair PData PData) :--> r :--> r) ->
        Term s (PBuiltinList (PBuiltinPair PData PData)) ->
        Term s r ->
        Term s r
      go self mp done = pmatch mp $ \case
        PNil -> done
        PCons h t ->
          pmatch h $ \(PBuiltinPair fst snd) ->
            self # t # (pwithValidated @a fst . pwithValidated @b snd $ done)

{- | Checks that we have a @List@.

@since 1.12.0
-}
instance PValidateData (PBuiltinList PData) where
  pwithValidated opq x = plet (pasList # opq) $ const x

-- | @since 1.12.0
instance PValidateData a => PValidateData (PAsData a) where
  pwithValidated = pwithValidated @a

{- | Checks that we have an @I@, and that it is in the range @[0, n - 1]@, where
@n@ is the number of \'arms\' in the encoded sum type.

@since 1.12.0
-}
instance SOP.Generic (a Any) => PValidateData (DeriveAsTag a) where
  {-# INLINEABLE pwithValidated #-}
  pwithValidated opq x =
    let len = SOP.lengthSList @_ @(SOP.Code (a Any)) Proxy
     in punsafeCase (pasInt # opq) . replicate len . popaque $ x

{- | Checks that we have a @List@, that it has (at least) enough elements for
each field of @a@, and that each of those elements, in order, validates as
per its respective 'PValidateData' instance.

@since 1.12.0
-}
instance
  forall (a :: S -> Type) (struct' :: [Type]) (struct :: [S -> Type]).
  ( SOP.Generic (a Any)
  , SOP.All PInnermostIsDataDataRepr struct
  , struct ~ UnTermRec struct'
  , SOP.Generic (a Any)
  , '[struct'] ~ SOP.Code (a Any)
  , SOP.All PValidateData struct
  , SOP.SListI struct
  ) =>
  PValidateData (DeriveAsDataRec a)
  where
  {-# INLINEABLE pwithValidated #-}
  pwithValidated opq x = plet (pasList # opq) $ \ell ->
    go ell (SOP.shape @(S -> Type) @struct) x
    where
      go ::
        forall (w :: [S -> Type]) (s :: S) (r :: S -> Type).
        SOP.All PValidateData w =>
        Term s (PBuiltinList PData) ->
        SOP.Shape w ->
        Term s r ->
        Term s r
      go ell aShape x = case aShape of
        SOP.ShapeNil -> x
        SOP.ShapeCons @_ @y SOP.ShapeNil -> pwithValidated @y (pheadBuiltin # ell) x
        SOP.ShapeCons @_ @y restShape -> pheadTailBuiltin ell $ \h t ->
          pwithValidated @y h $ go t restShape x

{- | Checks that we have a @Constr@, that its tag is in the range @[0, n - 1]@
(where @n@ is the number of \'arms\' in the encoded sum type), and that there
are at least enough fields in the second @Constr@ argument, each of which
decodes as per that field's 'PValidateData' instance.

@since 1.12.0
-}
instance
  forall (a :: S -> Type) (struct :: [[S -> Type]]).
  ( SOP.Generic (a Any)
  , struct ~ UnTermStruct (a Any)
  , SOP.All2 PInnermostIsDataDataRepr struct
  , SOP.All2 PValidateData struct
  , SOP.SListI2 struct
  ) =>
  PValidateData (DeriveAsDataStruct a)
  where
  {-# INLINEABLE pwithValidated #-}
  pwithValidated opq x = plet (pasConstr # opq) $ \p ->
    pmatch p $ \(PBuiltinPair ix fields) ->
      punsafeCase ix $ goOuter fields (SOP.shape @[S -> Type] @struct) x
    where
      goOuter ::
        forall (wOuter :: [[S -> Type]]) (s :: S) (r :: S -> Type).
        (SOP.SListI2 wOuter, SOP.All2 PValidateData wOuter) =>
        Term s (PBuiltinList PData) ->
        SOP.Shape wOuter ->
        Term s r ->
        [Term s POpaque]
      goOuter ell outerShape x = case outerShape of
        SOP.ShapeNil -> []
        SOP.ShapeCons @_ @y restShape -> popaque (goInner ell (SOP.shape @_ @y) x) : goOuter ell restShape x
      goInner ::
        forall (wInner :: [S -> Type]) (s :: S) (r :: S -> Type).
        SOP.All PValidateData wInner =>
        Term s (PBuiltinList PData) ->
        SOP.Shape wInner ->
        Term s r ->
        Term s r
      goInner ell aShape x = case aShape of
        SOP.ShapeNil -> x
        SOP.ShapeCons @_ @y SOP.ShapeNil -> pwithValidated @y (pheadBuiltin # ell) x
        SOP.ShapeCons @_ @y restShape -> pheadTailBuiltin ell $ \h t ->
          pwithValidated @y h $ goInner t restShape x

{- | Helper to define a do-nothing instance of 'PValidateData'. Useful when
defining an instance for a complex type where we want to validate some parts,
but not others.

@since 1.12.0
-}
newtype Don'tValidate (a :: S -> Type) (s :: S) = Don'tValidate {unDon'tValidate :: a s}

-- | @since 1.12.0
instance PlutusType a => PlutusType (Don'tValidate a) where
  type PInner (Don'tValidate a) = PInner a
  pcon' (Don'tValidate x) = pcon' x
  pmatch' x f = pmatch' x (f . Don'tValidate)

-- | @since 1.12.0
instance PValidateData (Don'tValidate a) where
  pwithValidated _ = id

{- | Helper to define an instance of 'PValidateData' for @newtype@s over
'Term's, which \'borrows\' the 'PValidateData' instance for whatever the
@newtype@ is wrapping.

@since 1.12.0
-}
newtype DeriveNewtypePValidateData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = DeriveNewtypePValidateData (a s)

-- | @since 1.12.0
instance PValidateData b => PValidateData (DeriveNewtypePValidateData a b) where
  pwithValidated = pwithValidated @b
