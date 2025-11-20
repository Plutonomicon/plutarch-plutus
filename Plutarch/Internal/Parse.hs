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
  PBuiltinList,
  PBuiltinPair (PBuiltinPair),
  PData,
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pasMap,
  pchooseListBuiltin,
  pheadBuiltin,
  ptailBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger, pconstantInteger)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.IsData (PIsData, pfromData)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.Numeric (PPositive)
import Plutarch.Internal.Ord ((#<), (#<=), (#>=))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType (PInner, pcon', pmatch'),
  pmatch,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  pdelay,
  perror,
  pforce,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Repr.Data (
  DeriveAsDataRec,
  DeriveAsDataStruct,
  PInnermostIsDataDataRepr,
 )
import Plutarch.Repr.Internal (UnTermRec, UnTermStruct)
import Plutarch.Repr.Tag (DeriveAsTag)
import Plutarch.Unsafe (punsafeCoerce)

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

@since wip
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
  pwithValidated opq x = pmatch (pasConstr # opq) $ \(PBuiltinPair i' _) ->
    plet i' $ \i ->
      pif
        (i #== pconstantInteger 0)
        x
        ( pif
            (i #== pconstantInteger 1)
            x
            perror
        )

{- | Checks that we have a @Constr@ with a second field of at least length 2.
Furthermore, checks that the first element validates as per @a@, while the
second element validates as per @b@. The @Constr@ tag is not checked at all.

@since 1.12.0
-}
instance (PValidateData a, PValidateData b) => PValidateData (PBuiltinPair (PAsData a) (PAsData b)) where
  pwithValidated opq x = pmatch (pasConstr # opq) $ \(PBuiltinPair _ p) ->
    plet (pheadBuiltin # p) $ \fstOne ->
      plet (pheadBuiltin #$ ptailBuiltin # p) $ \sndOne ->
        pwithValidated @a fstOne . pwithValidated @b sndOne $ x

{- | Checks that we have a @Constr@ with a second field of at least length 2.
The @Constr@ tag, or the elements, are not checked at all.

@since 1.12.0
-}
instance PValidateData (PBuiltinPair PData PData) where
  pwithValidated opq x = pmatch (pasConstr # opq) $ \(PBuiltinPair _ p) ->
    plet (pheadBuiltin # p) $ \_ ->
      plet (pheadBuiltin #$ ptailBuiltin # p) $ const x

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
      go self ell done = pforce $ pchooseListBuiltin # ell # pdelay done #$ pdelay $ plet (pheadBuiltin # ell) $ \h ->
        plet (ptailBuiltin # ell) $ \t ->
          self # t # pwithValidated @a h done

{- | Checks that we have a @Map@. Furthermore, checks that every key-value pair
validates as per @a@ and @b@. Takes precedence over the overlapping
@PValidateData (PBuiltinList a)@ instance.

@since wip
-}
instance {-# OVERLAPPING #-} (PValidateData a, PValidateData b) => PValidateData (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b))) where
  pwithValidated opq x = plet (pasMap # opq) $ \mp ->
    phoistAcyclic (pfix $ plam . go) # mp # x
    where
      go ::
        forall (r :: S -> Type) (s :: S).
        Term s (PBuiltinList (PBuiltinPair PData PData) :--> r :--> r) ->
        Term s (PBuiltinList (PBuiltinPair PData PData)) ->
        Term s r ->
        Term s r
      go self mp done = pforce $ pchooseListBuiltin # mp # pdelay done #$ pdelay $ pmatch (pheadBuiltin # mp) $ \(PBuiltinPair fst snd) ->
        plet (ptailBuiltin # mp) $ \t ->
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
  pwithValidated opq x = plet (pasInt # opq) $ \i ->
    let len = SOP.lengthSList @_ @(SOP.Code (a Any)) Proxy
     in plet (pconstant @PInteger . fromIntegral $ len) $ \plen ->
          pif
            (i #< 0)
            perror
            ( pif
                (i #>= plen)
                perror
                x
            )

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
    case SOP.shape @(S -> Type) @struct of
      SOP.ShapeNil -> x
      SOP.ShapeCons @ys @y restShape -> go @y ell restShape x
    where
      go ::
        forall (a :: S -> Type) (w :: [S -> Type]) (s :: S) (r :: S -> Type).
        (PValidateData a, SOP.All PValidateData w) =>
        Term s (PBuiltinList PData) ->
        SOP.Shape w ->
        Term s r ->
        Term s r
      go ell aShape =
        pwithValidated @a (pheadBuiltin # ell) . case aShape of
          -- We don't have any more elements of `ell` we care about, so nothing
          -- else to do.
          SOP.ShapeNil -> id
          SOP.ShapeCons @ys @y restShape -> go @y (plet (ptailBuiltin # ell) id) restShape

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
    pmatch p $ \(PBuiltinPair ix' fields') ->
      plet ix' $ \ix ->
        plet fields' $ \fields ->
          case SOP.shape @[S -> Type] @struct of
            outerShape ->
              let numArms = SOP.lengthSList @[S -> Type] (Proxy @struct)
                  possibleMatches = pconstant @PInteger . fromIntegral <$> [0, 1 .. numArms - 1]
               in goOuter ix fields outerShape possibleMatches x
    where
      goOuter ::
        forall (wOuter :: [[S -> Type]]) (s :: S) (r :: S -> Type).
        (SOP.SListI2 wOuter, SOP.All2 PValidateData wOuter) =>
        Term s PInteger ->
        Term s (PBuiltinList PData) ->
        SOP.Shape wOuter ->
        [Term s PInteger] ->
        Term s r ->
        Term s r
      goOuter ix ell outerShape = \case
        -- We tried everything, and nothing matched the index.
        [] -> const perror
        (i : is) -> case outerShape of
          -- Technically impossible
          SOP.ShapeNil -> const perror
          SOP.ShapeCons @ys @y restShape -> \arg ->
            pif
              (ix #== i)
              ( case SOP.shape @(S -> Type) @y of
                  SOP.ShapeNil -> arg
                  SOP.ShapeCons @zs @z restInnerShape -> goInner @z ell restInnerShape arg
              )
              (goOuter ix ell restShape is arg)
      goInner ::
        forall (a :: S -> Type) (wInner :: [S -> Type]) (s :: S) (r :: S -> Type).
        (PValidateData a, SOP.All PValidateData wInner) =>
        Term s (PBuiltinList PData) ->
        SOP.Shape wInner ->
        Term s r ->
        Term s r
      goInner ell aShape =
        pwithValidated @a (pheadBuiltin # ell) . case aShape of
          SOP.ShapeNil -> id
          SOP.ShapeCons @ys @y restShape -> goInner @y (plet (ptailBuiltin # ell) id) restShape

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
