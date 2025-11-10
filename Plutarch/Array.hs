{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoPartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | @since 1.12.0
module Plutarch.Array (
  -- * Type
  PPullArray,

  -- * Functions

  -- ** Introduction
  piota,
  pgenerate,
  pfromArray,
  pfromList,

  -- ** Transformation

  -- *** Linear
  pmapArray,
  pimapArray,

  -- *** Affine
  ptakeArray,
  pdropArray,

  -- ** Combination
  pzipWithArray,
  pizipWithArray,

  -- ** Elimination

  -- *** Folds
  pfoldlArray,

  -- *** Conversions
  ppullArrayToList,
  ppullArrayToSOPList,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Array (
  PArray,
  pindexArray,
  plengthOfArray,
  plistToArray,
 )
import Plutarch.Builtin.Bool (pif)
import Plutarch.Builtin.Data (PBuiltinList (PNil), pconsBuiltin)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.Numeric (
  PAdditiveSemigroup (pscalePositive, (#+)),
  PMultiplicativeSemigroup (ppowPositive, (#*)),
  PNatural,
  PPositive,
  pzero,
  (#-),
 )
import Plutarch.Internal.Ord (POrd ((#<=)))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PlutusType (PInner, pcon', pmatch'), pcon, pmatch)
import Plutarch.Internal.Quantification (PForall (PForall))
import Plutarch.Internal.Subtype (pupcast)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  punsafeCoerce,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.List (PList (PSCons, PSNil))

{- | A pull array, represented as its Boehm-Berrarducci encoding. Put another
way, a pull array is a function which can be \'materialized\' to produce the
elements of that array, in order.

Pull arrays give efficient linear transformations, in exchange for no ability
to index them without evaluation. Pull arrays are best used when you need to
perform a lot of transformations, with a fold or materialization at the end,
as they fuse away /all/ intermediate values. We achieve this by using
Boehm-Berrarducci encodings, which means that every pull array is essentially
a lambda onchain.

@since 1.12.0
-}
newtype PPullArray (a :: S -> Type) (s :: S)
  = PPullArray (PForall (PArrOf a) s)

-- | @since 1.12.0
instance PlutusType (PPullArray a) where
  type PInner (PPullArray a) = PForall (PArrOf a)
  pcon' (PPullArray t) = pcon t
  pmatch' t f = pmatch t $ \t' -> f (PPullArray t')

-- | @since 1.12.0
instance PAdditiveSemigroup a => PAdditiveSemigroup (PPullArray a) where
  arr1 #+ arr2 = pzipWithArray padd arr1 arr2
  pscalePositive arr p = pmapArray (pscaleBy # p) arr

-- | @since 1.12.0
instance PMultiplicativeSemigroup a => PMultiplicativeSemigroup (PPullArray a) where
  arr1 #* arr2 = pzipWithArray pmul arr1 arr2
  ppowPositive arr p = pmapArray (ppowBy # p) arr

{- | Given a builtin array, construct the equivalent pull array.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
pfromArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a) ->
  Term s (PPullArray a)
pfromArray arr = pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
  k # punsafeCoerce (plengthOfArray # arr) # (pindexArray # arr)

{- | Given a builtin list, construct the equivalent pull array. Uses
'plistToArray' internally.

\(\Theta(1)\) space complexity, \(\Theta(n)\) time complexity.

@since 1.12.0
-}
pfromList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBuiltinList a) ->
  Term s (PPullArray a)
pfromList ell = pfromArray (plistToArray # ell)

{- | Given a size limit \(k\) and a pull array of length \(n\), construct a new
pull array that consists of the first \(\min \{k, n\}\) elements of the
argument pull array, at the same indexes.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
ptakeArray ::
  forall (a :: S -> Type) (s :: S).
  Term s PNatural ->
  Term s (PPullArray a) ->
  Term s (PPullArray a)
ptakeArray limit arr = pmatch arr $ \(PPullArray (PForall f)) ->
  pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
    punsafeCoerce f # plam (\len g -> k # pmin limit len # g)

-- | @since 1.12.0
pdropArray ::
  forall (a :: S -> Type) (s :: S).
  Term s PNatural ->
  Term s (PPullArray a) ->
  Term s (PPullArray a)
pdropArray dropped arr = pmatch arr $ \(PPullArray (PForall f)) ->
  pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
    punsafeCoerce f # plam (\len g -> k # pdoz len dropped # plam (\ix -> g # (ix #+ pupcast @PInteger dropped)))

{- | Given a \'combining function\' and a starting value, reduce the argument
array by repeatedly combining elements with the starting value. This is a
left fold: thus, it will start at the lowest index and work its way upward.

Assuming \(\Theta(k)\) cost in space, and \(\Theta(\ell)\) cost in time, per
application of the \'combining function\', \(\Theta(kn)\) space complexity
and \(\Theta(k\ell)\) time complexity.

@since 1.12.0
-}
pfoldlArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (b :--> a :--> b) ->
  Term s b ->
  Term s (PPullArray a) ->
  Term s b
pfoldlArray f acc arr = pmatch arr $ \(PPullArray (PForall g)) ->
  punsafeCoerce g
    # plam
      ( \len get ->
          phoistAcyclic (pfix go) # f # get # pupcast @_ @PNatural len # 0 # acc
      )
  where
    go ::
      forall (s' :: S).
      Term s' ((b :--> a :--> b) :--> (PInteger :--> a) :--> PInteger :--> PInteger :--> b :--> b) ->
      Term s' ((b :--> a :--> b) :--> (PInteger :--> a) :--> PInteger :--> PInteger :--> b :--> b)
    go self = plam $ \combine get limit currIx acc' ->
      pif
        (currIx #== limit)
        acc'
        (self # combine # get # limit # (currIx + 1) #$ combine # acc' #$ get # currIx)

{- | Given a \'transformation function\' and a pull array, construct a new pull
array where each element of the argument array has been transformed without
moving it.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
pmapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pmapArray f arr = pmatch arr $ \(PPullArray (PForall g)) ->
  pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
    punsafeCoerce g # plam (\len h -> k # len # pcompose h f)

{- | Convert a pull array to a builtin list. Prefer this function to using a
fold, as it builts the list \'in reverse\' to avoid quadratic construction
cost.

If you want to construct a builtin /array/ instead, use this function
together with 'plistToArray'.

\(\Theta(n)\) space and time complexity.

@since 1.12.0
-}
ppullArrayToList ::
  forall (a :: S -> Type) (s :: S).
  PlutusType (PBuiltinList a) =>
  Term s (PPullArray a) ->
  Term s (PBuiltinList a)
ppullArrayToList arr = pmatch arr $ \(PPullArray (PForall f)) ->
  punsafeCoerce f
    # phoistAcyclic
      ( plam $ \len f ->
          phoistAcyclic (pfix go) # f # (pupcast @_ @PNatural len - 1) # pcon PNil
      )
  where
    go ::
      forall (s' :: S).
      Term s' ((PInteger :--> a) :--> PInteger :--> PBuiltinList a :--> PBuiltinList a) ->
      Term s' ((PInteger :--> a) :--> PInteger :--> PBuiltinList a :--> PBuiltinList a)
    go self = plam $ \f currIx acc ->
      pif
        (currIx #== (-1))
        acc
        (self # f # (currIx - 1) #$ pconsBuiltin # (f # currIx) # acc)

{- | Convert a pull array to a 'PList'. Prefer this function to using a fold, as
it builds the 'PList' \'in reverse\' to avoid quadratic construction cost.

\(\Theta(n)\) space and time complexity.

@since 1.12.0
-}
ppullArrayToSOPList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PPullArray a) ->
  Term s (PList a)
ppullArrayToSOPList arr = pmatch arr $ \(PPullArray (PForall f)) ->
  punsafeCoerce f
    # plam
      ( \len f ->
          phoistAcyclic (pfix go) # f # (pupcast @_ @PNatural len - 1) # pcon PSNil
      )
  where
    go ::
      forall (s' :: S).
      Term s' ((PInteger :--> a) :--> PInteger :--> PList a :--> PList a) ->
      Term s' ((PInteger :--> a) :--> PInteger :--> PList a :--> PList a)
    go self = plam $ \f currIx acc ->
      pif
        (currIx #== (-1))
        acc
        (self # f # (currIx - 1) # (pcon . PSCons (f # currIx) $ acc))

{- | As 'pmapArray', but with an index-aware \'transformer function\'.

@since 1.12.0
-}
pimapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pimapArray f arr = pmatch arr $ \(PPullArray (PForall g)) ->
  pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
    punsafeCoerce g # plam (\len h -> k # len # picompose f h)

{- | Given a \'combining function\' and two pull arrays, produce a new pull
array whose length is the minimum of the lengths of the arguments, and whose
elements are applications of the \'combining function\' at the respective
indexes of the argument arrays.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
pzipWithArray ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pzipWithArray f arr1 arr2 = pmatch arr1 $ \(PPullArray (PForall g1)) ->
  pmatch arr2 $ \(PPullArray (PForall g2)) ->
    pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
      punsafeCoerce g1
        # plam
          ( \len1 g1 ->
              punsafeCoerce g2
                # plam
                  ( \len2 g2 ->
                      k # pmin @PNatural len1 len2 # pliftIndex f g1 g2
                  )
          )

{- | As 'pzipWithArray', but with an index-aware \'combining function\'.

@since 1.12.0
-}
pizipWithArray ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pizipWithArray f arr1 arr2 = pmatch arr1 $ \(PPullArray (PForall g1)) ->
  pmatch arr2 $ \(PPullArray (PForall g2)) ->
    pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
      punsafeCoerce g1
        # plam
          ( \len1 g1 ->
              punsafeCoerce g2
                # plam
                  ( \len2 g2 ->
                      k # pmin @PNatural len1 len2 # piliftIndex f g1 g2
                  )
          )

{- | Given a length @n@, construct the pull array equivalent of @[0, 1, ... n -
1]@.

\(Theta(1)\) space and time complexity.

@since 1.12.0
-}
piota ::
  forall (s :: S).
  Term s PNatural ->
  Term s (PPullArray PInteger)
piota len = pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
  k # len # pid

{- | Given a length and a function from indexes to values, construct the pull
array of that length, each of whose indexes stores the value computed by that
function.

\(Theta(1)\) space and time complexity.

@since 1.12.0
-}
pgenerate ::
  forall (a :: S -> Type) (s :: S).
  Term s PNatural ->
  Term s (PInteger :--> a) ->
  Term s (PPullArray a)
pgenerate len f = pcon . PPullArray . PForall $ punsafeCoerce $ plam $ \k ->
  k # len # f

-- Helpers

newtype PArrOf (a :: S -> Type) (r :: S -> Type) (s :: S)
  = PArrOf ((:-->) (PNatural :--> (PInteger :--> r) :--> r) r s)

pcompose ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (b :--> c) ->
  Term s (a :--> c)
pcompose f g = plam $ \x -> g #$ f # x

picompose ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b) ->
  Term s (PInteger :--> a) ->
  Term s (PInteger :--> b)
picompose f g = plam $ \ix -> f # ix #$ g # ix

pliftIndex ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b :--> c) ->
  Term s (PInteger :--> a) ->
  Term s (PInteger :--> b) ->
  Term s (PInteger :--> c)
pliftIndex f g1 g2 = plam $ \i -> f # (g1 # i) # (g2 # i)

piliftIndex ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b :--> c) ->
  Term s (PInteger :--> a) ->
  Term s (PInteger :--> b) ->
  Term s (PInteger :--> c)
piliftIndex f g1 g2 = plam $ \i -> f # i # (g1 # i) # (g2 # i)

pmin ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a ->
  Term s a ->
  Term s a
pmin x y = pif (x #<= y) x y

pid ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> a)
pid = phoistAcyclic $ plam id

-- Difference-or-zero
pdoz :: Term s PNatural -> Term s PNatural -> Term s PNatural
pdoz x y = plet (pupcast @PInteger x #- pupcast y) $ \result ->
  pif
    (result #<= (-1))
    pzero
    (punsafeCoerce result)

padd ::
  forall (a :: S -> Type) (s :: S).
  PAdditiveSemigroup a =>
  Term s (a :--> a :--> a)
padd = phoistAcyclic $ plam (#+)

pscaleBy ::
  forall (a :: S -> Type) (s :: S).
  PAdditiveSemigroup a =>
  Term s (PPositive :--> a :--> a)
pscaleBy = phoistAcyclic $ plam $ \p x -> pscalePositive x p

pmul ::
  forall (a :: S -> Type) (s :: S).
  PMultiplicativeSemigroup a =>
  Term s (a :--> a :--> a)
pmul = phoistAcyclic $ plam (#*)

ppowBy ::
  forall (a :: S -> Type) (s :: S).
  PMultiplicativeSemigroup a =>
  Term s (PPositive :--> a :--> a)
ppowBy = phoistAcyclic $ plam $ \p x -> ppowPositive x p
