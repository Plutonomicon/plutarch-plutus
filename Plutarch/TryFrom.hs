{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Plutarch.TryFrom (
  PTryFrom (PTryFromExcess, ptryFrom),
  HTree (HNode, HLeaf),
  HSTree (HSLeaf, HSRoot, (:<<:)),
  hsing,
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (AppendSymbol, ErrorMessage (Text), KnownNat, Nat, Symbol, TypeError, natVal, type (+))

import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PBuiltinMap,
  PBuiltinPair,
  PData,
  PIsData (pfromData),
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pasMap,
  pdata,
  pforgetData,
  pfstBuiltin,
  ppairDataBuiltin,
  psndBuiltin,
 )

import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal.Other (
  POpaque,
  PType,
  S,
  Term,
  pcon,
  perror,
  plam,
  plet,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Unit (PUnit (PUnit))

import Plutarch.DataRepr.Internal (
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  PUnLabel,
  pdcons,
  pdnil,
 )

import Plutarch.Bool (pif, (#==))

import Plutarch.List (
  phead,
  pmap,
  ptail,
 )

import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import qualified PlutusCore as PLC

import Plutarch.TermCont (TermCont (TermCont, runTermCont), tcont, unTermCont)

----------------------- The class PTryFrom ----------------------------------------------

{- |
    This checks the datastructure for validity.
    If you don't care about parts of the datastructure
    don't verify those parts, just let it return a PData
    instead
    Be aware this might get really expensive, so only
    use it if you cannot establish trust otherwise
    (e.g. via only checking a part of your Data with
    PTryFrom)
    Laws:
      - the operation `ptryFrom` mustn't change the representation of the underlying data
      - the operation `ptryFrom` must always prove the integrity of the whole target type
        - example:
          `ptryFrom PData (PAsData (PBuiltinList PData))` must only succeed if the underlying
          representation is a `BuiltinList` containing any `PData`
        - all conversion are fallible, this happens if the representation doesn't match
          the expected type.
      - the operation `ptryFrom` proves equality between the less expressive `PType` `a` and
        the more expressive `PType` `b`, hence the first element of the resulting Tuple
        must always be wrapped in `PAsData` if the origin type was `PData` (see law 1)
      - the result type `b` must always be safe than the origin type `a`, i.e. it must carry
        more information
-}
class PTryFrom (a :: PType) (b :: PType) (s :: S) where
  type PTryFromExcess a b :: HTree
  ptryFrom :: Term s a -> ((Term s b, HSTree (PTryFromExcess a b) s) -> Term s r) -> Term s r

----------------------- HTree and friends -----------------------------------------------

data HTree
  = HLeaf Symbol PType
  | HNode Symbol [HTree]

data HSTree (as :: HTree) (s :: S) where
  HSLeaf :: forall label typ s. Term s typ -> HSTree ( 'HLeaf label typ) s
  HSRoot :: forall label s. HSTree ( 'HNode label '[]) s
  (:<<:) :: forall label tree trees s. HSTree tree s -> HSTree ( 'HNode label trees) s -> HSTree ( 'HNode label (tree ': trees)) s

infixr 5 :<<:

type (:<->:) :: HTree -> HTree -> HTree
type family n0 :<->: n1 where
  'HNode l n0 :<->: 'HNode l n1 = 'HNode l (n0 ++ n1)
  'HNode l0 _ :<->: 'HNode l1 _ = TypeError ( 'Text "We can only merge nodes with the same label")
  _ :<->: _ = TypeError ( 'Text "We can only merge nodes")

infixr 5 :<->:

type (++) :: [a] -> [a] -> [a]
type family l1 ++ l2 where
  '[] ++ ys = ys
  (x ': xs) ++ ys = xs ++ x ': ys

infixr 5 ++

htreeNode ::
  forall name e s tree.
  ( TreeElemOf name e tree
  ) =>
  HSTree tree s ->
  Term s e
htreeNode tree = indexHTree tree $ treeElemOf @name @e @tree

indexHTree :: forall name s tree. HSTree tree s -> (forall e. TreeElem name e tree -> Term s e)
indexHTree (_ :<<: node) (There rest) = indexHTree node rest
indexHTree (HSLeaf x :<<: _) Here = x
-- indexHTree (tree :<<: _) HereTree = tree
indexHTree _ _ = error "impossible"

class TreeElemOf (name :: Symbol) (typ :: PType) (tree :: HTree) where
  treeElemOf :: TreeElem name typ tree

instance {-# OVERLAPPING #-} TreeElemOf name e ( 'HNode sym ( 'HLeaf name e ': trees)) where
  treeElemOf = Here

instance
  {-# OVERLAPPING #-}
  ( HSTree ( 'HNode name tree) ~ e
  ) =>
  TreeElemOf name e ( 'HNode sym ( 'HNode name tree ': trees))
  where
  treeElemOf = HereTree

instance
  {-# OVERLAPPABLE #-}
  ( TreeElemOf name e ( 'HNode sym trees)
  ) =>
  TreeElemOf name e ( 'HNode sym (tree ': trees))
  where
  treeElemOf = There treeElemOf

data TreeElem (name :: Symbol) (typ :: PType) (tree :: HTree) where
  Here :: TreeElem name typ ( 'HNode sym ( 'HLeaf name typ ': trees))
  HereTree :: TreeElem name (HSTree ( 'HNode name inner)) ( 'HNode sym ( 'HNode name inner ': trees))
  There :: TreeElem name typ ( 'HNode sym l) -> TreeElem name typ ( 'HNode sym (tree ': l))

deriving stock instance Show (TreeElem a b c)

----------------------- PData instances -------------------------------------------------

hsing :: forall sym a (s :: S). Term s a -> HSTree ( 'HLeaf sym a) s
hsing = HSLeaf @sym

type Flip :: (a -> b -> Type) -> b -> a -> Type
newtype Flip (f :: a -> b -> Type) (y :: b) (x :: a) = Flip {unFlip :: f x y}

instance PTryFrom PData (PAsData PInteger) (s :: S) where
  type PTryFromExcess PData (PAsData PInteger) = 'HLeaf "unwrapped" PInteger
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, hsing ver)

instance PTryFrom PData (PAsData PByteString) s where
  type PTryFromExcess PData (PAsData PByteString) = 'HLeaf "unwrapped" PByteString
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, hsing ver)

instance PTryFrom PData PData s where
  type PTryFromExcess PData PData = 'HNode "empty" '[]
  ptryFrom opq = runTermCont $ pure $ (opq, HSRoot)

instance PTryFrom PData (PAsData PData) s where
  type PTryFromExcess PData (PAsData PData) = 'HNode "empty" '[]
  ptryFrom opq = runTermCont $ pure (pdata opq, HSRoot)

-- TODO: add the excess inner type
instance
  ( PTryFrom PData (PAsData a) s
  , PTryFrom PData (PAsData b) s
  ) =>
  PTryFrom PData (PAsData (PBuiltinMap a b)) s
  where
  type PTryFromExcess PData (PAsData (PBuiltinMap a b)) = 'HLeaf "unwrapped" (PBuiltinMap a b)
  ptryFrom opq = runTermCont $ do
    verMap <- tcont $ plet (pasMap # opq)
    -- I've not obtained a `PBuiltinList (PBuiltinPair a b)`
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- TermCont $ ptryFrom @PData @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- TermCont $ ptryFrom @PData @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, hsing ver)

-- TODO: add the excess inner type list
instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinList PData)) s where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = 'HLeaf "unwrapped" (PBuiltinList PData)
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure $ (punsafeCoerce opq, hsing ver)

instance
  {-# OVERLAPPABLE #-}
  forall a b s.
  ( PTryFrom PData a s
  , a ~ PAsData b
  , PIsData b
  ) =>
  PTryFrom PData (PAsData (PBuiltinList a)) s
  where
  type PTryFromExcess PData (PAsData (PBuiltinList a)) = 'HLeaf "unwrapped" (PBuiltinList a)
  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList a)
        lst = punsafeBuiltin PLC.UnListData # opq
        verify :: a ~ PAsData b => Term _ (PAsData b :--> a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- TermCont $ ptryFrom @PData @a @s $ pforgetData e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure $ (punsafeCoerce opq, hsing ver)

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData a s
  , a ~ PAsData a'
  , PIsData a'
  , PTryFrom PData b s
  , b ~ PAsData b'
  , PIsData b'
  ) =>
  PTryFrom PData (PAsData (PBuiltinPair a b)) s
  where
  type PTryFromExcess PData (PAsData (PBuiltinPair a b)) = 'HLeaf "unwrapped" (PBuiltinPair a b)
  ptryFrom opq = runTermCont $ do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> TermCont (ptryFrom @PData @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> TermCont (ptryFrom @PData @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure $ (punsafeCoerce opq, hsing ver)

type FromRecordField :: PLabeledType -> HTree
type family FromRecordField field where
  FromRecordField (label ':= ptyp) = 'HLeaf label ptyp

type FromRecordFields :: [PLabeledType] -> [HTree]
type family FromRecordFields xs where
  FromRecordFields '[] = '[]
  FromRecordFields (x ': xs) = FromRecordField x ': FromRecordFields xs

instance
  ( FromRecordFields xs ~ ValidationExcess xs
  , RecordValidation xs s
  ) =>
  PTryFrom PData (PAsData (PDataRecord xs)) (s :: S)
  where
  type
    PTryFromExcess PData (PAsData (PDataRecord xs)) =
      'HNode
        "record"
        ( '[ 'HLeaf "unwrapped" (PDataRecord xs)
           ]
            ++ FromRecordFields xs
        )
  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList PData)
        lst = pfromData $ punsafeCoerce opq
    (rec', exc) <- recoverRecord @xs @s lst
    rec <- tcont $ plet rec'
    let excess :: HSTree (PTryFromExcess PData (PAsData (PDataRecord xs))) s
        excess = HSLeaf @"unwrapped" rec :<<: exc
    pure (punsafeCoerce opq, excess)

class RecordValidation xs s where
  type ValidationExcess xs :: [HTree]
  recoverRecord ::
    Term s (PBuiltinList PData) ->
    TermCont s (Term s (PDataRecord xs), HSTree ( 'HNode "record" (ValidationExcess xs)) s)

instance
  {-# OVERLAPPABLE #-}
  ( RecordValidation xs s
  , PTryFrom PData (PAsData a) s
  , PTryFrom PData (PAsData (PDataRecord xs)) s
  , TreeElemOf "unwrapped" (PDataRecord xs) ( 'HNode "record" (ValidationExcess xs))
  ) =>
  RecordValidation ((label ':= a) ': xs) s
  where
  type ValidationExcess ((label ':= a) ': xs) = (PTryFromExcess PData (PAsData a)) ': ValidationExcess xs
  recoverRecord lst = do
    let lsthead :: Term s PData
        lsthead = phead # lst
        lsttail :: Term s (PBuiltinList PData)
        lsttail = ptail # lst
    (verhead, exchead) <- TermCont $ ptryFrom @PData @(PAsData a) @s lsthead
    (_, exctail) <- recoverRecord @xs @s lsttail
    rec <- tcont $ plet $ pdcons @label # verhead # (htreeNode @"unwrapped" exctail)
    pure (rec, exchead :<<: exctail)

instance {-# OVERLAPPING #-} RecordValidation '[] s where
  type ValidationExcess '[] = '[]
  recoverRecord _ = pure (pdnil, HSRoot)

{-
class SumValidation (n :: Nat) (sum :: [[PLabeledType]]) where
  validateSum :: Term s PData -> Term s (PBuiltinList PData)

instance
  {-# OVERLAPPABLE #-}
  forall n x xs s.
  ( PTryFrom PData (PAsData (PDataRecord x))
  , SumValidation (n + 1) xs
  , KnownNat n
  , PTryFromExcess PData (PAsData (PDataRecord x)) s ~ Flip Term (PDataRecord x) s
  ) =>
  SumValidation n (x ': xs)
  where
  validateSum s = unTermCont $
    do
      let n :: Integer
          n = natVal (Proxy @n)
      elem <- tcont $ plet $ pasConstr #$ s
      let snd' :: Term _ (PBuiltinList PData)
          snd' =
            pif
              (fromInteger n #== (pfstBuiltin # elem))
              ( unTermCont $ do
                  let rec = pdata $ psndBuiltin # elem
                  y <- (unFlip . snd) <$> TermCont (ptryFrom @PData @(PAsData (PDataRecord x)) $ pforgetData rec)
                  pure $ punsafeCoerce (y :: Term _ (PDataRecord x))
              )
              (validateSum @(n + 1) @xs $ punsafeCoerce s)
      tcont $ plet snd'

instance {-# OVERLAPPING #-} SumValidation n '[] where
  validateSum _ = perror

instance
  {-# OVERLAPPING #-}
  forall ys.
  ( SumValidation 0 ys
  ) =>
  PTryFrom PData (PAsData (PDataSum ys))
  where
  type PTryFromExcess PData (PAsData (PDataSum ys)) = Flip Term PUnit
  ptryFrom opq = runTermCont $ do
    _ <- tcont $ plet $ validateSum @0 @ys opq
    pure (punsafeCoerce opq, Flip $ pcon PUnit)

----------------------- POpaque Instances -----------------------------------------------

{- |
    for none of the opaque instances it can be verified
    that the actual structure is what it says to be
    because that data is lost when the PAsData wrapper
    is removed, this can only be safely used if you obtained
    your POpaque safely
-}
instance
  ( PTryFrom PData (PAsData a)
  , PIsData a
  ) =>
  PTryFrom POpaque a
  where
  type PTryFromExcess POpaque a = Flip Term (PAsData a)
  ptryFrom opq = runTermCont $ do
    let prop :: Term _ a
        prop = punsafeCoerce opq
    ver' <- fst <$> TermCont (ptryFrom @PData @(PAsData a) $ pforgetData $ pdata prop)
    ver <- tcont $ plet ver'
    pure $ (punsafeCoerce opq, Flip ver)

instance
  ( PTryFrom a b
  , PIsData a
  , PIsData b
  ) =>
  PTryFrom (PAsData a) (PAsData b)
  where
  type PTryFromExcess (PAsData a) (PAsData b) = PTryFromExcess a b
  ptryFrom opq = runTermCont $ do
    ver' <- snd <$> TermCont (ptryFrom @a @b (pfromData opq))
    pure $ (punsafeCoerce opq, ver')
    -}
