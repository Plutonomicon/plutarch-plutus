{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Plutarch.Repr.Internal (
  RecAsHaskell,
  StructAsHaskell,
  PStruct (PStruct, unPStruct),
  PRec (PRec, unPRec),
  pletL,
  grecEq,
  gstructEq,
  groupHandlers,
  StructSameRepr,
  UnTermRec,
  UnTermStruct,
  UnTermStruct',
  RecTypePrettyError,
) where

import Control.Arrow (Arrow (..))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashed)
import Data.Kind (Type)
import Data.List (groupBy, sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Proxy (Proxy (Proxy))
import Debug.Trace (traceShow)
import GHC.TypeError (ErrorMessage (Text), TypeError)
import Generics.SOP (
  All,
  All2,
  AllZipN,
  Code,
  I,
  K (K),
  LiftedCoercible,
  NP (Nil, (:*)),
  NS (S, Z),
  Prod,
  SListI,
  SOP (SOP),
  ccompare_SOP,
  hcliftA2,
  hcollapse,
  para_SList,
 )
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool, pif)
import Plutarch.Builtin.Integer (PInteger, pconstantInteger)
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.Lift (AsHaskell, pconstant)
import Plutarch.Internal.Term (RawTerm, S, Term, plet)
import Plutarch.Internal.TermCont (hashOpenTerm, unTermCont)

-- | @since 1.10.0
newtype PStruct (struct :: [[S -> Type]]) (s :: S) = PStruct
  { unPStruct :: SOP (Term s) struct
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
newtype PRec (struct :: [S -> Type]) (s :: S) = PRec
  { unPRec :: NP (Term s) struct
  -- ^ @since 1.10.0
  }

-- | @since 1.10.0
pletL :: All SListI as => SOP (Term s) as -> (SOP (Term s) as -> Term s r) -> Term s r
pletL (SOP (Z x)) f = pletL' x \x' -> f (SOP $ Z x')
pletL (SOP (S xs)) f = pletL (SOP xs) \(SOP xs') -> f (SOP $ S xs')

-- NOTE/TODO: These will generate large blob code, not too efficient unfortunately.
-- We are stuck with this for SOP and Scott however. Need some benchmark
-- reason: https://github.com/IntersectMBO/plutus/pull/5440

-- | @since 1.10.0
grecEq ::
  forall (s :: S) (struct :: [S -> Type]).
  All PEq struct =>
  NP (Term s) struct ->
  NP (Term s) struct ->
  Term s PBool
grecEq x y = pands $ hcollapse $ hcliftA2 (Proxy @PEq) (\a b -> K (a #== b)) x y

-- | @since 1.10.0
gstructEq ::
  forall (s :: S) (struct :: [[S -> Type]]).
  All2 PEq struct =>
  SOP (Term s) struct ->
  SOP (Term s) struct ->
  Term s PBool
gstructEq x y =
  let
    f :: forall xs. All PEq xs => NP (Term _) xs -> NP (Term _) xs -> Term _ PBool
    f a b = pands $ hcollapse $ hcliftA2 (Proxy @PEq) (\a' b' -> K (a' #== b')) a b
   in
    ccompare_SOP
      (Proxy @PEq)
      (pconstant @PBool False)
      f
      (pconstant @PBool False)
      x
      y

{- | This function handles optimization of function that require multiple handlers by checking hashes of each
| handler item and merging them in a way it will minimize size and cost of all computation

@since 1.10.0
-}
groupHandlers :: forall (s :: S) (r :: S -> Type). [(Integer, Term s r)] -> Term s PInteger -> Term s r
groupHandlers handlers idx = unTermCont $ do
  handlersWithHash :: [(Integer, (Term s b, Hashed RawTerm))] <-
    traverse (\(i, t) -> (\hash -> (i, (t, hash))) <$> hashOpenTerm t) handlers

  let
    groupedHandlers :: [([Integer], Term s b)]
    groupedHandlers =
      sortBy (comparing ((length . fst) &&& fst))
        . map (first sort)
        . HM.elems
        . HM.map (\xs -> (map fst xs, snd $ head xs))
        . HM.fromListWith (flip (++))
        $ [(h, [(i, t)]) | (i, (t, h)) <- handlersWithHash]

  pure $
    let
      -- This one builds chain of #&& condition, making if one per groups
      pgo :: [([Integer], Term s b)] -> Term s b
      pgo [(_, t)] = t
      pgo [(is, t), (_, t')] =
        pif (pands $ (\i -> pconstantInteger i #== idx) <$> is) t t'
      pgo ((is, t) : rest) =
        pif (pands $ (\i -> pconstantInteger i #== idx) <$> is) t (pgo rest)
      pgo [] = error "impossible"

      -- This one builds if one per every entry. This is bad because it duplicates handler
      buildIfs :: [Integer] -> Term s b -> (Term s b -> Term s b)
      buildIfs [] _ = id
      buildIfs (i : is) t =
        buildIfs is t . pif (pconstantInteger i #== idx) t

      pgo' :: [([Integer], Term s b)] -> Term s b
      pgo' [(is, t), (_, t')] = buildIfs is t t'
      pgo' ((is, t) : rest) = buildIfs is t $ pgo' rest
      pgo' [] = error "impossible"

      -- So that GHC doesn't complain
      _a = pgo'
      _b = pgo
     in
      -- first one seems to be faster
      pgo groupedHandlers

-- | @since 1.10.0
class
  ( SOP.Generic (a s)
  , AllZipN @Type (Prod SOP) (LiftedCoercible I (Term s)) (Code (a s)) struct
  , AllZipN @Type (Prod SOP) (LiftedCoercible (Term s) I) struct (Code (a s))
  ) =>
  StructSameRepr s a struct

instance
  ( SOP.Generic (a s)
  , AllZipN @Type (Prod SOP) (LiftedCoercible I (Term s)) (Code (a s)) struct
  , AllZipN @Type (Prod SOP) (LiftedCoercible (Term s) I) struct (Code (a s))
  ) =>
  StructSameRepr s a struct

-- | @since 1.10.0
type family UnTermRec (struct :: [Type]) :: [S -> Type] where
  UnTermRec '[] = '[]
  UnTermRec (Term _ a ': rest) = a ': UnTermRec rest

-- | @since 1.10.0
type UnTermStruct x = UnTermStruct' (Code x)

-- | @since 1.10.0
type RecTypePrettyError struct = RecTypePrettyError' struct ~ 'True

-- Helpers

newtype PLetL s r as = PLetL {unPLetL :: NP (Term s) as -> (NP (Term s) as -> Term s r) -> Term s r}

pletL' :: SListI as => NP (Term s) as -> (NP (Term s) as -> Term s r) -> Term s r
pletL' = unPLetL $ para_SList
  (PLetL \Nil f -> f Nil)
  \(PLetL prev) -> PLetL \(x :* xs) f -> plet x \x' ->
    prev xs (\xs' -> f (x' :* xs'))

type family UnTermStruct' (struct :: [[Type]]) :: [[S -> Type]] where
  UnTermStruct' '[] = '[]
  UnTermStruct' (x ': rest) = UnTermRec x ': UnTermStruct' rest

type RecTypePrettyError' :: forall {k}. [[k]] -> Bool
type family RecTypePrettyError' (xs :: [[k]]) :: Bool where
  RecTypePrettyError' (_ ': '[]) = 'True
  RecTypePrettyError' (_ ': _) =
    TypeError
      ('Text "Deriving record encoding only works with types with single constructor. More than one constructor is found.")

--------------------------------------------------------------------------------

pands :: [Term s PBool] -> Term s PBool
pands [] = pconstant True
pands [x'] = x'
pands (x' : xs') = pif x' (pands xs') (pconstant False)

--------------------------------------------------------------------------------

type family RecAsHaskell (x :: [S -> Type]) where
  RecAsHaskell (x ': xs) = AsHaskell x ': RecAsHaskell xs
  RecAsHaskell '[] = '[]

type family StructAsHaskell (x :: [[S -> Type]]) where
  StructAsHaskell (x ': xs) = RecAsHaskell x ': StructAsHaskell xs
  StructAsHaskell '[] = '[]
