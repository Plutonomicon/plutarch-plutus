{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- This needs to go later
{-# OPTIONS_GHC -Wwarn #-}

module Plutarch.Rework where

import Generics.SOP

import GHC.TypeLits

import Data.Functor.Compose

import Data.Kind (Type)
import Data.Proxy
import Plutarch.Builtin
import Plutarch.DataRepr.Internal
import Plutarch.Prelude
import Plutarch.Unsafe

newtype PStructure (struct :: [[S -> Type]]) (s :: S) = PStructure (SOP (Term s) struct)
newtype PRecord (struct :: [S -> Type]) (s :: S) = PRecord (NP (Term s) struct)

newtype PDataEncoded (a :: S -> Type) (s :: S) = PDataEncoded (Term s a)
newtype PScottEncoded (a :: S -> Type) (s :: S) = PScottEncoded (Term s a)
newtype PSOPEncoded (a :: S -> Type) (s :: S) = PSOPEncoded (Term s a)

-- Term s (PDataEncoded (PRecord '[PInteger, PInteger]) :--> PSOPEncoded (PRecord '[PInteger, PInteger]))

-- class (All2 PDataRepresentable struct, All2 PIsData struct) => PDataRepresentable (a :: S -> Type) where

data PDataStructure (struct :: [[S -> Type]]) (s :: S) where
  PDataStructure :: (All2 PDataRepresentable struct, All2 PIsData struct) => PStructure struct s -> PDataStructure struct s

class PDataRepresentable (a :: S -> Type)
instance PDataRepresentable (PDataStructure struct)
instance PDataRepresentable PInteger
instance PDataRepresentable PByteString

-- This needs to be defined on per type bases
-- instance PIsData a => PDataRepresentable a

data PScottStructure (struct :: [[S -> Type]]) (s :: S) where
  PScottStructure :: PStructure struct s -> PScottStructure struct s

class PScottRepresentable (a :: S -> Type)

-- This needs to be defined on per type bases
instance PScottRepresentable a

data PSOPStructure (struct :: [[S -> Type]]) (s :: S) where
  PSOPStructure :: All2 PSOPRepresentable struct => PStructure struct s -> PSOPStructure struct s

class PSOPRepresentable (a :: S -> Type)

-- This needs to be defined on per type bases
instance PSOPRepresentable a

class PConvertRepr from to where
  pconvertRepr :: Term s from -> Term s to

---------------------

type MyRepr = '[ '[PInteger], '[PByteString], '[PInteger, PInteger]]
type MyNestedRepr = '[ '[PStructure MyRepr], '[PByteString], '[PInteger, PInteger]]

test :: PStructure MyRepr s
test = PStructure (SOP (S $ S $ Z ((pconstant 10 :: Term s PInteger) :* (pconstant 12 :: Term s PInteger) :* Nil)))

test2 :: PDataStructure MyRepr s
test2 = PDataStructure test

testNested :: PDataStructure MyNestedRepr s
testNested = undefined -- PStructure (SOP (Z (test2 :* Nil)))

-- test2 :: PStructure' MyRepr s
-- test2 = PStructure' (SOP (S $ S $ Z (((pconstant 10) :: Term s PInteger) :* ((pconstant 12) :: Term s PInteger) :* Nil)))

----------------------------------------------------------------------PDataStructure

pdataReprToData :: forall (struct :: [[S -> Type]]) (s :: S). SListI2 struct => PDataStructure struct s -> Term s (PDataStructure struct)
pdataReprToData (PDataStructure (PStructure xs)) =
  let
    collapesdData = hcollapse $ hcmap (Proxy @PIsData) (K . pforgetData . pdata) xs
    builtinList = foldr (\x xs -> pconsBuiltin # x # xs) (pconstant []) collapesdData
    idx = pconstant $ toInteger $ hindex xs
   in
    punsafeCoerce $ pforgetData $ pconstrBuiltin # idx #$ builtinList

pdataStructureAsData :: Term s (PDataStructure struct) -> Term s PData
pdataStructureAsData = punsafeCoerce

type FAZ = '[PInteger, PInteger, PInteger]

faz :: Term s (PBuiltinList PData)
faz =
  let
    a, b, c :: Term s PData
    a = pforgetData $ pdata $ pconstant (1 :: Integer)
    b = pforgetData $ pdata $ pconstant (2 :: Integer)
    c = pforgetData $ pdata $ pconstant (4 :: Integer)
   in
    pconsBuiltin # a #$ pconsBuiltin # b #$ pconsBuiltin # c # pconstant []

-- pdataFromFoo :: Term s (PBuiltinList PData) -> (PRecord struct s -> Term s b) -> Term s b
-- pdataFromFoo = unH $ para_SList (H $ const $ pconstant []) undefined

-- $> import Plutarch.Pretty

-- $> prettyTermAndCost mempty $ precordFromDataFancy @FAZ faz (\(PRecord (x :* _y :* _z :* _)) ->  x + x)

-- $> PI.compile mempty $ plam $ \(x :: Term s PInteger) (y :: Term s PInteger) (z :: Term s PInteger) -> x + y + z

-- $> prettyTermAndCost mempty $ precordFromDataFancy @FAZ faz (\(PRecord (_x :* _y :* _z :* _)) ->  pconstant (5 :: Integer))

newtype H s struct = H {unH :: forall r. Term s (PBuiltinList PData) -> (PRecord struct s -> Term s r) -> Term s r}

-- Note, this binds all fields to plet, need optimization
precordFromDataFancy :: forall (struct :: [S -> Type]) b s. All PIsData struct => Term s (PBuiltinList PData) -> (PRecord struct s -> Term s b) -> Term s b
precordFromDataFancy x f =
  let
    go :: forall y ys. PIsData y => H s ys -> H s (y ': ys)
    go (H rest) = H $ \ds cps ->
      let
        tail = ptail # ds
        parsed = pfromData @y $ punsafeCoerce $ phead # ds
       in
        rest tail $ \(PRecord rest') ->
          plet parsed $
            \parsed' ->
              cps $ PRecord $ parsed' :* rest'
    record = unH $ cpara_SList (Proxy @PIsData) (H $ \_ cps -> cps $ PRecord Nil) go
   in
    plet x (`record` f)

-- This would be supposedly faster when record is large. Otherwise, spamming plet increases cost for small record.
precordFromDataFancy' :: forall (struct :: [S -> Type]) b s. All PIsData struct => Term s (PBuiltinList PData) -> (PRecord struct s -> Term s b) -> Term s b
precordFromDataFancy' x f =
  let
    go :: forall y ys. PIsData y => H s ys -> H s (y ': ys)
    go (H rest) = H $ \ds cps ->
      let
        tail = ptail # ds
        parsed = pfromData @y $ punsafeCoerce $ phead # ds
       in
        plet tail $ \tail' -> rest tail' $ \(PRecord rest') -> plet parsed $ \parsed' -> cps $ PRecord $ parsed' :* rest'
    record = unH $ cpara_SList (Proxy @PIsData) (H $ \_ cps -> cps $ PRecord Nil) go
   in
    plet x (`record` f)

-- -- $> prettyTermAndCost mempty $ hello
--
-- -- $> prettyTermAndCost mempty $ world

-- type MyRepr = '[ '[PInteger], '[PByteString], '[PInteger, PInteger]]

-- world :: forall s. Term s PInteger
-- world =
--   let
--     a :: Term s (PDataSum '[ '["foo" ':= PInteger], '["foo" ':= PByteString], '["foo" ':= PInteger, "bar" ':= PInteger]])
--     a = punsafeCoerce $ pdataStructureAsData $ pdataReprToData test2
--     handler :: PDataSum _ s -> Term s PInteger
--     handler (PDataSum (Z x)) = pconstant (1 :: Integer)
--     handler (PDataSum (S (Z x))) = pconstant (1 :: Integer)
--     handler _ = pconstant (2 :: Integer)
--   in pmatch a handler

-- hello =
--   let
--     handler (PStructure (SOP (Z x))) = pconstant (1 :: Integer)
--     handler _ = pconstant (2 :: Integer)
--   in pStructureFromDataFancy @MyRepr (pdataStructureAsData $ pdataReprToData test2) handler

-- pStructureFromDataFancy :: forall (struct :: [[S -> Type]]) b s. All2 PIsData struct => Term s PData -> (PStructure struct s -> Term s b) -> Term s b
-- pStructureFromDataFancy x f = undefined

-- newtype HR s struct = HR {unHR :: forall r. Integer -> Term s PInteger -> Term s (PBuiltinList PData) -> (PStructure struct s -> Term s r) -> Term s r}

-- pStructureFromDataFancy :: forall (struct :: [[S -> Type]]) b s. All2 PIsData struct => Term s PData -> (PStructure struct s -> Term s b) -> Term s b
-- pStructureFromDataFancy x f =
--   let
--     go :: forall y ys. All PIsData y => HR s ys -> HR s (y ': ys)
--     go (HR rest) = HR $ \i idx ds cps ->
--       let
--         bar = precordFromDataFancy @y ds
--         foo =
--           pif
--             (pconstant i #== idx)
--             (bar $ \(PRecord bar') -> cps $ PStructure $ SOP $ Z bar')
--             (rest (i + 1) idx ds (\(PStructure (SOP sop)) -> cps $ PStructure $ SOP $ S sop))
--        in
--         foo
--    in
--     unTermCont $ do
--       x' <- pletC $ pasConstr # x
--       idx <- pletC $ pfstBuiltin # x'
--       ds <- pletC $ psndBuiltin # x'

--       pure $ (unHR $ cpara_SList (Proxy @(All PIsData)) (HR $ \_ _ _ _ -> perror) go) 0 idx ds f

pdataFrom :: PRecordFromData struct => Term s (PBuiltinList PData) -> (PRecord struct s -> Term s b) -> Term s b
pdataFrom x f = plet x $ \y -> precordFromData' y f

class PRecordFromData struct where
  precordFromData' :: Term s (PBuiltinList PData) -> (PRecord struct s -> Term s b) -> Term s b

instance (PIsData x, PRecordFromData xs) => PRecordFromData (x ': xs) where
  precordFromData' ds =
    let
      x = pfromData @x $ punsafeCoerce $ phead # ds
      rest = precordFromData' @xs $ ptail # ds
     in
      \f -> rest (\(PRecord rest') -> plet x $ \x' -> f (PRecord (x' :* rest')))

instance PRecordFromData '[] where
  precordFromData' _ f = f $ PRecord Nil

data HandlerBuilder b s cs where
  HandlerBuilder :: Integer -> (PDataStructure cs s -> Term s b) -> (Term s b) -> HandlerBuilder b s (c ': cs)

class ReprFromData (struct :: [[S -> Type]]) where
  preprFromData' :: Term s (PAsData (PStructure struct)) -> PStructure struct s

-- instance ReprFromData xs where
--   preprFromData' (pforgetData -> x) = unTermCont $ do
--     constr <- pletC $ pasConstr # x
--     undefined

-- $> import Plutarch.Rework

-- $> import Prelude

-- $> import Plutarch.Prelude

-- $> import Plutarch.Builtin

-- $> import Generics.SOP

-- $> lengthSList (Proxy @MyRepr)

-- $> :t pdataReprFromData (pdataReprToData test2) (\x -> pconstant ())
