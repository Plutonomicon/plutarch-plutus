{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
-- This needs to go later
{-# OPTIONS_GHC -Wwarn #-}

module Plutarch.Rework where

import Generics.SOP

-- import GHC.TypeLits

import Data.Kind (Type)
import Data.Proxy
import Plutarch.Builtin
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

-- data Handler out xs where
--   HandlerNil
--   HandlerCons :: (forall s. Term s PData -> Term s b) -> Handler out xs -> Handler out (x ': xs)

-- First integer is for indexing constr index
-- Second function is the handler itself

-- data HandlerBuilder b s cs where
--   HandlerBuilderNil :: HandlerBuilder b s '[]
--   HandlerBuilderCons ::
--     Integer ->
--     ((PDataStructure struct s -> Term s b) -> (Term s (PBuiltinList PData) -> Term s b)) ->
--     HandlerBuilder b s (c ': cs)
-- data HandlerBuilder b s cs =
--   HandlerBuilder (Term s (PDataStructure cs) -> Term s b)

-- data A b s cs = A {i :: Integer, unA :: (PDataStructure cs s -> Term s b) -> HandlerBuilder b s cs}

newtype H s struct = H {unH :: forall r. (PRecord struct s -> Term s r) -> Term s r}

pdataFrom :: Foo struct => Term s (PBuiltinList PData) -> (PRecord struct s -> Term s b) -> Term s b
pdataFrom x f = plet x $ \y -> pdataFrom' y f

class Foo struct where
  pdataFrom' :: Term s (PBuiltinList PData) -> (PRecord struct s -> Term s b) -> Term s b

instance (PIsData x, Foo xs) => Foo (x ': xs) where
  pdataFrom' ds =
    let
      x = pfromData @x $ punsafeCoerce $ phead # ds
      rest = pdataFrom' @xs $ ptail # ds
     in
      \f -> rest (\(PRecord rest') -> plet x $ \x' -> f (PRecord (x' :* rest')))

instance Foo '[] where
  pdataFrom' _ f = f $ PRecord Nil

data HandlerBuilder b s cs where
  HandlerBuilder :: Integer -> (PDataStructure cs s -> Term s b) -> (Term s b) -> HandlerBuilder b s (c ': cs)

-- pdataReprFromData
--   :: forall (struct :: [[S->Type]]) b (s :: S)
--    . (All2 PDataRepresentable struct, All2 PIsData struct, SListI struct)
--   => Term s (PDataStructure struct) -> (PDataStructure struct s -> Term s b) -> Term s b
-- pdataReprFromData (pdataStructureAsData -> x) f = unTermCont $ do
--   constr <- pletC $ pasConstr # x
--   idx <- pletC $ pfstBuiltin # constr

--   let
--     idx = pfstBuiltin # constr
--     d = psndBuiltin # constr

--     -- rewrap (PDataStructure (PStructure (SOP x))) = PDataStructure (PStructure (SOP (S x)))

--     -- foo :: SListI struct => HandlerBuilder b s struct
--     -- foo =
--     --   para_SList
--     --   @struct
--     --   (HandlerBuilderNil)
--     --   (\(HandlerBuilderCons i prevf) ->
--     --      let
--     --        go :: Term s (PBuiltinList PData) -> Term s b
--     --        go ds =
--     --          pif (idx #== pconstant i)
--     --            (f $ PDataStructure $ PStructure $ SOP $ undefined)
--     --            (prevf (\(PDataStructure (PStructure (SOP x))) -> f $ PDataStructure $ PStructure $ SOP $ S x) ds)
--     --      in HandlerBuilderCons (i + 1) go
--     --      )

--     -- foo :: HandlerBuilder b s struct
--     -- foo =
--     --   para_SList
--     --   @struct
--     --   (HandlerBuilder 0 (const perror) perror)
--     --   (\(HandlerBuilder i f' prev) ->
--     --      HandlerBuilder (i+1) (\(PDataStructure (PStructure (SOP z))) -> f' $ PDataStructure $ PStructure $ SOP $ S z) $
--     --        pif (pconstant i #== idx)
--     --          undefined
--     --          prev
--     --      )

--     -- foo :: A b s struct
--     -- foo =
--     --   para_SList
--     --   @struct
--     --   (A 0 $ const $ HandlerBuilder (const perror))
--     --   (\(A i prev) -> A (i + 1) $ \f' -> HandlerBuilder $ \ds ->
--     --      let
--     --        -- conv :: NP (Term s) y
--     --        -- conv = undefined
--     --        go f' ds = undefined
--     --      in pif (pconstant i #== idx)
--     --           (undefined)
--     --           (prev )
--     --      )

--   -- pure $ f $ (PDataStructure (PStructure $ SOP $ foo 0))

--   undefined

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
