module Plutarch.Prelude (
  -- * eDSL types and functions.
  (:-->),
  PDelayed,
  Term,
  plam,
  plam',
  papp,
  pdelay,
  pforce,
  phoistAcyclic,
  perror,
  (#$),
  (#),
  plet,
  pinl,
  pto,
  pfix,
  Type,
  S,
  PType,
  PlutusType (PInner),
  PCon (pcon),
  PMatch (pmatch),

  -- * Integers and integer utilities
  PInteger,
  PIntegral (pdiv, pmod, pquot, prem),

  -- * Rational numbers and utilities
  PRational,
  pnumerator,
  pdenominator,
  pfromInteger,
  pround,

  -- * Booleans and boolean functions
  PBool (..),
  PEq ((#==)),
  POrd ((#<=), (#<)),
  pif,
  pnot,
  (#&&),
  (#||),

  -- * Bytestrings and bytestring utilities
  PByteString,
  phexByteStr,
  pconsBS,
  psliceBS,
  plengthBS,
  pindexBS,

  -- * String and string utilities
  PString,
  pencodeUtf8,
  pdecodeUtf8,

  -- * Unit type and utilities
  PUnit (..),

  -- * Common list typeclass and utilities
  PListLike (PElemConstraint, pelimList, pcons, pnil, phead, ptail, pnull),
  PIsListLike,
  plistEquals,
  pelem,
  plength,
  ptryIndex,
  pdrop,
  psingleton,
  pconcat,
  pzipWith,
  pzipWith',
  pzip,
  pmap,
  pfilter,
  precList,
  pfoldr,
  pfoldrLazy,
  pfoldl,
  pall,
  pany,

  -- * Scott encoded list type
  PList (..),

  -- * Scott encoded maybe type and utilities
  PMaybe (..),

  -- * Scott encoded either type and utilities
  PEither (..),

  -- * Scott encoded pair type and utilities
  PPair (..),

  -- * Builtin types and utilities
  PData (..),
  pfstBuiltin,
  psndBuiltin,
  PBuiltinPair,
  PBuiltinList (..),
  PIsData (pfromData, pdata),
  PAsData,

  -- * DataRepr and related functions
  PDataRecord,
  PDataSum,
  PIsDataRepr,
  PLabeledType ((:=)),
  pdcons,
  pdnil,
  pfield,
  pletFields,
  hrecField,

  -- * Tracing
  ptrace,
  ptraceIfFalse,
  ptraceIfTrue,
  ptraceError,

  -- * Cryptographic hashes and signatures
  psha2_256,
  psha3_256,
  pverifySignature,

  -- * Converstion between Plutarch terms and Haskell types
  pconstant,
  plift,
  PConstant,
  PLift,

  -- * Typeclass derivers.
  DerivePNewtype (DerivePNewtype),

  -- * Continuation monad
  TermCont (TermCont, runTermCont),
  unTermCont,
  tcont,
) where

import Prelude ()

import Data.Kind (Type)
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.ByteString
import Plutarch.Crypto
import Plutarch.DataRepr
import Plutarch.Either
import Plutarch.Integer
import Plutarch.Lift
import Plutarch.List
import Plutarch.Maybe
import Plutarch.Pair
import Plutarch.Rational
import Plutarch.String
import Plutarch.TermCont
import Plutarch.Trace
import Plutarch.Unit
