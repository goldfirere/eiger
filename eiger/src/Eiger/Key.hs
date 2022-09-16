{-# LANGUAGE DataKinds, GADTs, ImpredicativeTypes, TypeFamilies #-}

{-|
Description : Keys for Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

Define a Key type that can be used to look up records in the Eiger database.
This could, in theory, be based upon "The Key Monad: Type-Safe Unconstrained
Dynamic Typing", by van der Ploeg et al. [Haskell'16], but that requires threading
an ST-like state token s, and the library implementing that paper, titled `key`,
is no longer maintained.

-}

module Eiger.Key (
  Key, HasKey, fakeKey, lazyKey, mkTextKey,
  UserKey(..), getUserKey, getInternalKeyUnique,
  KeyGen, keyGen, genKey, getKeys, removeKey,
  InfList(..),
  ) where

import Eiger.Record.Type
import Eiger.Util

import Data.GADT.Compare ( GEq(..), GCompare(..), GOrdering(..) )
import Data.GADT.Show ( GShow(..) )

import Type.Reflection
import Data.Type.Equality
import Text.Printf
import Data.Coerce

-- | The key (unique identifier) for the record it is stored in.
-- Every Eiger record type must have precisely one Key field; it should
-- be parameterized by the type of the record itself.
type Key :: Record -> Type
data Key rec = UnsafeMkKey (TypeRep rec) UserKey
  deriving (Eq, Ord)

-- | @HasKey record@ states that @record@ has a field named
-- @"key"@ of type @Key record@.
type HasKey record = HasRecordField "key" record (Key record)

-- | A key suitable for presentation to users. It is rendered as either a string
-- (not beginning with a digit) or a number. The numeric keys are generated
-- automatically and may not be user-specified.
data UserKey = ExternalUserKey Text | InternalUserKey InternalKey
  deriving (Eq, Ord)

-- | Opaque type of internally generated keys
newtype InternalKey = MkIK Int
  deriving (Eq, Ord)

-- | A fake key. Comparing this key for equality throws an exception. Useful when
-- creating a record before assigning it a key.
fakeKey :: Key record
fakeKey = UnsafeMkKey err err
  where
    err = error "Compared a fakeKey for equality; this is a bug in Eiger."

-- | A lazy key. Forcing this key is guaranteed to be safe, but using it in
-- an equality comparison is a very bad idea, unless the underlying key can
-- be forced.
lazyKey :: Key record -> Key record
lazyKey ~(UnsafeMkKey rep n) = UnsafeMkKey rep n

-- | Create a 'Key' from a 'Text'. It is up to the caller to ensure
-- that the key is unique among other keys.
mkTextKey :: Typeable record => Text -> Key record
mkTextKey t = UnsafeMkKey typeRep (ExternalUserKey t)

-- | Return the 'UserKey' underlying this 'Key'
getUserKey :: Key record -> UserKey
getUserKey (UnsafeMkKey _ user_key) = user_key

-- | Retrive a unique number associated with this 'InternalKey'; guaranteed
-- to be different for each 'InternalKey'.
getInternalKeyUnique :: InternalKey -> Int
getInternalKeyUnique (MkIK n) = n

-- | Returns a proof only when two keys are equal, even if two keys
-- are for the same type (and thus returning a proof would be type-safe).
instance TestEquality Key where
  UnsafeMkKey rep1 i1 `testEquality` UnsafeMkKey rep2 i2
    | Just Refl <- rep1 `testEquality` rep2
    , i1 == i2
    = Just Refl

    | otherwise
    = Nothing

instance GEq Key where
  geq = testEquality

instance GCompare Key where
  k1@(UnsafeMkKey rep1 i1) `gcompare` k2@(UnsafeMkKey rep2 i2)
    | Just Refl <- k1 `geq` k2                        = GEQ
    | (SomeTypeRep rep1, i1) < (SomeTypeRep rep2, i2) = GLT
    | otherwise                                       = GGT

instance Show (Key record) where
  -- show in hexadecimal, to make these look more opaque
  show (UnsafeMkKey _ (ExternalUserKey t)) = "\"" ++ tunpack t ++ "\""
  show (UnsafeMkKey rep (InternalUserKey (MkIK i))) = printf "%s@%04d" (show rep) i

instance GShow Key where gshowsPrec = showsPrec

--------------------------------------------------
-- generating keys

-- | A source of unique keys
type KeyGen :: Type
newtype KeyGen = MkKeyGen (InfList InternalKey)
  -- INVARIANT: keys are in ascending order by stored Int

-- | A source of unique keys
keyGen :: KeyGen
keyGen = coerce (iterateInfList @Int (+1) 1)
  -- NB: eta-expansion is important for Quick Look to work

-- | Get a fresh key for a given record type
genKey :: Typeable record => KeyGen -> (Key record, KeyGen)
genKey (MkKeyGen (head_key ::: tail_keys)) = (UnsafeMkKey typeRep (InternalUserKey head_key), MkKeyGen tail_keys)

-- | Get an infinite list of unique keys
getKeys :: KeyGen -> InfList (forall record. Typeable record => Key record)
getKeys (MkKeyGen keys) = fmap (\ik -> UnsafeMkKey typeRep (InternalUserKey ik)) keys

-- | Remove the given key from the generator, failing if this key has already been used.
removeKey :: KeyGen -> InternalKey -> Maybe KeyGen
removeKey (MkKeyGen all_keys) this_key = fmap MkKeyGen (go all_keys)
  where
    go (head_key ::: rest_keys) = case this_key `compare` head_key of
      LT -> Nothing
      EQ -> Just rest_keys
      GT -> fmap (head_key :::) (go rest_keys)
