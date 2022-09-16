{-# LANGUAGE DataKinds, ImpredicativeTypes, TypeFamilies #-}

{-|
Description : Database of Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

A global collection of keyed records.

-}

module Eiger.Database (
  Database,
  emptyDatabase, addDb, addDbWithKey, updateDb,
  databaseLookup, allTextKeys, getAllKeysOfType, getAllPairs,
  DatabaseValue(..),
  ) where

import Eiger.Record.Type
import Eiger.Key
import Eiger.Util

import qualified Data.Dependent.Map as DM
import qualified Data.Dependent.Sum as DS
import qualified Data.Set as S

import Data.Maybe ( mapMaybe )
import Type.Reflection
import Data.List ( intercalate )

-- | A global store. This can map keys to records.
type Database :: Type
data Database = MkDb { available_keys :: KeyGen
                     , used_text_keys :: S.Set Text
                     , database :: DM.DMap Key DatabaseValue }
    -- INVARIANT: keys ∩ keys database = ∅

instance Show Database where   -- for debugging only!
  show (MkDb
    { available_keys = available
    , used_text_keys = used_text
    , database = mapping }) =
    concat [ "MkDb { available_keys = ", show (fst (genKey @() available)), "..., used_text_keys = "
           , show used_text, ", database = ", show_mapping ]
    where
      show_mapping = "[" ++ intercalate "," (map show_pair (DM.assocs mapping)) ++ "]"

      show_pair :: DS.DSum Key DatabaseValue -> String
      show_pair (k DS.:=> val) = show k ++ " :=> " ++ show val

-- | Store a value indexed by its record type.
type DatabaseValue :: Record -> Type
data DatabaseValue record = MkDbV (TypeRep record) !record

instance Show (DatabaseValue record) where
  show (MkDbV rep _) = "MkDbV " ++ (show rep) ++ " ..."

-- | An empty 'Database'
emptyDatabase :: Database
emptyDatabase = MkDb { available_keys = keyGen
                     , used_text_keys = mempty
                     , database = mempty }

-- | Look up a key in the database. Throws an exception if the
-- key is not in the database.
databaseLookup :: Database -> Key record -> Maybe record
databaseLookup (MkDb { database = dmap }) key = case DM.lookup key dmap of
  Nothing -> Nothing
  Just (MkDbV _ record) -> Just record

-- | Retrieve the set of all text keys that are used in this database.
allTextKeys :: Database -> S.Set Text
allTextKeys = used_text_keys

-- | Get all keys of a certain record type.
getAllKeysOfType :: forall record. Typeable record => Database -> [Key record]
getAllKeysOfType (MkDb { database = dmap }) =
  mapMaybe check_type $ DM.assocs dmap
  where
    record_rep = typeRep @record

    check_type :: DS.DSum Key DatabaseValue -> Maybe (Key record)
    check_type ((DS.:=>) key (MkDbV found_rep _))
      | Just HRefl <- record_rep `eqTypeRep` found_rep
      = Just key
      | otherwise
      = Nothing

-- | Get all keys to this database.
getAllPairs :: Database -> [DS.DSum Key DatabaseValue]
getAllPairs (MkDb { database = dmap }) = DM.assocs dmap

-- | Insert an item (with a field named @key@) to a database, returning
-- the extended database and the record's assigned key. Returns Nothing
-- if the user has specified a textual key and that key is already used.
addDb ::
  HasKey record =>
  Typeable record =>
  Database ->
  Maybe Text ->    -- ^ user-specified key, if one exists
  record ->
  Maybe (Key record, Database)
addDb (MkDb { available_keys = available
            , used_text_keys = used_text
            , database = mapping }) m_key record = do  -- Maybe monad
  (record_key, new_available_keys, new_used_text_keys) <- case m_key of
    Just text_key
      | S.member text_key used_text -> Nothing
      | otherwise -> return (mkTextKey text_key, available, S.insert text_key used_text)
    Nothing -> return (k, ks, used_text)
      where (k,ks) = genKey available
  let keyed_record = setRecordField @"key" record record_key
  return (record_key, MkDb { available_keys = new_available_keys
                           , used_text_keys = new_used_text_keys
                           , database = DM.insert record_key (MkDbV typeRep keyed_record) mapping })

-- | Insert an item with a known key to a database. Throws an exception
-- if the key is already in the database.
addDbWithKey ::
  HasKey record =>
  Typeable record =>
  Database -> record -> Database
addDbWithKey db@(MkDb { available_keys = key_gen, used_text_keys = used_text, database = mapping }) record
  | ExternalUserKey text_key <- this_user_key
  , not (S.member text_key used_text)
  = db { used_text_keys = S.insert text_key used_text, database = new_mapping }
  | InternalUserKey n <- this_user_key
  , Just new_keys <- removeKey key_gen n
  = db { available_keys = new_keys, database = new_mapping }
  | otherwise
  = error $ "Duplicate key: " ++ show this_key
  where
    this_key = getRecordField @"key" record
    this_user_key = getUserKey this_key
    new_mapping = DM.insert this_key (MkDbV typeRep record) mapping
infixl 0 `addDbWithKey`

-- | Update a record in the database. Do *not* change the key of the record!
-- Returns 'Nothing' if the key is not found.
updateDb ::
  forall record.
  Database ->
  Key record ->
  (record -> record) ->
  Maybe Database
updateDb db@(MkDb { database = mapping }) key upd = do   -- Maybe monad
  new_mapping <- DM.alterF key alter_fun mapping
  return (db { database = new_mapping})
  where
    alter_fun :: Maybe (DatabaseValue record) -> Maybe (Maybe (DatabaseValue record))
    alter_fun Nothing = Nothing   -- failure
    alter_fun (Just (MkDbV rep old_rec)) = Just (Just (MkDbV rep (upd old_rec)))
      -- NB: Just Nothing would delete the record, but otherwise succeed