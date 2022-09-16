{-# LANGUAGE DataKinds, OverloadedStrings, GADTs, ViewPatterns, ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : JSON printing for an Eiger database
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This writes an Eiger database to a JSON 'JSON.Value'.

-}

module Eiger.JSON.Write (
  databaseToJSON,
  WriteJSONError(..), renderJSONError,
  ) where

import Eiger.Database
import Eiger.Key
import Eiger.Record
import Eiger.JSON.Class
import Eiger.JSON.Instance

import Eiger.JSON.Util

import qualified Data.Dependent.Sum as DS

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Key as JSON

import qualified Generics.SOP as SOP
import qualified Data.SOP.Constraint as SOP

import qualified Type.Reflection.Name as TRN
import qualified Instance.Runtime as Instances

import qualified Data.Text as T

import qualified Data.Vector as V

import Type.Reflection
import Data.Proxy
import Data.Tuple
import Data.Foldable
import Control.Applicative

-- Helpful for writing ToJSON instances
instance JSON.ToJSON (Key r) where
  toJSON = JSON.String . getKeyText

-- | Describes errors encountered in the encoding process
data WriteJSONError
  -- | An Eiger record is missing an 'IsJSONRecord' instance. This might
  -- be because you forgot to say @\$deriveJSONRecordInstances@.
  = MissingIsJSONRecordInstance TyCon  -- ^ of the problematic record

  -- | A field in a record is missing a ToJSON instance
  | MissingToJSONInstance String       -- ^ field name
                          TyCon        -- ^ of record
                          SomeTypeRep  -- ^ of the problematic field

renderJSONError :: WriteJSONError -> T.Text
renderJSONError (MissingIsJSONRecordInstance record_tc)
  = mconcat [ "No IsJSONRecord instance for ", tshow record_tc
            , "; did you forget to `$deriveJSONInstances`?" ]
renderJSONError (MissingToJSONInstance field record_tc (SomeTypeRep field_rep))
  = mconcat [ "No ToJSON instance for ", T.pack field, " field of ", tshow record_tc
            , " of type ", tshow field_rep, "." ]

-- | Convert a 'Database' to a 'JSON.Value' including all entries in the database.
-- Uses the encoding described in "Eiger.JSON". Any entry in the database that does
-- not have an 'IsRecord' instance in the 'InstanceDatabase' will get mapped to @null@.
-- Any errors encountered while writing are accumulated in the second return value;
-- if this is non-empty, there is likely missing information in the first return value.
-- However, the 'JSON.Value' output will still be well-formed, just less informative.
databaseToJSON :: InstanceDatabase -> Database -> (JSON.Value, [WriteJSONError])
databaseToJSON instances db = swap $ JSON.Object <$> object_mapping
  where
    object_mapping = fold <$> mapM to_key_map (getAllPairs db)

    to_key_map :: DS.DSum Key DatabaseValue -> M (JSON.KeyMap JSON.Value)
    to_key_map ((DS.:=>) key (MkDbV record_rep record)) = do
      converted <- sequence $
                   Instances.withInstanceTypeRep (is_record instances) record_rep $
                   convertRecord instances record
      val <- case converted of
        Nothing     -> ([MissingIsJSONRecordInstance (typeRepTyCon record_rep)], JSON.Null)
        Just val -> return val
      return (JSON.singleton (convertKey key) val)

-----------------------------------------
type M = (,) [WriteJSONError]   -- we use this as our lightweight accumulator monad throughout

getKeyText :: Key record -> T.Text
getKeyText key = case getUserKey key of
  ExternalUserKey t -> t
  InternalUserKey ik -> "$" <> tshow (getInternalKeyUnique ik)

convertKey :: Key record -> JSON.Key
convertKey = JSON.fromText . getKeyText

convertRecord ::
  forall record.
  IsJSONRecord record =>
  InstanceDatabase ->
  record ->
  M JSON.Value
convertRecord instances record = getConstructorInfo @record $ \ constructor_info -> do
    fields <- go_ctor constructor_info (SOP.productTypeFrom record)
    return $ JSON.Object $
      JSON.insert "type" (JSON.String (unTypeNamer $ TRN.renderTypeRep (typeRep @record))) fields

  where
    go_ctor ::
      forall fields.
      SOP.All Typeable fields =>
      SOP.ConstructorInfo fields ->
      SOP.NP SOP.I fields ->
      M (JSON.KeyMap JSON.Value)
    go_ctor (SOP.Record _name field_infos) fields = do
      zipped :: SOP.NP (SOP.K (JSON.KeyMap JSON.Value)) fields
             <- hczipWithM (Proxy @Typeable) go_field field_infos fields
      return (hfold zipped)

    go_ctor (SOP.Constructor {}) _ = error "impossible: found a regular constructor using SOP"
    go_ctor (SOP.Infix {}) _ = error "impossible: found an infix constructor using SOP"

    go_field ::
      Typeable field =>
      SOP.FieldInfo field ->
      SOP.I field ->
      M (SOP.K (JSON.KeyMap JSON.Value) field)
    go_field (SOP.FieldInfo "key") _ = return $ SOP.K mempty  -- keys are not serialized this way
    go_field (SOP.FieldInfo field_name) (SOP.I field_data) =
      SOP.K <$> (JSON.singleton (JSON.fromString field_name) <$> (go_data field_name field_data))

    go_data :: forall field_type. Typeable field_type => String -> field_type -> M JSON.Value
    go_data field_name field_data = case typeRep @field_type of
      App f_rep arg_rep
        | Just HRefl <- f_rep `eqTypeRep` typeRep @(Fact Optional)
        -> case field_data of
             MustDerive -> return JSON.Null
             AlreadyKnown x -> go_field_type field_name arg_rep x

        | Just HRefl <- f_rep `eqTypeRep` typeRep @(Fact Input)
        -> go_field_type field_name arg_rep (unInputFact field_data)

      _ -> error "Field type is not a Fact."

    go_field_type :: forall field_type. String -> TypeRep field_type -> field_type -> M JSON.Value
    go_field_type field_name field_type_rep field_data = case field_type_rep of
      App f_rep arg_rep
        | Just HRefl <- f_rep `eqTypeRep` typeRep @Key
        -> return $ JSON.toJSON field_data   -- use ToJSON instance above, but it won't be in the
                                             -- instance database for runtime lookup

        | Just HRefl <- f_rep `eqTypeRep` typeRep @[]
        -> JSON.Array <$> (V.fromList <$> (mapM (withTypeable arg_rep (go_data field_name)) field_data))

      _ | Just val <- Instances.withInstanceTypeRep (to_json instances) field_type_rep $
                      JSON.toJSON field_data
        -> return val

        | otherwise
        -> ( [MissingToJSONInstance field_name (getRecordTyCon record) (SomeTypeRep field_type_rep)]
           , JSON.Null )

---------------------------------------
-- Utilities

hfold :: (SOP.SListIN h xs, SOP.HTraverse_ h, Monoid m) => h (SOP.K m) xs -> m
hfold xs = SOP.unK (SOP.htraverse_ (SOP.mapKK id) xs)

-- This could be more general, but life is short.
hczipWithM ::
  SOP.All c xs =>
  Applicative m =>
  proxy c ->
  (forall a. c a => f a -> f' a -> m (f'' a)) ->
  SOP.NP f xs ->
  SOP.NP f' xs ->
  m (SOP.NP f'' xs)
hczipWithM _ _ SOP.Nil SOP.Nil = pure SOP.Nil
hczipWithM p f (x SOP.:* xs) (y SOP.:* ys) = liftA2 (SOP.:*) (f x y) (hczipWithM p f xs ys)