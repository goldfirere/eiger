{-# LANGUAGE OverloadedStrings, DataKinds, TypeFamilies, AllowAmbiguousTypes,
             UndecidableSuperClasses, PatternSynonyms, ViewPatterns,
             LambdaCase, ImpredicativeTypes #-}

{-|
Description : JSON parsing for an Eiger database
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This parses a JSON Value into an Eiger database.

-}

module Eiger.JSON.Read (
  databaseFromJSON,
  ) where

import Eiger.JSON.Util

import Eiger.JSON.Class
import Eiger.JSON.Instance

import Eiger.Record hiding ( pattern MissingField )
import Eiger.Key
import qualified Eiger.DSL.Number as DSL

import Instance.Runtime

import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Key as JSON

import qualified Data.Scientific as Sci

import qualified Generics.SOP as SOP

import qualified Type.Reflection as Type

import Data.Text ( Text )
import qualified Data.Text as T
import Data.Proxy
import Control.Monad
import Data.String
import Type.Reflection

----------------------------------------------------
-- external interface

-- | Convert a parsed JSON 'JSON.Value' into a 'Database'
databaseFromJSON ::
  InstanceDatabase ->                 -- ^ the 'IsRecord' instances for all record types
                                      -- that might appear in the JSON to be loaded,
                                      -- along with 'Integral', 'RealFloat', and 'IsString'
                                      -- instances for data fields
  JSON.Value ->                       -- ^ the parsed JSON that should be loaded
  Database ->                         -- ^ an initial database to extend (could be 'emptyDatabase')
  Either Text Database                -- ^ Either an error message or an extended database
databaseFromJSON instances value db
  = runM (convertEntities value) instances db

-------------------------------------------------
-- Our internal monad.

newtype M a =
  MkM (InstanceDatabase ->  -- the instance database
       Database ->          -- the database so far
       MResult a)
  deriving Functor

data MResult a = Failure Text
               | Success a (S.Set Text) Database
                  -- builds up the set of foreign keys used, to make
                  -- sure they're all present in the final database
  deriving Functor

runM :: M () -> InstanceDatabase -> Database -> Either Text Database
runM (MkM f) instances initial_db =
  case f instances initial_db of
    Failure err -> Left err
    Success () used_keys final_db
      | S.null missing_keys -> Right final_db
      | otherwise -> Left ("The following keys were not bound to entities: " <> tshow missing_keys)
      where
        missing_keys = used_keys S.\\ allTextKeys final_db

conversionFail :: Text -> M a
conversionFail err = MkM (\ _ _ -> Failure err)

register ::
  HasKey record =>
  Typeable record =>
  Maybe Text ->      -- the JSON key, if one exists
  record ->          -- the object to add
  M (Key record)
register m_key obj =
  MkM $ \ _ db ->
  case addDb db m_key obj of
    Nothing -> Failure ("Duplicate user-specified key " <> expectJust "register" m_key <> ".")
    Just (db_key, db') -> Success db_key mempty db'

withType ::
  (InstanceDatabase -> Instances TypeNamer c) ->
  Text ->
  (forall t.
   c t =>
   Proxy t ->
   M a) ->
  M a
-- TODO: remove Proxy argument once #448 has arrived
withType getter type_name k =
  MkM $ \ instances db ->
  case withInstanceProxy (getter instances) (mkTypeNamer type_name) k of
    Nothing      -> Failure ("Unknown type " <> type_name)
    Just (MkM f) -> f instances db

withTypeRep ::
  (InstanceDatabase -> Instances TypeNamer c) ->
  TypeRep t ->
  (c t => M a) ->
  M a
withTypeRep getter type_rep k =
  MkM $ \ instances db ->
  case withInstanceTypeRep (getter instances) type_rep k of
    Nothing      -> Failure ("Unknown type " <> tshow type_rep)
    Just (MkM f) -> f instances db

retrieveKey :: Typeable record => Text -> M (Key record)
retrieveKey text_key =
  MkM $ \ _ db ->
  Success (mkTextKey text_key) (S.singleton text_key) db

unexpected :: (Show a, Typeable a) => a -> M b
unexpected thing = conversionFail ("Unexpected " <> tshow (Type.typeOf thing) <> ": " <> tshow thing)

instance Applicative M where
  pure x = MkM $ \ _ db -> Success x mempty db
  (<*>) = ap

instance Monad M where
  MkM f1 >>= fmb =
    MkM $ \ instances db0 ->
    case f1 instances db0 of
      Failure err -> Failure err
      Success x used_keys1 db1 -> case fmb x of
        MkM f2 -> case f2 instances db1 of
          failure@(Failure {}) -> failure
          Success r used_keys2 db2 -> Success r (used_keys1 <> used_keys2) db2

--------------------------------------------------
-- The converter

-- | Convert an @entities@.
convertEntities :: JSON.Value -> M ()
convertEntities (JSON.Object mapping) = convertGroupEntries (JSON.toList mapping)
convertEntities (JSON.Array values) = mapM_ (convertEntity Nothing) values
convertEntities other = unexpected other

-- | Convert an @entity@
convertEntity :: Maybe Text -> JSON.Value -> M ()
convertEntity json_key (JSON.Object mapping)
  | Just (JSON.String type_name) <- JSON.lookup "type" mapping
  = withType is_record type_name $ \ (_ :: Proxy record) ->
    void $ convertEntityEntries @record json_key (JSON.delete "type" mapping)

  -- an entity is required to be an Object with a "type" field; fail otherwise
convertEntity _ other
  = conversionFail $ "An entity must be encoded as a JSON object with a \"type\" field:\n" <>
                     tshow other

-- Convert an @entity_entries@, at a known type.
convertEntityEntries ::
  forall (record :: Record).
  IsJSONRecord record =>
  Maybe Text ->                -- key string, if one exists
  JSON.KeyMap JSON.Value ->
  M (Key record)
convertEntityEntries json_key entries = do
  obj <- getConstructorInfo @record $ \ constructor_info ->
             SOP.productTypeTo <$> go_ctor constructor_info
  register @record json_key obj
  where
    go_ctor ::
      SOP.All Typeable fields =>
      SOP.ConstructorInfo fields ->
      M (SOP.NP SOP.I fields)
       -- this will extract all of the fields in order and return the results as an NP,
       -- to be converted by SOP.productTypeTo.
    go_ctor (SOP.Record _conname fields) = SOP.hctraverse (Proxy @Typeable) go_field fields

      -- these are impossible because of the IsOneRecordConstructorDatatypeInfo check
      -- as part of HasOneRecordConstructor, but the SOP library does not propagate this
      -- information
    go_ctor (SOP.Constructor {}) = error "impossible: found a regular constructor using SOP"
    go_ctor (SOP.Infix {}) = error "impossible: found an infix constructor using SOP"

      -- convert one field, of a known type. This iterates through the declared fields
      -- of the type, NOT the fields present in the JSON file.
    go_field ::
      forall field_type.
      Typeable field_type =>
      SOP.FieldInfo field_type ->
      M field_type
      -- special handling of the "key" field: we have to wait until later until we know the key
      -- so return a fakeKey.
    go_field (SOP.FieldInfo "key") = case typeRep @field_type of
      Type.App key record
        | Just Type.HRefl <- key `Type.eqTypeRep` typeRep @Key
        , Just Type.HRefl <- record `Type.eqTypeRep` typeRep @record
        -> return fakeKey
      _ -> error "impossible: the key field of a record does not have the right type"

      -- handle a non-"key" field
    go_field (SOP.FieldInfo field_name) =
      case JSON.lookup (JSON.fromString field_name) entries of
        MissingField
            -- missing or null entry for optional field: OK
          | OptionalTR _ <- typeRep @field_type
            -> return MustDerive

            -- missing or null entry for required ('Input') field: not OK
          | otherwise ->
            conversionFail $ "The " <> T.pack field_name <> " field of " <> tshow (typeRep @record) <>
                             " is required, but it is missing" <>
                             (case json_key of Just jk -> " in record " <> jk
                                               _       -> mempty) <>
                             "."

          -- real entry
        Just val -> go_present_field (T.pack field_name) (typeRep @field_type) val

      -- precondition: the JSON.Value is not null.
    go_present_field ::
      Text ->   -- field name, for errors only
      TypeRep field_type -> JSON.Value -> M field_type
    go_present_field field_name field_type_rep val = case field_type_rep of

        -- optional value: recur
      OptionalTR inner_field_type_rep -> do
        field_value <- go_field_type field_name inner_field_type_rep val
        return (AlreadyKnown field_value)

      InputTR inner_field_type_rep -> do
        field_value <- go_field_type field_name inner_field_type_rep val
        return (InputFact field_value)

      _ -> conversionFail $ "The " <> field_name <> " field of " <> tshow (typeRep @record) <>
                            " is not a 'Fact' type. All fields in an Eiger record (other than the key) " <>
                            "must be 'Fact' types."

      -- precondition: the JSON.Value is not null.
      -- This is called after discerning the outer structure of the field, whether
      -- it's a Fact Input or Fact Optional
    go_field_type :: Text -> TypeRep field_type -> JSON.Value -> M field_type
    go_field_type field_name field_type_rep val = case field_type_rep of

        -- expecting a key
      KeyTR (record_rep :: TypeRep inner_record) -> withTypeRep is_record record_rep $ case val of
          -- found a key string to use for a key field
        JSON.String key_text -> retrieveKey key_text
          -- found an inline object to use for a key field
        JSON.Object inner_obj_entries -> do
          convertEntityEntries @inner_record Nothing inner_obj_entries

          -- all other possibilities are failures
        _ -> conversionFail $ "Field " <> field_name <> " is a key into another entity, but " <>
                              "its value is neither a string nor an object:\n" <>
                              tshow val

        -- list value
      ListTR element_rep -> case val of
        JSON.Array vals -> do
          field_values <- V.mapM (go_present_field field_name element_rep) (V.filter (not . jsonNull) vals)
          return $ V.toList field_values

        _ -> conversionFail $ "Field " <> field_name <> " is a list, but its value is not a JSON array."

        -- non-key value: convert using FromJSON instance
      _ -> case val of
        JSON.Object {} -> conversionFail $ "Field " <> field_name <> " is not a key, but its value " <>
                                           "is an object:\n" <>
                                           tshow val

        JSON.Array {} -> conversionFail $ "Field " <> field_name <> " is not a list, but its value is " <>
                                          " an array:\n" <>
                                          tshow val

        JSON.String str -> withTypeRep is_string field_type_rep $ return (fromString $ T.unpack str)

        JSON.Number n -> case floatingOrInteger n of
          Left float -> withTypeRep real_float field_type_rep $ return float
          Right int  -> withTypeRep from_int   field_type_rep $ return int

        JSON.Bool b
          | Just Type.HRefl <- field_type_rep `Type.eqTypeRep` typeRep @Bool -> return b
          | otherwise -> conversionFail $ "Field " <> field_name <> " is not a Bool, but its value is."

        JSON.Null -> error "impossible: JSON.Null in go_present_field"

convertGroupEntries :: [(JSON.Key, JSON.Value)] -> M ()
convertGroupEntries = mapM_ convertGroupEntry

convertGroupEntry :: (JSON.Key, JSON.Value) -> M ()
convertGroupEntry ("data", entities) = convertEntities entities
convertGroupEntry (key, entity) = convertEntity (Just $ JSON.toText key) entity

-----------------------------------------------
-- Utilities

-- | This matches either Nothing or Just 'JSON.Null'; useful for matching against
-- the result of a lookup in a 'JSON.KeyMap'.
pattern MissingField :: Maybe JSON.Value
pattern MissingField <- (maybe True (\case JSON.Null -> True; _ -> False) -> True)

{-# COMPLETE MissingField, Just #-}

-- | Match against a type like @Fact Optional field_type@
pattern OptionalTR ::
  forall field_type.
  () =>
  forall inner_field_type.
  field_type ~ Fact Optional inner_field_type =>
  TypeRep inner_field_type ->
  TypeRep field_type
pattern OptionalTR inner <-
  Type.App (Type.eqTypeRep (typeRep @(Fact Optional)) -> Just Type.HRefl) inner

-- | Match against a type like @Fact Input field_type@
pattern InputTR ::
  forall field_type.
  () =>
  forall inner_field_type.
  field_type ~ Fact Input inner_field_type =>
  TypeRep inner_field_type ->
  TypeRep field_type
pattern InputTR inner <-
  Type.App (Type.eqTypeRep (typeRep @(Fact Input)) -> Just Type.HRefl) inner


-- | Match against a type like @Key record@
pattern KeyTR ::
  forall field_type.
  () =>
  forall record.
  field_type ~ Key record =>
  TypeRep record ->
  TypeRep field_type
pattern KeyTR record <-
  Type.App (Type.eqTypeRep (typeRep @Key) -> Just Type.HRefl) record

-- | Match against a type like @List blah@
pattern ListTR ::
  forall field_type.
  () =>
  forall record.
  field_type ~ [] record =>
  TypeRep record ->
  TypeRep field_type
pattern ListTR record <-
  Type.App (Type.eqTypeRep (typeRep @[]) -> Just Type.HRefl) record

-- check whether a value is Null
jsonNull :: JSON.Value -> Bool
jsonNull JSON.Null = True
jsonNull _         = False

floatingOrInteger :: Sci.Scientific -> Either (forall r. RealFloat r => r) (forall i. DSL.FromInt i => i)
floatingOrInteger s
  | Sci.isFloating s = Left (Sci.toRealFloat s)
  | otherwise
  = Right $ case Sci.floatingOrInteger @Double s of Left _ -> error "impossible: inconsistent Scientific conversion"
                                                    Right n -> DSL.fromInt (fromInteger n)

---------------------------------------------------
-- Maybe

-- | Expect a @Maybe a@ to be a 'Just', returning its
-- contents. If the input is not a 'Just', throws an
-- exception including the string given (for debugging).
expectJust :: String -> Maybe a -> a
expectJust _ (Just x)      = x
expectJust err_msg Nothing = error $ "expectJust: " ++ err_msg