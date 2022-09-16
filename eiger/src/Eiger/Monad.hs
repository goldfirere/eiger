{-# LANGUAGE DataKinds, AllowAmbiguousTypes, TypeFamilies, DeriveAnyClass,
             DerivingStrategies #-}


{-|
Description : A monadic context to use when accessing an Eiger field
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

-}

module Eiger.Monad (
  -- TODO: put this in a logical order
  Error(..), pprError,
  EigerM, runEigerM, succeedEigerM, failEigerM,
  catchEigerM_, catchEigerM, catchDependencies,
  recordFieldDependency, recordTypeDependency, updateDatabase,
  Dependencies, typeDependencies, fieldDependencies,
  failWithError,
  missingRule, getRecordFromKey, getDatabase,
  ) where

import Eiger.Util
import Eiger.Record.Type
import Eiger.Database
import Eiger.Key

import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Some

import Control.Applicative
import Type.Reflection
import Control.Monad
import GHC.TypeLits
import Data.Proxy
-------------------------------------------------
-- Errors

-- | The type of errors that can happen during an Eiger computation.
type Error :: Type
data Error
  -- | No rule was specified for an 'Optional' field, but that field
  -- was not present in the input and was needed during computation.
  = MissingField
      TyCon    -- ^ record type
      String   -- ^ name of field
      String   -- ^ key of record

  -- | A field provided at runtime does not exist in the record expected.
  | UnknownField
      TyCon    -- ^ record type
      String   -- ^ name of field

  -- | A missing key in the database
  | MissingKey
      String  -- ^ key
      TyCon   -- ^ record type

  -- | A constraint that could not be satisfied in 'checkFieldNameConstrained'
  | UnsatisfiableConstraint
      TyCon        -- ^ record type
      String       -- ^ field name
      SomeTypeRep  -- ^ the constraint

  -- | A configurable runtime error
  | RuntimeError
      String      -- ^ error message for user

  deriving (Generic, NFData, Show)

-- | Create a 'MissingKey' error
missingKey :: forall record. Typeable record => Key record -> Error
missingKey key = MissingKey (show key) (typeRepTyCon (typeRep @record))

-- | Render an error suitable for an end user.
pprError :: Error -> String
pprError (MissingField record_tc field_name key) =
  concat
    [ "No value or rule for field "
    , field_name
    , " in record "
    , key
    , " of type "
    , tyConName record_tc
    , "."
    ]
pprError (UnknownField record_tc field_name) =
  concat
    [ "No field named ", field_name, " in record ", tyConName record_tc, "."]
pprError (MissingKey key record_tc) =
  concat
    [ "Key ", key, " for a record of type ", tyConName record_tc, " is missing." ]
pprError (UnsatisfiableConstraint record_tc field_name constraint_rep) =
  concat
    [ "Could not satisfy a ", show constraint_rep
    , " constraint when accessing field ", field_name, " from "
    , tyConName record_tc, "."]
pprError (RuntimeError msg) = msg

--------------------------------------------------
-- The EigerM monad

-- | A monad for running Eiger computations. Allows tracking of errors and
-- database update.
type EigerM :: Type -> Type
newtype EigerM a
  = MkEigerM (Database -> (Database, Dependencies, EigerMResult a))
  deriving Functor

-- | A type for tracking dependencies of a computation.
data Dependencies = MkDeps
  { fieldDependencies :: S.Set (Some Key, String)
  , typeDependencies :: S.Set TyCon }

instance Semigroup Dependencies where
  MkDeps { fieldDependencies = fdeps1, typeDependencies = tdeps1 } <>
    MkDeps { fieldDependencies = fdeps2, typeDependencies = tdeps2 } =
    MkDeps
      { fieldDependencies = fdeps1 <> fdeps2
      , typeDependencies = tdeps1 <> tdeps2 }

instance Monoid Dependencies where
  mempty = MkDeps { fieldDependencies = mempty, typeDependencies = mempty }

-- | The result sum type for Eiger computations.
data EigerMResult a
  = Errors (Seq.Seq Error)
  | Success a
  deriving Functor

instance Applicative EigerM where
  pure x = MkEigerM $ \ db -> (db, mempty, Success x)
  liftA2 f (MkEigerM action1) (MkEigerM action2) =
    MkEigerM $ \ db0 -> case action1 db0 of
      (db1, deps1, Errors errs1) -> case action2 db1 of   -- IMPORTANT: continue executing
                                                          -- this is key for accumulating errors
        (db2, deps2, Errors errs2) -> (db2, deps1 <> deps2, Errors (errs1 <> errs2))
        (db2, deps2, Success {})   -> (db2, deps1 <> deps2, Errors errs1)
      (db1, deps1, Success x) -> case action2 db1 of
        (db2, deps2, Errors errs2) -> (db2, deps1 <> deps2, Errors errs2)
        (db2, deps2, Success y) -> (db2, deps1 <> deps2, Success (f x y))

instance Monad EigerM where
  MkEigerM action1 >>= f_action2 =
    MkEigerM $ \ db0 ->
    case action1 db0 of
      (db1, deps1, Errors errs1) -> (db1, deps1, Errors errs1)
      (db1, deps1, Success x) -> case f_action2 x of
        MkEigerM action2 -> case action2 db1 of
          (db2, deps2, result) -> (db2, deps1 <> deps2, result)

  (>>) = (*>)  -- use Applicative combination to preserve errors

-- | Execute an Eiger computation
runEigerM :: Database -> EigerM a -> Either [Error] (Database, a)
runEigerM db (MkEigerM action) = case action db of
  (_, _, Errors errs) -> Left (F.toList errs)
  (db', _, Success x) -> Right (db', x)

-- | Execute an Eiger computation, assuming it will succeed. Crashes if the
-- computation fails. Useful for testing (only).
succeedEigerM :: Database -> EigerM a -> a
succeedEigerM db (MkEigerM action) = case action db of
  (_, _, Errors errs) -> error ("EigerM computation failed:\n" ++ unlines (map pprError (F.toList errs)))
  (_db, _deps, Success x) -> x

-- | Execute an Eiger computation, assuming it will fail. Crashes if the
-- computation succeeds. Useful for testing (only).
failEigerM :: Database -> EigerM a -> [Error]
failEigerM db (MkEigerM action) = case action db of
  (_db, _deps, Errors errs) -> F.toList errs
  (_db, _deps, Success {}) -> error "EigerM computation succeeded."

-- | Fail in the 'EigerM' monad with the given error.
failWithError :: Error -> EigerM a
failWithError err = MkEigerM $ \ db -> (db, mempty, Errors (Seq.singleton err))

-- | Throw an error recording an absent rule.
missingRule ::
  forall (field_name :: Symbol) record a.
  KnownSymbol field_name =>
  Typeable record =>
  HasKey record =>
  record ->
  EigerM a
missingRule record = failWithError err
  where
    err = MissingField (getRecordTyCon record) (symbolString @field_name) (show (getRecordField @"key" record))

-- | Look up a key in the monad's database
getRecordFromKey :: forall record. Typeable record => Key record -> EigerM record
getRecordFromKey key = do
  db <- getDatabase
  case databaseLookup db key of
    Nothing -> failWithError $ missingKey key
    Just record -> return record

-- | Retrieve the database from the monad
getDatabase :: EigerM Database
getDatabase = MkEigerM $ \ db -> (db, mempty, Success db)

-- | Catch errors from a subcomputation. The provided function filters out any errors
-- it needs to; any left are re-thrown. If the filtered errors are empty, then the
-- call to 'catchEigerM_' succeeds.
catchEigerM_ :: (Seq.Seq Error -> Seq.Seq Error) -> EigerM a -> EigerM ()
catchEigerM_ error_fun action = void $ catchEigerM error_fun_wrapper action
  where
    error_fun_wrapper :: Seq.Seq Error -> Either (Seq.Seq Error) ()
    error_fun_wrapper errs
      | null processed_errs = Right ()
      | otherwise           = Left processed_errs
      where processed_errs = error_fun errs

-- | Catch errors from a subcomputation. The catcher can either return a list of new
-- errors to throw (typically, but not necessarily, a subset of the original list) or
-- can return a new result. 'catchEigerM' returns @Left result@ if errors were thrown
-- and the catcher returns @result@ or @Right result@ if the action throws no errors
-- and returns @result@.
catchEigerM :: (Seq.Seq Error -> Either (Seq.Seq Error) a) -> EigerM b -> EigerM (Either a b)
catchEigerM error_fun (MkEigerM action) = MkEigerM $ \ db0 -> case action db0 of
  (db1, deps1, Errors errs) -> case error_fun errs of
    Left filtered_errs -> (db1, deps1, Errors filtered_errs)
    Right a -> (db1, deps1, Success (Left a))
  (db1, deps1, Success b) -> (db1, deps1, Success (Right b))

-- | Runs a subcomputation, tracking the dependencies of the
-- computation. Once dependencies are retrieved
-- by this function, they are considered dealt with and are no longer retained in the
-- 'EigerM' monad; use 'recordDependency' to re-add any if desired.
--
-- If the inner action fails, 'catchDependencies' fails, too, leaving the tracked
-- dependencies untouched.
catchDependencies :: EigerM a -> EigerM (a, Dependencies)
catchDependencies (MkEigerM action) = MkEigerM $ \ db0 -> case action db0 of
  (db1, deps1, Errors errs) -> (db1, deps1, Errors errs)
  (db1, deps1, Success x) -> (db1, mempty, Success (x, deps1))

-- | Record that a search was done for all records in the database of type @record@.
recordTypeDependency :: forall (record :: Record). Typeable record => EigerM ()
recordTypeDependency = MkEigerM $ \ db ->
  ( db
  , mempty { typeDependencies = S.singleton (typeRepTyCon (typeRep @record)) }
  , Success ())

-- | Record that a field was accessed.
recordFieldDependency ::
  forall field_name record.
  KnownSymbol field_name =>
  Key record ->
  EigerM ()
recordFieldDependency key = MkEigerM $ \ db ->
  ( db
  , mempty { fieldDependencies =
      S.singleton (Some key, symbolVal @field_name Proxy) }
  , Success ())

-- | Apply an update to the database. Do not change any record keys!
updateDatabase :: forall record. Typeable record => Key record -> (record -> record) -> EigerM ()
updateDatabase key upd = MkEigerM $ \ db ->
  case updateDb db key upd of
    Nothing -> (db, mempty, Errors $ Seq.singleton $ missingKey key)
    Just new_db -> (new_db, mempty, Success ())