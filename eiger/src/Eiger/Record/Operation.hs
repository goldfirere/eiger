{-# LANGUAGE DataKinds, AllowAmbiguousTypes, TypeFamilies, UndecidableInstances,
             TemplateHaskellQuotes, ViewPatterns, LambdaCase, UndecidableSuperClasses #-}

{-|
Description : User interface for Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This module defines operations client code will use to access and Eiger
records.

-}

module Eiger.Record.Operation (
  -- TODO: haddock
  get, getMissingDependencies, compute,
  checkFieldName, checkFieldNameConstrained, DataModel, mkDataModel,
  getAll,
  failWith,

  -- Re-export:
  Proxy(..),
  ) where

import Eiger.Util as H
import Eiger.Record.Core
import Eiger.Record.Class

import qualified Instance.Runtime as RI
import qualified Instance.Runtime.TH as RI
import qualified Type.Reflection.Name as TRN
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.Set as S

import qualified GHC.TypeLits as TL
import Data.Proxy
import Language.Haskell.TH as TH
import Type.Reflection
import qualified Data.Monoid as Monoid
import Control.Monad

-----------------------------------------------------------
-- Retrieving information from a field

-- | Extract the information from a field, either by reading it off or computing
-- it from other fields via defined rules. Pass in the field name as a type-level
-- string, like @get @"field_name" record@.
get ::
  forall (field_name :: Symbol) record.
  HasFact field_name record =>
  Key record ->
  EigerM (FactType record field_name)
get key = getFact @field_name =<< getRecordFromKey key

-- | Extract a list of field names that must be filled in before we can compute
-- the value of the given field. This additionally returns all 'TyCon's for record
-- types that were collected from the database; adding a new record of one of these
-- types might create new dependencies.
getMissingDependencies ::
  forall field_name record.
  HasFact field_name record =>
  Key record ->
  EigerM ([String], [TyCon])
    -- GHC BUG #21209: should need only @field_name below, not the other type args
getMissingDependencies key = do
  (result, deps) <- catchDependencies $
    catchEigerM errors_fun (getFact @field_name =<< getRecordFromKey key)
  let type_deps = S.toList $ typeDependencies deps
  return $ case result of
    Left errs -> (map render_dep errs, type_deps)
    Right _   -> ([], type_deps)
  where
    render_dep :: (String, String) -> String
    render_dep (missing_key, field_name) = missing_key ++ "." ++ field_name

    errors_fun errs
      | null non_dep_errs = Right dep_errs
      | otherwise         = Left (Seq.fromList non_dep_errs)
      where
        (dep_errs, non_dep_errs) = flip partitionWith (F.toList errs) $ \ case
          MissingField _ name key_string -> Left (key_string, name)
          other                          -> Right other

-----------------------------------------------
-- Interpreting a runtime-supplied field name

-- | The 'UncurriedHasFact' class is an uncurried version of 'HasFact',
-- for use with 'Instances'. It is also missing the @field_type@ parameter,
-- as that can be derived from the others.
class ( Typeable (Snd name_record) ,
        HasFact (Fst name_record) (Snd name_record) ,
        Typeable (FactType (Snd name_record) (Fst name_record)) ) =>
    UncurriedHasFact name_record
instance ( Typeable record ,
           HasFact field_name record ,
           Typeable (FactType record field_name) ) =>
    UncurriedHasFact '(field_name, record)

-- | 'DataModel' is a database of 'HasFact' instances that can be
-- queried at runtime to validate field names. Build with 'mkDataModel'.
newtype DataModel = MkHFI (RI.Instances TRN.PackageQualified UncurriedHasFact)

-- | Create a 'DataModel' for use in 'checkFieldName'. This will find all
-- records in scope with a 'IsRecord' instance. However, beware: this will miss
-- any records declared in the same module as the occurrence of 'mkDataModel'.
-- If you must pull these in, separate this call from the instance declarations by
-- a line that looks like
--
-- @
--    $(pure [])
-- @
mkDataModel :: Q Exp
mkDataModel = do
  record_types <- RI.allGroundInstanceTypes (ConT ''IsRecord)
  pairss <- mapM mk_pairs record_types
  [| MkHFI $ RI.instancesForInvisible @( $( return $ RI.promotedList (concat pairss) )) |]

  where
    mk_pairs :: TH.Type -> Q [TH.Type]
    mk_pairs record_ty@(ConT record_name) = do
      record_info <- reify record_name
      return $ case record_info of
        TyConI (DataD _cxt _name _tvs _ksig [RecC _con_name field_tys] _derivs) ->
          map go_field_ty field_tys
        TyConI (NewtypeD _cst _name _tvs _ksig (RecC _con_name field_tys) _derivs) ->
          map go_field_ty field_tys
        _ -> []

        where
          go_field_ty :: VarBangType -> TH.Type
          go_field_ty (field_name, _bang, _type) =
            PromotedTupleT 2 `AppT` LitT (StrTyLit (nameBase field_name)) `AppT` record_ty

    mk_pairs _other = return []

-- | Interpret a 'Text' as a field name of a given record. This is written
-- in continuation-passing style so that the continuation ("callback" function,
-- if you prefer) has access to the 'HasFact' instances that allows using
-- other Eiger functions. Fails in the 'EigerM' monad if the field does not
-- exist in the given record.
--
-- Example usage: @checkFieldName $mkDataModel (field_name :: Text) $ \ (_ :: Proxy field_name) -> ...@
checkFieldName ::
  forall (record :: Record) r.
  Typeable record =>
  DataModel ->
  Text ->
  (forall (field_name :: Symbol).
   HasFact field_name record =>
   Proxy field_name ->
   TypeRep (FactType record field_name) ->
   EigerM r) ->
  EigerM r
checkFieldName
  (MkHFI instances)
  field_name@(TL.someSymbolVal . T.unpack -> TL.SomeSymbol @field_name _)
  k =
    -- can't easily implement this in terms of checkFieldNameConstrained,
    -- because I can't create an Instances that will succeed with every type.
  case RI.withInstanceTypeRep instances (typeRep @'(field_name, record)) $
       k @field_name Proxy typeRep of
    Nothing -> failWithError (UnknownField (typeRepTyCon (typeRep @record)) (T.unpack field_name))
    Just r  -> r

-- | Like 'checkFieldName', but ensures that a given field has an instance
-- of a particular class.
checkFieldNameConstrained ::
  forall (record :: Record) c r type_text.
  Typeable record =>
  Typeable c =>
  TRN.TypeText type_text =>
  DataModel ->
  RI.Instances type_text c ->
  Text ->
  (forall (field_name :: Symbol).
   HasFact field_name record =>
   c (FactType record field_name) =>
   Proxy field_name ->
   TypeRep (FactType record field_name) ->
   EigerM r) ->
  EigerM r
checkFieldNameConstrained
  (MkHFI instances)
  c_instances
  field_name@(TL.someSymbolVal . T.unpack -> TL.SomeSymbol @field_name _)
  k =
  case RI.withInstanceTypeRep instances (typeRep @'(field_name, record)) $
       RI.withInstanceTypeRep c_instances (typeRep @(FactType record field_name)) $
       k @field_name Proxy typeRep of
     -- each withInstanceTypRep adds a Maybe layer
    Just (Just r) -> r
    Just Nothing -> failWithError (UnsatisfiableConstraint (typeRepTyCon (typeRep @record)) (T.unpack field_name) (SomeTypeRep $ typeRep @c))
    Nothing -> failWithError (UnknownField (typeRepTyCon (typeRep @record)) (T.unpack field_name))

-------------------------------------------------------
-- Run all computations

-- | Propagate all information in the database following all rules.
compute :: DataModel -> EigerM ()
compute (MkHFI instances) = do
  db <- getDatabase
  Monoid.getAp $ RI.foldInstances instances $ \ (_ :: Proxy name_record) ->
    Monoid.Ap $ go_HasFact @(Fst name_record) @(Snd name_record) db

  where
    go_HasFact ::
      forall (field_name :: Symbol) (record :: Record).
      HasFact field_name record =>
      Database ->
      EigerM ()
    go_HasFact db = do
      let keys = getAllKeysOfType @record db
      mapM_ (go_record @field_name) keys

    go_record ::
      forall (field_name :: Symbol) (record :: Record).
      HasFact field_name record =>
      Key record ->
      EigerM ()
    go_record key =
        -- ignore any fields missing rules
      catchEigerM_ (Seq.filter (not . isMissingField)) $
      void $ get @field_name key

    isMissingField (MissingField {}) = True
    isMissingField _                 = False

-------------------------------------------------------
-- Operations used in rules.

-- | Retrieve all records of the given type from the database
getAll :: forall (record :: Record). Typeable record => ComputationRule [Key record]
getAll = MkRule $ accessAllKeysOfType @record


-------------------------------------------------------
-- Failure

-- | Aborts the computation with a message to be printed to the user.
failWith :: String -> ComputationRule a
failWith msg = MkRule $ failWithError (RuntimeError msg)