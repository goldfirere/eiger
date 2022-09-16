{-# LANGUAGE DataKinds, TypeFamilies, FunctionalDependencies, AllowAmbiguousTypes,
             UndecidableInstances, QuantifiedConstraints #-}

{-|
Description : Kind of Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This module is kept separate from Eiger.Record to allow splitting up
Eiger.Record without hs-boot files.

-}

module Eiger.Record.Type (
  Record,

  -- * Type representations
  getRecordTyCon,

  -- * Fields
  HasRecordField, getRecordField, setRecordField,

  ) where

import Eiger.Util

import qualified Data.Generics.Product.Fields as GL

import Type.Reflection

-----------------------------------------------------
-- Eiger Records

-- | This kind classifies Eiger records.
type Record :: Type
type Record = Type

----------------------------------------------------
-- Accessing types

-- | Get a 'TyCon' representing a record type
getRecordTyCon ::
  forall (record :: Record).
  Typeable record =>
  record -> TyCon
getRecordTyCon _record = typeRepTyCon (typeRep @record)

----------------------------------------------------
-- Fields

-- | @HasRecordField field_name record field_type@ states that
-- @record@ has a field named @field_name@ of type @field_type@,
-- using Haskell's built-in record system.
type HasRecordField field_name record field_type =
  ( GL.HasField' field_name record field_type
  , Generic record )

-- | Access a record field.
getRecordField ::
  forall field_name record field_type.
  HasRecordField field_name record field_type =>
  record -> field_type
getRecordField = GL.getField @field_name

-- | Update a record field.
setRecordField ::
  forall field_name record field_type.
  HasRecordField field_name record field_type =>
  record -> field_type -> record
setRecordField r x = GL.setField @field_name x r