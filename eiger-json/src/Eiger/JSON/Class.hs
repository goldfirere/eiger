{-# LANGUAGE DataKinds, TypeFamilies, UndecidableSuperClasses, UndecidableInstances #-}

{-|
Description : Class required of records to be read and written via JSON
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

-}

module Eiger.JSON.Class (
  IsJSONRecord,
  ) where

import Eiger.Record
import Eiger.Key

import qualified Generics.SOP as SOP

import Data.Kind
import Type.Reflection

----------------------------------------------------
-- The IsJSONRecord class

-- | 'IsJSONRecord' collects together many constraints that need to be satisfied
-- to read and write Eiger records as JSON.
type IsJSONRecord :: Record -> Constraint
class ( SOP.HasDatatypeInfo record ,
        HasOneRecordConstructor record ,
        SOP.All (SOP.All Typeable) (SOP.Code record) ,
        HasKey record ,
        Typeable record ) =>
      IsJSONRecord record
instance ( SOP.HasDatatypeInfo record ,
        HasOneRecordConstructor record ,
        SOP.All (SOP.All Typeable) (SOP.Code record) ,
        HasKey record ,
        Typeable record ) =>
      IsJSONRecord record

----------------------------------------------
-- Generics.SOP constraints

-- | Assert that a datatype or newtype has exactly one record constructor, using
-- the "Generics.SOP" approach.
type HasOneRecordConstructor :: Type -> Constraint
type HasOneRecordConstructor ty = Length (SOP.Code ty) ~ Succ Zero

------------------------------------------------------
-- Unary natural numbers

data Nat = Zero | Succ Nat

type Length :: [a] -> Nat
type family Length xs where
  Length '[]      = Zero
  Length (_ : xs) = Succ (Length xs)
