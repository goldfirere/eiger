{-# LANGUAGE DataKinds, TypeFamilyDependencies,
             UndecidableInstances, AllowAmbiguousTypes,
             DefaultSignatures, OverloadedRecordDot, UndecidableSuperClasses, DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-orphans #-}   -- instance GetField ... (Key record) ...

{-|
Description : Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

Eiger is built around the concept of records. This module has supporting definitions
that allow us to define these. TODO: Write user-facing documentation.

-}

module Eiger.Record.Core {- TODO (
  -- * Type-level information
  Record, Mode(..), KnownFact, DerivableFact,

  -- * Defining record computation rules
  Computable(..), RulesFor, Self,
  ComputationRule,

  -- ** Combinators for computation rules
  match,

  -- * Storing data
  StoredKnown(..), extractStoredKnown,
  StoredDerivable(..),

  -- * Queries
  get, getMissingDependencies,

  -- * Exports to clean up error messages (you should not need these in your code)
  Generic,
  ) -} ( module Eiger.Record.Core, module Eiger.Record.Type, Key, Generic,
         module Eiger.Database, module Eiger.Monad ) where

import Eiger.Util
import Eiger.Generic
import Eiger.Monad
import Eiger.Record.Type
import Eiger.Key
import Eiger.Database

import qualified GHC.Generics as G

--------------------------------------------------
-- Facts

-- | A Fact is a fact about an entity. It might be an input fact (loaded
-- from some data source) or a derived fact (computed from other facts).
type Fact :: FactSort -> Type -> Type
data family Fact sort t

-- | Extract the type of the fact from a 'Fact' type.
type ExtractFactType :: Type -> Type
type family ExtractFactType fact where
  ExtractFactType (Fact _sort t) = t
  ExtractFactType (Key record) = Key record   -- special case for "key" field

-- | Get the fact type of an Eiger field.
type FactType :: Type -> Symbol -> Type
type FactType record field_name = ExtractFactType (FieldType record field_name)

-- | Is a fact known or derivable? Used only at compile time.
-- An 'Input' fact must be included when initially constructing the data;
-- otherwise, the data is invalid. On the other hand, an 'Optional'
-- fact might or might not be included when the data is loaded. 'Optional'
-- facts may be associated with a 'ComputationRule' that computes the
-- fact from others. Lastly, a 'Computed' fact cannot be specified
-- in the data source and must be computed via a 'ComputationRule'.
type FactSort :: Type
data FactSort = Input | Optional

-- | Get a 'FactSort' from the way a fact is stored
type GetFactSort :: Type -> FactSort
type family GetFactSort storage_type where
  GetFactSort (Fact sort _) = sort
  GetFactSort (Key _) = Input   -- special case for "key" field

-- | Get the 'FactSort' of a field in a record
type GetFieldFactSort :: Record -> Symbol -> FactSort
type GetFieldFactSort record field_name = GetFactSort (FieldType record field_name)

-- | @HasFact name record@ holds when the record @record@ has a fact
-- named @name@. Works for both known facts and derivable
-- facts.
type HasFact :: Symbol -> Record -> Constraint
type HasFact field_name record =
  HasFact_ field_name record (GetFieldFactSort record field_name)

-- | An internal form of 'HasFact' that can branch on a 'FactSort'.
type HasFact_ :: Symbol -> Record -> FactSort -> Constraint
class (GetFieldFactSort record field_name ~ fact_sort, Typeable record) =>
    HasFact_ field_name record fact_sort where
  -- | See 'get'. TODO: document better
  getFact :: record -> EigerM (FactType record field_name)

-- This is a special case for the "key" field. We could remove this if we made the
-- "key" field a Fact Input.
instance {-# OVERLAPPING #-}
         ( GetFieldFactSort record "key" ~ Input ,
           HasKey record ,
           Typeable record ,
           FactType record "key" ~ Key record
         ) =>
         HasFact_ "key" record Input where
  getFact record = return (getRecordField @"key" record)

instance ( GetFieldFactSort record field_name ~ Input ,
           HasRecordField field_name record (Fact Input (FactType record field_name)) ,
           Typeable record
         ) =>
    HasFact_ field_name record Input where
  getFact record = return (unInputFact $ getRecordField @field_name record)

instance ( GetFieldFactSort record field_name ~ Optional ,
           KnownSymbol field_name ,
           HasRecordField field_name record (Fact Optional (FactType record field_name)) ,
           Rule record field_name ,
           Typeable record ,
           HasKey record ,
           HasRules record
         ) =>
    HasFact_ field_name record Optional where
  getFact record = case getRecordField @field_name record of -- try to read the field
    AlreadyKnown result -> return result  -- found it!
    MustDerive -> case rule @record @field_name key of
      NoRule -> missingRule @field_name record
      HasRule r -> do
        result <- runComputationRule r
        updateDatabase key (\ inner_record -> setRecordField @field_name inner_record (AlreadyKnown result))
        return result
    where
      key = getRecordField @"key" record

--------------------------------------------------
-- Defining rules

-- | Defining an instance of 'Rule' allows clients to define a computation
-- rule used to compute the value of a record field.
type Rule :: Record -> Symbol -> Constraint
class Rule record field_name where
  rule :: HasRules record => Key record -> OptionalRule (FactType record field_name)

-- | A 'HasRules' constraint says that a record type has 'Rule' instances
-- for all of its 'Optional' fields. This is helpful when your 'Rule'
-- instances are scattered across files. In particular, if a 'Rule' for
-- a record @R1@ requires a rule for a record @R2@ (where that 'Rule' in
-- not in scope because it is in a downstream module), you can put a
-- @HasRules R2 =>@ constraint on the instance declaration for
-- @instance HasRules R2 => Rule R1 "some_field"@.
type HasRules :: Record -> Constraint
type HasRules record = HasRulesRep record (UnM1 (UnM1 (G.Rep record)))

-- Helper function for 'HasRules'
type HasRulesRep :: Type -> Representation -> Constraint
type family HasRulesRep record rep where
  HasRulesRep record ((l G.:*: r) G.:*: tail) = HasRulesRep record (l G.:*: r G.:*: tail)
  HasRulesRep record (G.M1 G.S (G.MetaSel (Just field_name) _packing _strictness _laziness)
                               (G.Rec0 (Fact Optional _))) = Rule record field_name
  HasRulesRep record (G.M1 G.S (G.MetaSel (Just field_name) _packing _strictness _laziness)
                               (G.Rec0 (Fact Optional _)) G.:*: tail) =
    (Rule record field_name, HasRulesRep record tail)
  HasRulesRep record (_ G.:*: tail) = HasRulesRep record tail

-----------------------------------------------------
-- Computations

-- | An @OptionalRule@ specifies either that there is no way to derive
-- this fact from others or how to derive it.
type OptionalRule :: Type -> Type
data OptionalRule t
  = NoRule
  | HasRule (ComputationRule t)

-- | How to specify that there is no rule for a given fact
noRule :: OptionalRule t
noRule = NoRule

-- | How to specify which rule to use for a given fact
useRule :: ComputationRule t -> OptionalRule t
useRule = HasRule

-- | A 'ComputationRule' is a delayed function that can compute
-- the value of a fact from its dependencies.
type ComputationRule :: Type -> Type
newtype ComputationRule t where
  -- | Compute the value from some dependencies
  MkRule ::
    EigerM t ->
      -- ^ How to compute the result from the known facts and the retrieved derived facts
    ComputationRule t
  deriving newtype (Functor, Applicative, Monad)

-- | Compute the result of a computation rule
runComputationRule ::
  forall field_type.
  ComputationRule field_type ->
  EigerM field_type
runComputationRule (MkRule fun) = fun

accessField ::
  forall field_name record.
  HasFact field_name record =>
  KnownSymbol field_name =>
  Key record ->
  EigerM (FactType record field_name)
accessField key = do
  recordFieldDependency @field_name key
  record <- getRecordFromKey key
  getFact @field_name record

accessAllKeysOfType ::
  forall record.
  Typeable record =>
  EigerM [Key record]
accessAllKeysOfType = do
  recordTypeDependency @record
  db <- getDatabase
  return (getAllKeysOfType db)

-------------------------------------------------------------
-- user-facing combinators for computation rules

-- | We cannot use @case@ directly on computations, so instead we use 'match'
-- and lambda-case, like this:
--
-- @
--   match self.value \case ->
--     Value1 -> ...
--     Value2 -> ...
-- @
match ::
  ComputationRule field_type ->
  (field_type -> ComputationRule result_type) ->
  ComputationRule result_type
match = (>>=)

-------------------------------------------------------
-- Field access with OverloadedRecordDot

-- | The constraint @GetField field_name record@ holds when you can
-- project a field named @field_name@ from a value of type @record@.
-- The 'FieldAccessResult' type family tells you what the type of the
-- field is.
type GetField :: Symbol -> Type -> Constraint
class GetField field_name record where
  type FieldAccessResult field_name record :: Type
  getField :: record -> FieldAccessResult field_name record

-- This allows chaining together computations with @.@
instance (HasFact field_name record, KnownSymbol field_name) =>
    GetField field_name (ComputationRule (Key record)) where
  type FieldAccessResult field_name (ComputationRule (Key record)) =
    ComputationRule (FactType record field_name)
  getField key_rule = getField @field_name =<< key_rule

-- Access the contents of a record from its key.
instance (HasFact field_name record, KnownSymbol field_name) =>
    GetField field_name (Key record) where
  type FieldAccessResult field_name (Key record) =
    ComputationRule (FactType record field_name)
  getField key = MkRule $ accessField @field_name key

------------------------------------------------------------
-- Storing data at runtime

newtype instance Fact Input t = InputFact { unInputFact :: t }

data instance Fact Optional t
  = MustDerive
  | AlreadyKnown t