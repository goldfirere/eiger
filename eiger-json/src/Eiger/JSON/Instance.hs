{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Description : Support for runtime instance lookup for reading and writing JSON
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This module powers the runtime instance lookup necessary to read and write
JSON versions of Eiger databases.

-}

module Eiger.JSON.Instance (
  deriveJSONRecordInstances,

  InstanceDatabase(..), mkInstanceDatabase,

  -- * Type names
  TypeNamer, mkTypeNamer, unTypeNamer
  ) where

import Eiger.JSON.Class
import qualified Eiger.DSL.Number as DSL
import Eiger.Record

import Data.String

import Instance.Runtime
import Instance.Runtime.TH

import qualified Data.Text as T

import qualified Data.Aeson as JSON

import qualified Type.Reflection.Name as TRN

import qualified Generics.SOP as SOP

import Language.Haskell.TH
import Control.Monad

-------------------------------------------------
-- Deriving extra instances for use with JSON

-- | Once you have defined all your Eiger records and have an 'IsRecord'
-- instance for each one, say @\$deriveJSONRecordInstances@ on a line
-- by itself to allow these records to be serialized to/from JSON.
deriveJSONRecordInstances :: Q [Dec]
deriveJSONRecordInstances = do
  record_types <- allGroundInstanceTypes (ConT ''IsRecord)
  decss <- forM record_types $ \r -> [d|
    deriving instance SOP.Generic $(pure r)
    deriving instance SOP.HasDatatypeInfo $(pure r)
    |]
  return (concat decss)

-------------------------------------------------
-- InstanceDatabase

-- | A collection of instances for use
-- during JSON conversion. Make with @$mkInstanceDatabase@.
data InstanceDatabase =
  MkInstanceDatabase { is_record  :: Instances TypeNamer IsJSONRecord
                     , from_int   :: Instances TypeNamer DSL.FromInt
                     , real_float :: Instances TypeNamer RealFloat
                     , to_json    :: Instances TypeNamer JSON.ToJSON
                     , is_string  :: Instances TypeNamer IsString }

-- | Create an 'InstanceDatabase' with all known ground instances. A
-- /ground instance/ is one with no type variables. Use like
-- @$mkInstanceDatabase@, invoking Template Haskell.
-- Note that any instances declared in the current module will
-- be skipped.
mkInstanceDatabase :: Q Exp
mkInstanceDatabase =
  [| MkInstanceDatabase
       { is_record = $records
       , from_int = $from_ints
       , real_float = $floats
       , to_json = $to_jsons
       , is_string = $strings
       } |]
  where
    records, from_ints, floats, strings :: Q Exp
    records = [| instancesForInvisible
                   @( $(foldr (\h t -> PromotedConsT `AppT` h `AppT` t) PromotedNilT <$>
                        allGroundInstanceTypes (ConT ''IsRecord))) |]
    from_ints = allGroundInstances [t| DSL.FromInt |]
    floats = allGroundInstances [t| RealFloat |]
    to_jsons = allGroundInstances [t| JSON.ToJSON |]
    strings = allGroundInstances [t| IsString |]

--------------------------------------
-- Type names

-- | Change this definition to choose a different scheme to render type names.
type TypeNamer = TRN.Unqualified

mkTypeNamer :: T.Text -> TypeNamer
mkTypeNamer = TRN.MkUnqualified

unTypeNamer :: TypeNamer -> T.Text
unTypeNamer = TRN.getUnqualified
