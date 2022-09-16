{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : Helper functions to use with GHC.Generics and Generics.SOP
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

-}

module Eiger.Generic (
  FieldType, UnM1,

  -- * Re-exports
  G.Generic(..),
  ) where

import Eiger.Util

import qualified GHC.Generics as G

------------------------------------------
-- GHC.Generics decomposition

-- | Remove an 'G.M1' wrapper
type UnM1 :: Representation -> Representation
type family UnM1 m1 where
  UnM1 (G.M1 sort meta_info payload) = payload

--------------------------------------------
-- Extracting a field type

-- | Get the type of a field from a representation of a record type
type RepFieldType :: Symbol -> Representation -> Type
type family RepFieldType field_name rep_data where
    -- this first equation would be redundant with the next if GHC used a more sensible representation of products, as a proper list
  RepFieldType field_name (G.M1 G.S (G.MetaSel (Just field_name)
                                       _packing _strictness _laziness)
                                    (G.Rec0 field_type)) = field_type
  RepFieldType field_name (G.M1 G.S (G.MetaSel (Just field_name)
                                       _packing _strictness _laziness)
                                    (G.Rec0 field_type) G.:*: _) = field_type

       -- reassociate (this is necessary sometimes!)
  RepFieldType field_name ((l G.:*: r) G.:*: tail) = RepFieldType field_name (l G.:*: r G.:*: tail)

  RepFieldType field_name (_ G.:*: tail_fields) = RepFieldType field_name tail_fields

-- | Get the type of a field from the type of a record
type FieldType :: Type -> Symbol -> Type
type FieldType record field_name =
  RepFieldType field_name (UnM1 (UnM1 (G.Rep record)))
