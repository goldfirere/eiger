{-# LANGUAGE DataKinds #-}

{-|
Description : Defines the IsRecord class to label Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This exports 'IsRecord'. Every Eiger record must have an instance of 'IsRecord'.

-}

module Eiger.Record.Class (
  IsRecord,
  ) where

import Eiger.Record.Type
import Eiger.Util

-- | The 'IsRecord' class marks Eiger records. Include @deriving IsRecord@ after
-- all your Eiger record declarations.
type IsRecord :: Record -> Constraint
class IsRecord rec