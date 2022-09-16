{-|
Description : Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

Eiger is built around the concept of records. This module has supporting definitions
that allow us to define these. TODO: Write user-facing documentation.

-}

module Eiger.Record (
    module Eiger.Record.Core, module Eiger.Record.Type, module Eiger.Record.Class,
    -- TODO: Arrange for better haddock
  ) where

import Eiger.Record.Type
import Eiger.Record.Core
import Eiger.Record.Class