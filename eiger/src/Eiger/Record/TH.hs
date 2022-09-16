{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Description : Template Haskell support for working with Eiger records
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This module provides Template Haskell convenience functions for working with Eiger
records.

-}

module Eiger.Record.TH (
  deriveRecordInstances,
  ) where

import Eiger.Record.Class

import Instance.Runtime.TH

import GHC.Generics
import Language.Haskell.TH
import Control.Monad

deriveRecordInstances :: Q [Dec]
deriveRecordInstances = do
  record_types <- allGroundInstanceTypes (ConT ''IsRecord)
  decss <- forM record_types $ \ r -> [d| deriving instance Generic $(pure r) |]
  return (concat decss)
