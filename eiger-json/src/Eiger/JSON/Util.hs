{-# LANGUAGE DataKinds, GADTs, AllowAmbiguousTypes #-}

{-|
Description : Utilities
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

Utilities for eiger-json
-}

module Eiger.JSON.Util where

import Eiger.JSON.Class

import qualified Generics.SOP as SOP

import qualified Data.Text as T

import Data.Proxy

--------------------------------------
-- generics-sop

-- | Retrieve the 'SOP.ConstructorInfo' from a record type.
getConstructorInfo ::
  forall record r.
  IsJSONRecord record =>
  (forall xs. SOP.Code record ~ '[ xs ] => SOP.ConstructorInfo xs -> r) ->
  r
getConstructorInfo k = case SOP.datatypeInfo @record Proxy of
  SOP.ADT _modname _typename (info SOP.:* SOP.Nil) _strictness -> k info
  SOP.Newtype _modname _typename info -> k info

--------------------------------------
-- Text

tshow :: Show a => a -> T.Text
tshow = T.pack . show
