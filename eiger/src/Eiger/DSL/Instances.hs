{-# LANGUAGE IncoherentInstances, GADTs, UndecidableInstances #-}
   -- IncoherentInstances is a workaround for a 9.2 bug
{-# OPTIONS_GHC -Wno-orphans #-}  -- this is all about orphans

{-|
Description : Instances for Eiger types, such as ComputationRule
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

-}

module Eiger.DSL.Instances where

import qualified Prelude as P ()
import Eiger.DSL.Number
import Eiger.Record

import Control.Applicative

------------------------------------------------------------
-- Eiger numeric instances for ComputationRule

instance Addable a1 a2 sum => Addable (ComputationRule a1) (ComputationRule a2) (ComputationRule sum) where
  (+) = liftA2 (+)
instance Subtractable a1 a2 sum => Subtractable (ComputationRule a1) (ComputationRule a2) (ComputationRule sum) where
  (-) = liftA2 (-)

instance Multiplicable f1 f2 prod => Multiplicable (ComputationRule f1) (ComputationRule f2) (ComputationRule prod) where
  (*) = liftA2 (*)

instance Dividable top bottom quotient => Dividable (ComputationRule top) (ComputationRule bottom) (ComputationRule quotient) where
  (/) = liftA2 (/)