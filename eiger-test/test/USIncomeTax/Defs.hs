{-# LANGUAGE TypeApplications, DerivingVia, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module USIncomeTax.Defs where

import qualified Prelude as P

import Data.Coerce
import Text.Printf

import Eiger.DSL.Number

newtype DollarAmount = MkDA Int   -- stored as # of cents
  deriving (P.Eq, P.Ord)

instance Addable DollarAmount DollarAmount DollarAmount where
  (+) = coerce ((+) @Int @Int)

instance Subtractable DollarAmount DollarAmount DollarAmount where
  (-) = coerce ((-) @Int @Int)

instance FromInt DollarAmount where
  fromInt = dollars

dollars :: Int -> DollarAmount
dollars x = MkDA (round ((100 :: Int) * x))

instance P.Show DollarAmount where
  show (MkDA num_cents) = '$' : P.show (num_cents `P.div` 100) P.++ "." P.++ printf "%02d" (num_cents `P.mod` 100)

newtype SocialSecurityNumber = MkSSN Int   -- really shouldn't be an Int, but a 9-element vector of digits
