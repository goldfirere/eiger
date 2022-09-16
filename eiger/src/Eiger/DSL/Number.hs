{-# LANGUAGE UndecidableSuperClasses, DerivingVia,
             UndecidableInstances, GADTs, FunctionalDependencies #-}

{-|
Description : A numerical hierarchy suitable for use in the Eiger DSL
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This numerical hierarchy is meant as a replacement for the existing
'Num' class and supports more fine-grained notions of addability
and multiplicability. In particular, this new hierarchy is meant
to support currency amounts, where it makes sense to add two
currency amounts but never to multiply them. Another useful
mental model is temperature, where it makes sense to subtract two
temperatures in °C to get a difference, but never to add two
temperatures in °C.

-}

module Eiger.DSL.Number (
   -- * Conversions
  FromInt(..), ToInt(..), FromFloat(..), ToFloat(..),

   -- * Addition
  Addable(..), Subtractable(..),

   -- * Multiplication
  Multiplicable(..), Dividable(..), round,

   -- * Re-exports
   P.Int, P.Integer, P.Word, P.Float, P.Double, P.Rational,
  ) where

import Eiger.DSL.Equality

import qualified Prelude as P
import qualified Data.Int as P
import qualified Data.Word as P

------------------------------------------------
-- Conversions

-- TODO: Fix comments!
-- | @FromInt n@ denotes that there is a conversion from 'Integer'
-- to @n@.
class FromInt n where
  fromInt :: P.Int -> n

-- | @ToInt n@ denotes that there is a conversion from @n@ to
-- 'Integer'.
--
-- If @FromInt n@ also holds:
-- Law: @toInt (fromInt int) == int@.
class ToFloat n => ToInt n where
  toInt :: n -> P.Int

-- | @FromFloat n@ denotes that there is a conversion from 'Rational'
-- to @n@.
class FromInt n => FromFloat n where
  fromFloat :: P.Rational -> n

-- | @ToFloat n@ denotes that there is a conversion from @n@ to 'Rational'.
class ToFloat n where
  -- | Convert to a 'Rational' representation
  toFloat :: n -> P.Rational
------------------------------------------------
-- Addition

-- | TODO: Update comments.
-- @Additive base addend@ means that something of type @addend@
-- can be meaningfully added to something of type @base@, producing
-- another @base@. Addition must be associative.
--
-- Law 1: @b + (a - b) == b@
--
-- Law 2: @a + (b + c) == (a + b) + c@. Note that the @+@ operations
-- are at different types!
class Addable addend1 addend2 sum | addend1 addend2 -> sum where
  (+) :: addend1 -> addend2 -> sum
  infixl 6 +

class Subtractable sum addend difference | sum addend -> difference where
  (-) :: sum -> addend -> difference
  infixl 6 -

-----------------------------------------------------
-- Multiplication

-- | @Scalable scalar base@ means that a quantity of type @base@ can be
-- multiplied by a @scalar@.
class Multiplicable factor1 factor2 product | factor1 factor2 -> product where
  (*) :: factor1 -> factor2 -> product
  infixl 7 *

-- | @Dividable scalar base@ means that two quantities of type @base@
-- can be divided to form a ratio of type @scalar@.
class Dividable top bottom quotient | top bottom -> quotient where
  (/) :: top -> bottom -> quotient
  infixl 7 /

-------------------------------------------------------
-- Non-integers

-- | Like 'P.round'
round :: ToFloat n => FromInt i => n -> i
round n = fromInt (P.round (toFloat n))

---------------------------------------------------------
-- Instances

deriving via ViaNum P.Integer instance FromInt P.Integer
deriving via ViaNum P.Int instance FromInt P.Int
deriving via ViaNum P.Word instance FromInt P.Word
deriving via ViaNum P.Float instance FromInt P.Float
deriving via ViaNum P.Double instance FromInt P.Double
deriving via ViaNum P.Int8 instance FromInt P.Int8
deriving via ViaNum P.Int16 instance FromInt P.Int16
deriving via ViaNum P.Int32 instance FromInt P.Int32
deriving via ViaNum P.Int64 instance FromInt P.Int64
deriving via ViaNum P.Word8 instance FromInt P.Word8
deriving via ViaNum P.Word16 instance FromInt P.Word16
deriving via ViaNum P.Word32 instance FromInt P.Word32
deriving via ViaNum P.Word64 instance FromInt P.Word64
deriving via ViaNum P.Rational instance FromInt P.Rational

deriving via ViaNum P.Integer instance ToInt P.Integer
deriving via ViaNum P.Int instance ToInt P.Int
deriving via ViaNum P.Word instance ToInt P.Word
deriving via ViaNum P.Int8 instance ToInt P.Int8
deriving via ViaNum P.Int16 instance ToInt P.Int16
deriving via ViaNum P.Int32 instance ToInt P.Int32
deriving via ViaNum P.Int64 instance ToInt P.Int64
deriving via ViaNum P.Word8 instance ToInt P.Word8
deriving via ViaNum P.Word16 instance ToInt P.Word16
deriving via ViaNum P.Word32 instance ToInt P.Word32
deriving via ViaNum P.Word64 instance ToInt P.Word64

deriving via ViaNum P.Rational instance FromFloat P.Rational
deriving via ViaNum P.Float instance FromFloat P.Float
deriving via ViaNum P.Double instance FromFloat P.Double

deriving via ViaNum P.Integer instance ToFloat P.Integer
deriving via ViaNum P.Int instance ToFloat P.Int
deriving via ViaNum P.Word instance ToFloat P.Word
deriving via ViaNum P.Float instance ToFloat P.Float
deriving via ViaNum P.Double instance ToFloat P.Double
deriving via ViaNum P.Int8 instance ToFloat P.Int8
deriving via ViaNum P.Int16 instance ToFloat P.Int16
deriving via ViaNum P.Int32 instance ToFloat P.Int32
deriving via ViaNum P.Int64 instance ToFloat P.Int64
deriving via ViaNum P.Word8 instance ToFloat P.Word8
deriving via ViaNum P.Word16 instance ToFloat P.Word16
deriving via ViaNum P.Word32 instance ToFloat P.Word32
deriving via ViaNum P.Word64 instance ToFloat P.Word64
deriving via ViaNum P.Rational instance ToFloat P.Rational

instance Addable P.Integer P.Integer P.Integer where
  (+) = (P.+)
instance Subtractable P.Integer P.Integer P.Integer where
  (-) = (P.-)

instance Addable P.Int P.Int P.Int where
  (+) = (P.+)
instance Subtractable P.Int P.Int P.Int where
  (-) = (P.-)

instance Addable P.Word P.Word P.Word where
  (+) = (P.+)
instance Subtractable P.Word P.Word P.Word where
  (-) = (P.-)

instance Addable P.Float P.Float P.Float where
  (+) = (P.+)
instance Subtractable P.Float P.Float P.Float where
  (-) = (P.-)

instance Multiplicable P.Integer P.Integer P.Integer where
  (*) = (P.*)
instance Multiplicable P.Int P.Int P.Int where
  (*) = (P.*)

{-
deriving via ScaleInteger P.Int instance ToInt scalar => Scalable scalar P.Int
deriving via ScaleInteger P.Word instance ToInt scalar => Scalable scalar P.Word
deriving via ScaleRational P.Float instance ToFloat scalar => Scalable scalar P.Float
deriving via ScaleRational P.Double instance ToFloat scalar => Scalable scalar P.Double
deriving via ScaleInteger P.Int8 instance ToInt scalar => Scalable scalar P.Int8
deriving via ScaleInteger P.Int16 instance ToInt scalar => Scalable scalar P.Int16
deriving via ScaleInteger P.Int32 instance ToInt scalar => Scalable scalar P.Int32
deriving via ScaleInteger P.Int64 instance ToInt scalar => Scalable scalar P.Int64
deriving via ScaleInteger P.Word8 instance ToInt scalar => Scalable scalar P.Word8
deriving via ScaleInteger P.Word16 instance ToInt scalar => Scalable scalar P.Word16
deriving via ScaleInteger P.Word32 instance ToInt scalar => Scalable scalar P.Word32
deriving via ScaleInteger P.Word64 instance ToInt scalar => Scalable scalar P.Word64
deriving via ScaleRational P.Rational instance ToFloat scalar => Scalable scalar P.Rational
-}

instance Dividable P.Float P.Float P.Float where
  (/) = (P./)

{-
instance Dividable P.Double P.Double where (/) = (P./)
instance Dividable P.Rational P.Rational where (/) = (P./)
-}

-----------------------------------------------
-- Utilities

newtype ViaNum n = MkVN n

deriving newtype instance Eq n => Eq (ViaNum n)
deriving newtype instance Ord n => Ord (ViaNum n)

instance P.Num n => FromInt (ViaNum n) where
  fromInt n = MkVN (P.fromIntegral n)

instance P.Integral n => ToInt (ViaNum n) where
  toInt (MkVN n) = P.fromIntegral n

instance P.Fractional n => FromFloat (ViaNum n) where
  fromFloat f = MkVN (P.fromRational (P.toRational f))

instance P.Real n => ToFloat (ViaNum n) where
  toFloat (MkVN f) = P.fromRational (P.toRational f)
