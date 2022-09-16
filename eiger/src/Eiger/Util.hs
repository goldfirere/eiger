{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes,
             DataKinds, TypeFamilies #-}

{-|
Description : Basic utilities
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

-}


module Eiger.Util (
  -- * Type-level utilities

  Type, Constraint,
  Typeable,
  Generic(..), Representation,
  NFData,

  -- ** Equality
  (:~:)(..), (:~~:)(..),

  -- ** Symbols
  Symbol, SSymbol, symbol, KnownSymbol, symbolString,

  -- ** Infinite lists
  InfList(..), iterateInfList,

  -- ** Tuples
  Fst, Snd,

  -- ** Text
  Text, tshow, tpack, tunpack,

  -- ** Laziness
  KnotTied(..), getKnotTied,

  -- ** Lists
  partitionWith,

  -- ** Type-level programming
  List,
  )
  where

import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal )
import Data.Kind
import Data.Proxy
import Data.Type.Equality ( (:~:)(..), (:~~:)(..) )
import Type.Reflection ( Typeable )
import GHC.Generics
import Data.Text ( Text, pack, unpack )
import Control.DeepSeq

--------------------------------------------------
-- Generics

-- | Representations in GHC.Generics have kind @Type -> Type@.
type Representation = Type -> Type

--------------------------------------------------
-- Symbols

-- | A singleton for 'Symbol'. Could use the singletons library, but
-- no need to take that dependency.
type SSymbol :: Symbol -> Type
newtype SSymbol sym = UnsafeMkSSymbol String

-- | Get the singleton for a 'KnownSymbol'
symbol :: forall sym. KnownSymbol sym => SSymbol sym
symbol = UnsafeMkSSymbol (symbolVal @sym Proxy)

symbolString :: forall sym. KnownSymbol sym => String
symbolString = symbolVal @sym Proxy

---------------------------------------------------
-- Tuples

type Fst :: (a, b) -> a
type family Fst xy where
  Fst '(x, y) = x

type Snd :: (a, b) -> b
type family Snd xy where
  Snd '(x, y) = y

---------------------------------------------------
-- Infinite lists

-- | A list type that contains only infinite lists
type InfList :: Type -> Type
data InfList a = a ::: InfList a
infixr 5 :::

-- | Generate an infinite list by successively applying
-- a function to an initial value. @iterateInfList f x@ produces
-- the list @x ::: f x ::: f (f x) ::: f (f (f x)) ::: ...@.
iterateInfList :: (a -> a) -> a -> InfList a
iterateInfList f x = x ::: iterateInfList f (f x)

instance Functor InfList where
  fmap f (x ::: xs) = f x ::: fmap f xs

----------------------------------------------------
-- Text

tshow :: Show a => a -> Text
tshow = pack . show

tpack :: String -> Text
tpack = pack

tunpack :: Text -> String
tunpack = unpack

-----------------------------------------------------
-- Laziness

-- | A type marker to remind consumers of a value that it is
-- knot-tied and must be used only in lazy contexts.
newtype KnotTied a = MkKnotTied a

getKnotTied :: KnotTied a -> a
getKnotTied (MkKnotTied x) = x

------------------------------------------------------
-- Lists

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = go [] []
  where
    go l_acc r_acc [] = (reverse l_acc, reverse r_acc)
    go l_acc r_acc (x:xs) = case f x of
      Left l -> go (l:l_acc) r_acc xs
      Right r -> go l_acc (r:r_acc) xs

------------------------------------------------------
-- Type-level lists

-- | A synonym for @[]@ to avoid punning
type List :: Type -> Type
type List = []
