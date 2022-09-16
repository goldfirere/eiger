{-# OPTIONS_GHC -Wno-duplicate-exports #-}

{-|
Description : Prelude replacement for Eiger modules
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This module changes many definitions in the Prelude to work seamlessly
in rule computation expressions.

-}

module Eiger.DSL (
  -- * Types
  Bool(..), Int, Integer, Eq, Float, Double, P.Show(..), P.Read(..), P.read,

  -- * Unchanged from regular Haskell
  (P.$), P.undefined, P.pure, P.error,

  -- * Common operators
  (+), (-), (*), (/),
  (==), (/=), (<), (<=), (>), (>=),

  -- * Useful ComputationRule combinators
  use, useMapping, filteredBy, takeAll, and, or, select, andThen, count,
  singleElement,

  -- * Definitions used by RebindableSyntax
  ifThenElse, fromInteger, fromRational,

  -- * Numbers

  -- ** Conversions
  DSL.FromInt(..), DSL.ToInt(..), DSL.FromFloat(..), DSL.ToFloat(..),

  -- ** Addition
  DSL.Addable(..),

  -- ** Multiplication
  DSL.Multiplicable(..), DSL.Dividable(..),

  -- ** Other operations
  round, sum,
  not


  ) where

import Eiger.Record
import Eiger.Record.Operation

import Prelude ( Bool(..), Eq, Ord )
import qualified Prelude as P
import qualified Data.List as P ( foldl' )
import Control.Applicative
import Control.Monad ( filterM )
import Eiger.DSL.Number as DSL
import Eiger.DSL.Instances ()


fromInteger :: Integer -> ComputationRule Int
fromInteger n = P.pure (P.fromInteger n)

fromRational :: Rational -> ComputationRule Float
fromRational r = P.pure (P.fromRational r)

-- | TODO: document
sum :: (Addable a a a, FromInt a) => ComputationRule [a] -> ComputationRule a
sum = P.fmap (P.foldl' (+) (fromInt 0))

-- | When expecting a single-element list, returns the one element. If the
-- input is not a single-element list, fails with an error message reporting
-- the actual list's length.
singleElement :: ComputationRule [a] -> ComputationRule a
singleElement rul = do
  lst <- rul
  case lst of
    [x] -> P.return x
    _ -> failWith ("Expected a single element list, but got " P.++ P.show (P.length lst) P.++ " elements instead.")

not :: ComputationRule Bool -> ComputationRule Bool
not = P.fmap P.not

(==), (/=) :: Eq a => ComputationRule a -> ComputationRule a -> ComputationRule Bool
(==) = liftA2 (P.==)
(/=) = liftA2 (P./=)

infix 4 ==, /=

(<), (<=), (>), (>=) :: Ord a => ComputationRule a -> ComputationRule a -> ComputationRule Bool
(<) = liftA2 (P.<)
(<=) = liftA2 (P.<=)
(>) = liftA2 (P.>)
(>=) = liftA2 (P.>=)

infix 4 <, <=, >, >=

-- | This gives us the ability to use @if@ in ComputationRule expressions.
ifThenElse ::
  ComputationRule Bool ->
  ComputationRule a ->
  ComputationRule a ->
  ComputationRule a
ifThenElse switch true false = do
  b <- switch
  if b then true else false

-- | Combine two computations for booleans; short-circuiting
and, or :: ComputationRule Bool -> ComputationRule Bool -> ComputationRule Bool
and m_x m_y = do
  x <- m_x
  if x then m_y else P.pure False

or m_x m_y = do
  x <- m_x
  if x then P.pure True else m_y

infixr 3 `and`
infixr 2 `or`

-- | Use a pure value in a rule
use :: t -> ComputationRule t
use = P.pure

-- | Use a pure mapping in a rule
useMapping :: (a -> b) -> ComputationRule a -> ComputationRule b
useMapping = P.fmap

-- | Filter a list by a predicate
filteredBy :: ComputationRule [a] -> (a -> ComputationRule Bool) -> ComputationRule [a]
filteredBy m_xs p = do
  xs <- m_xs
  filterM p xs

takeAll :: a -> ComputationRule Bool
takeAll _ = pure True

-- | Select out elements from a list
select ::
  ComputationRule [Key record] ->
  (Key record -> ComputationRule field_type) ->
  ComputationRule [field_type]
select m_keys selector = do
  keys <- m_keys
  P.mapM selector keys

-- | Count the number of elements in a list
count :: ComputationRule [a] -> ComputationRule Int
count = P.fmap P.length

-- | Apply a function to an argument that precedes it. Useful for chaining
-- operations: @getAll \@IncomeRecord `filteredBy` filter `select` (.field) `andThen` sum@.
andThen :: a -> (a -> b) -> b
andThen x f = f x