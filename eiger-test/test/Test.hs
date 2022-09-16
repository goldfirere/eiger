module Main where

import Test.Tasty

import qualified USIncomeTax.Test.HardCoded as HardCoded
import qualified USIncomeTax.Test.Loaded as Loaded
import qualified BEPS.Loaded as BEPS

main :: IO ()
main = defaultMain all_tests

all_tests :: TestTree
all_tests = testGroup "All"
  [ HardCoded.usIncomeTaxTests
  , Loaded.usIncomeTaxTests
  , BEPS.tests
  ]