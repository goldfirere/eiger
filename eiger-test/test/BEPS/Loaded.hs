{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes       #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-uni-patterns -Wno-orphans #-}

module BEPS.Loaded where

import Paths

import           Eiger.JSON
import           Eiger.Key
import           Eiger.Record
import           Eiger.Record.Operation
import           BEPS.Rules
import BEPS.Rules2 ()

import qualified Data.Aeson             as JSON
import           Data.Text              (Text)

import           Control.Exception
import Control.DeepSeq

import System.FilePath
import Type.Reflection

import           Test.Tasty
import           Test.Tasty.HUnit

-----------------------------------------------------------
-- Establish extra instances to use with JSON

$deriveJSONRecordInstances

-----------------------------------------------------------
-- example with specific data

loadData :: IO Database
loadData = do
  let instances = $mkInstanceDatabase
  result <- JSON.eitherDecodeFileStrict (inputDirectory </> "beps.json")
  case result of
    Left err -> throwIO (JSONException err)
    Right json_value -> do
      case databaseFromJSON instances json_value emptyDatabase of
        Left err -> throwIO (ConvertException err)
        Right db -> return db

newtype JSONException = JSONException String
  deriving stock Show
  deriving anyclass Exception
newtype ConvertException = ConvertException Text
  deriving stock Show
  deriving anyclass Exception

------------------------------------------
-- tests

pizzaHutKey :: Key Entity
pizzaHutKey = mkTextKey "Pizza Hut"

chKey :: Key Jurisdiction
chKey = mkTextKey "switzerland"

tests :: TestTree
tests = withResource loadData (\_ -> return ()) $ \ loaded ->
  testGroup "BEPS (loaded)"
  [ testCase "globe income" $
          do db <- loaded
             evaluate $ rnf $
               failEigerM db (get @"globe_income_or_loss_of_the_constituent_entity" pizzaHutKey)
  , testCase "globe income dependency" $
          do db <- loaded
             succeedEigerM db (
               getMissingDependencies @"globe_income_or_loss_of_the_constituent_entity" pizzaHutKey) @?=
                (["\"Pizza Hut\".financial_accounting_net_income_or_loss"
                 ,"\"Pizza Hut\".net_taxes_expense"
                 ,"\"Pizza Hut\".excluded_dividends"
                 ,"\"Pizza Hut\".excluded_equity_gain_or_loss"
                 ,"\"Pizza Hut\".included_revaluation_method_gain_or_loss"
                 ,"\"Pizza Hut\".gain_or_loss_from_disposition_of_assets_and_liabilities"
                 ,"\"Pizza Hut\".asymmetric_foreign_currency_gains_or_losses"
                 ,"\"Pizza Hut\".policy_disallowed_expenses"
                 ,"\"Pizza Hut\".prior_period_errors_and_changes_in_accounting_principles"
                 ,"\"Pizza Hut\".accrued_pension_expense"
                 ],
                 [])
      , testCase "effective_tax_rate dependencies" $
          do db <- loaded
             succeedEigerM db (getMissingDependencies @"effective_tax_rate" @Jurisdiction chKey) @?=
               (["\"Pizza Hut\".adjusted_covered_taxes","\"switzerland\".net_globe_income"],
                [typeRepTyCon (typeRep @Entity)])
      ]
