{-# LANGUAGE DataKinds, DisambiguateRecordFields, OverloadedRecordDot, TemplateHaskell,
             OverloadedStrings, GADTs #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-uni-patterns -Wno-type-defaults #-}

module USIncomeTax.Test.HardCoded where

import USIncomeTax.Defs
import USIncomeTax.Rules
import Eiger.Key
import Eiger.Record
import Eiger.Record.Operation

import Test.Tasty
import Test.Tasty.HUnit

import Type.Reflection

k1:::k2:::k3:::k4:::k5:::k6:::k7:::_ = getKeys keyGen

-----------------------------------------------------------
-- example with specific data

unknownTotal :: IncomeRecord
unknownTotal = MkIR { key = k1
                    , earned = AlreadyKnown (dollars 10)
                    , unearned = AlreadyKnown (dollars 15)
                    , total = MustDerive }

knownTotal :: IncomeRecord
knownTotal = MkIR { key = k2
                  , earned = MustDerive
                  , unearned = MustDerive
                  , total = AlreadyKnown (dollars 20) }

singlePerson :: PersonRecord
singlePerson = MkPR { key = k3
                    , filing_status = InputFact Single
                    , income = InputFact unknownTotal.key
                    , filed_income = MustDerive }

primarySpouse :: PersonRecord
primarySpouse = MkPR { key = k4
                     , filing_status = InputFact (MarriedFilingJointly k4 k5)
                     , income = InputFact knownTotal.key
                     , filed_income = MustDerive }

secondarySpouse :: PersonRecord
secondarySpouse = MkPR { key = k5
                       , filing_status = InputFact (MarriedFilingJointly k4 k5)
                       , income = InputFact unknownTotal.key
                       , filed_income = MustDerive }

unknowableTotal :: IncomeRecord
unknowableTotal = MkIR { key = k6
                       , earned = AlreadyKnown (dollars 10)
                       , unearned = MustDerive
                       , total = MustDerive }

blankIncome :: IncomeRecord
blankIncome = MkIR { key = k7
                   , earned = MustDerive
                   , unearned = MustDerive
                   , total = MustDerive }


db :: Database
db = emptyDatabase `addDbWithKey` unknownTotal `addDbWithKey` knownTotal `addDbWithKey` singlePerson `addDbWithKey` primarySpouse `addDbWithKey` secondarySpouse `addDbWithKey` unknowableTotal `addDbWithKey` blankIncome

dataModel :: DataModel
dataModel = $mkDataModel

------------------------------------------
-- tests

usIncomeTaxTests :: TestTree
usIncomeTaxTests = testGroup "US income tax (hard coded)"
  [ testGroup "computation success"
      [ testCase "Fact unknown total" $
          succeedEigerM db (get @"total" unknownTotal.key) @?= dollars 25
      , testCase "retrieve known total" $
          succeedEigerM db (get @"total" knownTotal.key) @?= dollars 20
      , testCase "single person" $
          succeedEigerM db (get @"filed_income" singlePerson.key) @?= dollars 25
      , testCase "primary spouse" $
          succeedEigerM db (get @"filed_income" primarySpouse.key) @?= dollars 45
      , testCase "secondary spouse" $
          succeedEigerM db (get @"filed_income" secondarySpouse.key) @?= dollars 0
      , testCase "earned from unknowable" $
          succeedEigerM db (get @"earned" unknowableTotal.key) @?= dollars 10
      ]
  , testGroup "computation failure"
      [ testCase "unknowable total" $
          map pprError (failEigerM db (get @"total" unknowableTotal.key)) @?=
          ["No value or rule for field unearned in record IncomeRecord@0006 of type IncomeRecord."]
      , testCase "blank total" $
          map pprError (failEigerM db (get @"total" blankIncome.key)) @?=
          ["No value or rule for field earned in record IncomeRecord@0007 of type IncomeRecord.",
           "No value or rule for field unearned in record IncomeRecord@0007 of type IncomeRecord."]
      ]
  , testGroup "dependencies"
      [ testCase "0-step no dependencies" $
          succeedEigerM db (getMissingDependencies @"total" knownTotal.key) @?= ([], [])
      , testCase "1-step no dependencies" $
          succeedEigerM db (getMissingDependencies @"total" unknownTotal.key) @?= ([], [])
      , testCase "unknown unearned income" $
          succeedEigerM db (getMissingDependencies @"total" unknowableTotal.key) @?= ([show unknowableTotal.key ++ ".unearned"], [])
      , testCase "blank total deps" $
          succeedEigerM db (getMissingDependencies @"total" blankIncome.key) @?= (["IncomeRecord@0007.earned", "IncomeRecord@0007.unearned"], [])
      ]
  , testGroup "checkFieldName"
      [ testCase "total income" $
        succeedEigerM db (checkFieldName @IncomeRecord dataModel "total" $
                       \ (_ :: Proxy total) field_type_rep -> do
                       case field_type_rep `eqTypeRep` typeRep @DollarAmount of
                         Just HRefl -> get @total knownTotal.key
                         Nothing -> failWithError (error "type mismatch")) @?= dollars 20
      , testCase "unknowable total dependencies dynamically" $
        succeedEigerM db (checkFieldName @IncomeRecord dataModel "total" $
                       \ (_ :: Proxy total) _rep -> getMissingDependencies @total unknowableTotal.key)
                       @?= ([show unknowableTotal.key ++ ".unearned"], [])
      ]
  ]
