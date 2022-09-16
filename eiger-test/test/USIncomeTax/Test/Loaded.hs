{-# LANGUAGE DataKinds, DisambiguateRecordFields, OverloadedRecordDot, TemplateHaskell,
             DerivingStrategies, DeriveAnyClass, ImpredicativeTypes, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-uni-patterns -Wno-type-defaults -Wno-orphans #-}

module USIncomeTax.Test.Loaded where

import Paths

import USIncomeTax.Defs
import USIncomeTax.Rules
import Eiger.Key
import Eiger.Record
import Eiger.JSON
import Eiger.Record.Operation

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import Data.Text ( Text )
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as T

import Control.Exception
import Control.Monad

import System.FilePath
import Data.Scientific

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

instance JSON.ToJSON DollarAmount where
  toJSON (MkDA cents) = JSON.Number $ scientific (toInteger cents) (-2)

instance JSON.ToJSON FilingStatus where
  toJSON Single = JSON.String "Single"
  toJSON MarriedFilingSeparately = JSON.String "MarriedFilingSeparately"
  toJSON (MarriedFilingJointly primary secondary) =
    JSON.object [ "data" JSON..= "MarriedFilingJointly"
                , "primary_filer" JSON..= primary
                , "secondary_filer" JSON..= secondary ]

$deriveJSONRecordInstances

-----------------------------------------------------------
-- example with specific data

instances :: InstanceDatabase
instances = $mkInstanceDatabase

dataModel :: DataModel
dataModel = $mkDataModel

loadData :: IO Database
loadData = do
  result <- JSON.eitherDecodeFileStrict (inputDirectory </> "us_income_tax.json")
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
newtype EigerException = EigerException String
  deriving stock Show
  deriving anyclass Exception

------------------------------------------
-- tests

unknownKey, knownKey, unknowableKey, blankKey :: Key IncomeRecord
unknownKey = mkTextKey "unknown"
knownKey = mkTextKey "known"
unknowableKey = mkTextKey "unknowable"
blankKey = mkTextKey "blank"

singleKey :: Key PersonRecord
singleKey = mkTextKey "single"

usIncomeTaxTests :: TestTree
usIncomeTaxTests = withResource loadData (\_ -> return ()) $ \ loaded ->
  testGroup "US income tax (loaded)"
  [ testGroup "computation success"
      [ testCase "Fact unknown total" $
          do db <- loaded; succeedEigerM db (get @"total" @IncomeRecord unknownKey) @?= dollars 25
      , testCase "retrieve known total" $
          do db <- loaded; succeedEigerM db (get @"total" @IncomeRecord knownKey) @?= dollars 20
      , testCase "single person" $
          do db <- loaded; succeedEigerM db (get @"filed_income" @PersonRecord singleKey) @?= dollars 25
--      , testCase "primary spouse" $
--          do db <- loaded; succeedEigerM db (get @"filed_income" primarySpouse) @?= dollars 45
--      , testCase "secondary spouse" $
--          do db <- loaded; succeedEigerM db (get @"filed_income" secondarySpouse) @?= dollars 0
      , testCase "earned from unknowable" $
          do db <- loaded; succeedEigerM db (get @"earned" @IncomeRecord unknowableKey) @?= dollars 10
      ]
  , testGroup "computation failure"
      [ testCase "unknowable total" $
          do db <- loaded; map pprError (failEigerM db (get @"total" @IncomeRecord unknowableKey)) @?=
                ["No value or rule for field unearned in record \"unknowable\" of type IncomeRecord."]
      , testCase "blank total" $
          do db <- loaded; map pprError (failEigerM db (get @"total" @IncomeRecord blankKey)) @?=
                ["No value or rule for field earned in record \"blank\" of type IncomeRecord.",
                 "No value or rule for field unearned in record \"blank\" of type IncomeRecord."]
      ]

  , testGroup "dependencies"
      [ testCase "0-step no dependencies" $
          do db <- loaded; succeedEigerM db (getMissingDependencies @"total" knownKey) @?= ([], [])
      , testCase "1-step no dependencies" $
          do db <- loaded; succeedEigerM db (getMissingDependencies @"total" unknownKey) @?= ([], [])
      , testCase "unknown unearned income" $
          do db <- loaded; succeedEigerM db (getMissingDependencies @"total" unknowableKey) @?= (["\"unknowable\".unearned"], [])
      , testCase "blank total deps" $
          do db <- loaded; succeedEigerM db (getMissingDependencies @"total" blankKey) @?= (["\"blank\".earned", "\"blank\".unearned"], [])
      ]

   , testGroup "JSON writing"
      [ let output_file = outputDirectory </> "original.json" in
        goldenTest
          "write original JSON"
          output_file $ do
            db <- loaded
            let (output, errors) = databaseToJSON instances db
            when (not (null errors)) $
              throwIO (JSONException (unlines (map (T.unpack . renderJSONError) errors)))
            LBS.writeFile output_file (JSON.encodePretty output)

      , let output_file = outputDirectory </> "updated.json" in
        goldenTest
          "write updated JSON"
          output_file $ do
            db <- loaded
            let result = runEigerM db (compute dataModel)
            case result of
              Left errors -> throwIO (EigerException (unlines (map pprError errors)))
              Right (updated_db, ()) ->
                let (json_db, json_errors) = databaseToJSON instances updated_db in
                if null json_errors
                then LBS.writeFile output_file (JSON.encodePretty json_db)
                else throwIO (JSONException (unlines (map (T.unpack . renderJSONError) json_errors)))
      ]

  ]

goldenTest ::
  String   ->    -- test name
  FilePath ->    -- output file
  IO ()    ->    -- action
  TestTree
goldenTest name output_file action =
  localOption (SizeCutoff 1_000_000) $  -- increase diff size
  localOption OnPass $                  -- delete output file on success
  goldenVsFileDiff name diffFunction (output_file <.> "golden") output_file action

-- see https://hackage.haskell.org/package/tasty-golden-2.3.5/docs/Test-Tasty-Golden.html#v:goldenVsStringDiff
diffFunction :: FilePath -> FilePath -> [String]
diffFunction ref new = ["diff", "-u", "-w", "-B", ref, new]
  -- This shows several lines of context, ignoring whitespace and blank lines