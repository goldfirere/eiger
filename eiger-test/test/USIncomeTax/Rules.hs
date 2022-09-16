{-# LANGUAGE RebindableSyntax, DataKinds, TypeFamilies,
             UndecidableInstances, OverloadedRecordDot,
             LambdaCase, BlockArguments, DuplicateRecordFields,
             NoFieldSelectors, PartialTypeSignatures,
             DeriveAnyClass, TemplateHaskell #-}

module USIncomeTax.Rules where

import Eiger.DSL
import Eiger.Record
import Eiger.Record.TH

import Data.String

import USIncomeTax.Defs

---------------------------------------------------------
-- specific client module, auditable by a domain expert

data IncomeRecord = MkIR { key :: Key IncomeRecord
                         , earned :: Fact Optional DollarAmount
                         , unearned :: Fact Optional DollarAmount
                         , total :: Fact Optional DollarAmount
                         }
                         deriving IsRecord

data FilingStatus = Single
                  | MarriedFilingJointly { primary_filer   :: Key PersonRecord
                                         , secondary_filer :: Key PersonRecord }
                  | MarriedFilingSeparately

data PersonRecord = MkPR { key           :: Key PersonRecord
                         , filing_status :: Fact Input FilingStatus
                         , income        :: Fact Input (Key IncomeRecord)
                         , filed_income  :: Fact Optional DollarAmount
                         }
                         deriving IsRecord

$deriveRecordInstances

instance Rule IncomeRecord "earned" where rule _ = noRule
instance Rule IncomeRecord "unearned" where rule _ = noRule

instance Rule IncomeRecord "total" where
  rule self = useRule $ self.earned + self.unearned



instance IsString FilingStatus where
  fromString "Single" = Single
  fromString "MarriedFilingSeparately" = MarriedFilingSeparately
  fromString _ = error "exotic FilingStatus"


instance Rule PersonRecord "filed_income" where
  rule self = useRule $ match self.filing_status \case
                        Single -> self.income.total
                        MarriedFilingSeparately -> self.income.total
                        MarriedFilingJointly primary spouse ->
                          if self.key == use primary
                          then self.income.total + (use spouse).income.total
                          else useMapping dollars 0
              {-
sum (select @Entity (\ e -> e.jurisdiction == self.jurisdiction && e.fiscalYear == self.fiscalYear)).income

sum (findAll @Entity `suchThat` filter).income
 where
  filter entity = entity.jursticiton == self.jursitciton
            `and` enitty.fiscalYear == self.fiscalYear

findAll @Entity `suchThat` filter `take` (.income) `andThen` sum
-}