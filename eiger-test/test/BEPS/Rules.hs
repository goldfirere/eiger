{-# LANGUAGE DuplicateRecordFields, DataKinds, OverloadedRecordDot,
             RebindableSyntax, DeriveAnyClass, TemplateHaskell,
             UndecidableInstances, TypeFamilies #-}

module BEPS.Rules where


import Eiger.DSL
import Eiger.Record
import Eiger.Record.Operation
import Eiger.Record.TH
import Data.String

data Jurisdiction = MkJ
  { key :: Key Jurisdiction
  , entity :: Fact Optional [Key Entity]
  , year :: Fact Input (Key FiscalYear)
  , effective_tax_rate :: Fact Optional Float
  , net_globe_income :: Fact Optional Float
  }
  deriving IsRecord

data Group a = MkG { }

data FiscalYear = MkFY {
  key :: Key FiscalYear }
  deriving IsRecord

data EntityClass = InvestmentEntity
  deriving Eq

instance IsString EntityClass where
  fromString _ = InvestmentEntity

data Entity = MkE
  { key :: Key Entity
  , jurisdiction :: Fact Input (Key Jurisdiction)
  , year :: Fact Input (Key FiscalYear)
  , klass :: Fact Input EntityClass
  , globe_income_or_loss_of_the_constituent_entity
  , globe_income_of_the_constituent_entity
  , globe_loss_of_the_constituent_entity
  , financial_accounting_net_income_or_loss
  , net_taxes_expense
  , excluded_dividends
  , excluded_equity_gain_or_loss
  , included_revaluation_method_gain_or_loss
  , gain_or_loss_from_disposition_of_assets_and_liabilities
  , asymmetric_foreign_currency_gains_or_losses
  , policy_disallowed_expenses
  , prior_period_errors_and_changes_in_accounting_principles
  , accrued_pension_expense
  , adjusted_covered_taxes :: Fact Optional Float
  }
  deriving IsRecord

$deriveRecordInstances

instance Rule Jurisdiction "entity" where rule _ = noRule
instance Rule Jurisdiction "net_globe_income" where rule _ = noRule

instance HasRules Entity => Rule Jurisdiction "effective_tax_rate" where
  rule self = useRule $ adjusted_covered_taxes' / self.net_globe_income
    where
      adjusted_covered_taxes' =
                getAll @Entity
                   `filteredBy` filter
                   `select` (.adjusted_covered_taxes)
                   `andThen` sum

      filter entity = (entity.jurisdiction == self.key)

instance Rule Entity "policy_disallowed_expenses" where rule _ = noRule
instance Rule Entity "asymmetric_foreign_currency_gains_or_losses" where rule _ = noRule
instance Rule Entity "gain_or_loss_from_disposition_of_assets_and_liabilities" where rule _ = noRule
instance Rule Entity "included_revaluation_method_gain_or_loss" where rule _ = noRule
instance Rule Entity "excluded_equity_gain_or_loss" where rule _ = noRule
instance Rule Entity "financial_accounting_net_income_or_loss" where rule _ = noRule
instance Rule Entity "net_taxes_expense" where rule _ = noRule
instance Rule Entity "excluded_dividends" where rule _ = noRule

instance Rule Entity "globe_income_or_loss_of_the_constituent_entity" where
  rule self = useRule $
          self.financial_accounting_net_income_or_loss
          {- Article 3.2.1 -}
        - self.net_taxes_expense
        - self.excluded_dividends
        - self.excluded_equity_gain_or_loss
        - self.included_revaluation_method_gain_or_loss
        - self.gain_or_loss_from_disposition_of_assets_and_liabilities
        - self.asymmetric_foreign_currency_gains_or_losses
        - self.policy_disallowed_expenses
        - self.prior_period_errors_and_changes_in_accounting_principles
        - self.accrued_pension_expense

instance Rule Entity "globe_income_of_the_constituent_entity" where
  rule self = useRule $
        let value = self.globe_income_or_loss_of_the_constituent_entity in
        if value >= 0.0 then value else 0.0
