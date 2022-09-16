-- this tests the ability for rules to be defined across multiple files

{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BEPS.Rules2 where

import BEPS.Rules
import Eiger.Record

instance Rule Entity "adjusted_covered_taxes" where rule _ = noRule
instance Rule Entity "accrued_pension_expense" where rule _ = noRule
instance Rule Entity "prior_period_errors_and_changes_in_accounting_principles" where rule _ = noRule
instance Rule Entity "globe_loss_of_the_constituent_entity" where rule _ = noRule