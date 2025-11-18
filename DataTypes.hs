{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Domain data types for the Loan Calculator.
--   Contains algebraic data types (ADTs) that model real-world concepts.
module DataTypes where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Type of loan – extendable to more types in future (e.g., Balloon, VariableRate).
data LoanType
  = Annuity        -- ^ Same payment every month
  | InterestOnly   -- ^ Only interest each month, principal at the end
  deriving (Show, Eq, Generic, NFData)

-- | Configuration given by the user for a particular loan.
data LoanConfig = LoanConfig
  { principal  :: Double  -- ^ Loan amount (e.g. 1000000.0)
  , annualRate :: Double  -- ^ Annual interest rate in percent (e.g. 12.0)
  , years      :: Int     -- ^ Duration of the loan in years
  , loanType   :: LoanType-- ^ Type of loan (Annuity or InterestOnly)
  } deriving (Show, Eq, Generic, NFData)

-- | One monthly payment row in the amortization schedule.
data Payment = Payment
  { period             :: Int     -- ^ Month number (1..N)
  , openingBalance     :: Double  -- ^ Balance at start of month
  , interestComponent  :: Double  -- ^ Interest charged for this month
  , principalComponent :: Double  -- ^ Principal repaid this month
  , closingBalance     :: Double  -- ^ Balance at end of month
  } deriving (Show, Eq, Generic, NFData)
