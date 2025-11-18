module DataTypes where

data LoanType
  = Annuity
  | InterestOnly
  deriving (Show, Eq)

data LoanConfig = LoanConfig
  { principal  :: Double
  , annualRate :: Double
  , years      :: Int
  , loanType   :: LoanType
  } deriving (Show, Eq)

data Payment = Payment
  { period             :: Int
  , openingBalance     :: Double
  , interestComponent  :: Double
  , principalComponent :: Double
  , closingBalance     :: Double
  } deriving (Show, Eq)
