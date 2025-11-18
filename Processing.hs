-- | Pure financial processing logic for the Loan Calculator.
--   Contains only pure functions, making it easy to test and reason about.
--   Also includes parallel variants using 'parallel' library.
module Processing
  ( monthlyRate
  , totalPeriods
  , annuityPayment
  , amortizationSchedule
  , totalInterestPaid
  , amortizationScheduleParallel
  , totalInterestPaidParallel
  ) where

import DataTypes
import Control.Parallel.Strategies (parList, rdeepseq, using)

-- | Convert annual percentage rate to monthly fraction.
--   E.g. 12% -> 0.01 (12 / 100 / 12).
monthlyRate :: LoanConfig -> Double
monthlyRate cfg = annualRate cfg / 100.0 / 12.0

-- | Total number of monthly periods in the loan.
totalPeriods :: LoanConfig -> Int
totalPeriods cfg = years cfg * 12

-- | Monthly payment for an annuity loan using:
--   P = p * r * (1+r)^n / ((1+r)^n - 1)
annuityPayment :: LoanConfig -> Double
annuityPayment cfg =
  let p = principal cfg
      r = monthlyRate cfg
      n = fromIntegral (totalPeriods cfg)
  in if r == 0
        then p / n
        else p * r * (1 + r) ** n / ((1 + r) ** n - 1)

-- | Top-level function: build full amortization schedule (sequential).
amortizationSchedule :: LoanConfig -> [Payment]
amortizationSchedule cfg =
  case loanType cfg of
    Annuity      -> annuitySchedule cfg
    InterestOnly -> interestOnlySchedule cfg

-- | Pure recursive annuity schedule (sequential).
annuitySchedule :: LoanConfig -> [Payment]
annuitySchedule cfg = go 1 (principal cfg)
  where
    r   :: Double
    r   = monthlyRate cfg

    n   :: Int
    n   = totalPeriods cfg

    pmt :: Double
    pmt = annuityPayment cfg

    go :: Int -> Double -> [Payment]
    go k bal
      | k > n || bal <= 0.0001 = []
      | otherwise =
          let interest     = bal * r
              principalPay = min (pmt - interest) bal
              closing      = bal - principalPay
              payment      = Payment
                               { period             = k
                               , openingBalance     = bal
                               , interestComponent  = interest
                               , principalComponent = principalPay
                               , closingBalance     = closing
                               }
          in payment : go (k + 1) closing

-- | Interest-only schedule (sequential).
interestOnlySchedule :: LoanConfig -> [Payment]
interestOnlySchedule cfg =
  let r = monthlyRate cfg
      n = totalPeriods cfg
      p = principal cfg

      mkPayment :: Int -> Payment
      mkPayment k =
        if k < n
          then Payment k p (p * r) 0 p
          else Payment k p (p * r) p 0
  in map mkPayment [1 .. n]

-- | Sum of all interest paid (sequential).
totalInterestPaid :: [Payment] -> Double
totalInterestPaid = sum . map interestComponent

-- | Parallel version of amortization schedule.
--   Still pure – we just tell Haskell to evaluate the list in parallel.
amortizationScheduleParallel :: LoanConfig -> [Payment]
amortizationScheduleParallel cfg =
  amortizationSchedule cfg `using` parList rdeepseq

-- | Parallel version of total interest computation.
totalInterestPaidParallel :: [Payment] -> Double
totalInterestPaidParallel payments =
  let interests :: [Double]
      interests = map interestComponent payments `using` parList rdeepseq
  in sum interests
