-- | Pure financial processing logic for the Loan Calculator.
--   Contains only pure functions, making it easy to test and reason about.
--   Also demonstrates how pure functions can be evaluated in parallel.
module Processing
  ( monthlyRate
  , totalPeriods
  , annuityPayment
  , amortizationSchedule
  , amortizationScheduleParallel
  , totalInterestPaid
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

-- | Monthly payment for an annuity loan using the standard formula:
--   P = pr(1+r)^n / ((1+r)^n - 1)
--   where:
--     p = principal
--     r = monthly rate
--     n = number of months
annuityPayment :: LoanConfig -> Double
annuityPayment cfg =
  let p = principal cfg
      r = monthlyRate cfg
      n = fromIntegral (totalPeriods cfg)
  in if r == 0
        then p / n
        else p * r * (1 + r) ** n / ((1 + r) ** n - 1)

-- | Top-level function: build full amortization schedule
--   based on the selected 'LoanType'.
amortizationSchedule :: LoanConfig -> [Payment]
amortizationSchedule cfg =
  case loanType cfg of
    Annuity      -> annuitySchedule cfg
    InterestOnly -> interestOnlySchedule cfg

-- | Pure, sequential annuity amortization schedule.
--   Implemented using recursion over the remaining balance.
annuitySchedule :: LoanConfig -> [Payment]
annuitySchedule cfg = go 1 (principal cfg)
  where
    r    :: Double
    r    = monthlyRate cfg

    n    :: Int
    n    = totalPeriods cfg

    pmt  :: Double
    pmt  = annuityPayment cfg

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

-- | Interest-only schedule:
--   Pay only interest for (n - 1) months,
--   then repay full principal in final month.
interestOnlySchedule :: LoanConfig -> [Payment]
interestOnlySchedule cfg =
  let r :: Double
      r = monthlyRate cfg

      n :: Int
      n = totalPeriods cfg

      p :: Double
      p = principal cfg

      mkPayment :: Int -> Payment
      mkPayment k =
        if k < n
          then Payment k p (p * r) 0 p
          else Payment k p (p * r) p 0
  in map mkPayment [1 .. n]

-- | Pure sum of all interest components in the schedule.
totalInterestPaid :: [Payment] -> Double
totalInterestPaid = sum . map interestComponent

-- | Parallel variant of 'amortizationSchedule'.
--   This uses 'parList rdeepseq' to evaluate the list of Payments
--   in parallel, demonstrating how pure functions can be safely
--   parallelized without shared mutable state.
amortizationScheduleParallel :: LoanConfig -> [Payment]
amortizationScheduleParallel cfg =
  amortizationSchedule cfg `using` parList rdeepseq

-- | Parallel variant of 'totalInterestPaid'.
--   Conceptually splits work across cores while keeping logic pure.
totalInterestPaidParallel :: [Payment] -> Double
totalInterestPaidParallel payments =
  let interests = map interestComponent payments `using` parList rdeepseq
  in sum interests
