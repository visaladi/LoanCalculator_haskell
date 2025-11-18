module Processing
  ( monthlyRate
  , totalPeriods
  , annuityPayment
  , amortizationSchedule
  , totalInterestPaid
  ) where

import DataTypes

monthlyRate :: LoanConfig -> Double
monthlyRate cfg = annualRate cfg / 100.0 / 12.0

totalPeriods :: LoanConfig -> Int
totalPeriods cfg = years cfg * 12

annuityPayment :: LoanConfig -> Double
annuityPayment cfg =
  let p = principal cfg
      r = monthlyRate cfg
      n = fromIntegral (totalPeriods cfg)
  in if r == 0
        then p / n
        else p * r * (1 + r) ** n / ((1 + r) ** n - 1)

amortizationSchedule :: LoanConfig -> [Payment]
amortizationSchedule cfg =
  case loanType cfg of
    Annuity      -> annuitySchedule cfg
    InterestOnly -> interestOnlySchedule cfg

annuitySchedule :: LoanConfig -> [Payment]
annuitySchedule cfg = go 1 (principal cfg)
  where
    r    = monthlyRate cfg
    n    = totalPeriods cfg
    pmt  = annuityPayment cfg

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

interestOnlySchedule :: LoanConfig -> [Payment]
interestOnlySchedule cfg =
  let r    = monthlyRate cfg
      n    = totalPeriods cfg
      p    = principal cfg
      mkPayment k =
        if k < n
          then Payment k p (p * r) 0 p
          else Payment k p (p * r) p 0
  in map mkPayment [1 .. n]

totalInterestPaid :: [Payment] -> Double
totalInterestPaid = sum . map interestComponent
