-- | IO layer for the Loan Calculator.
--   Handles all user interaction and console output,
--   while delegating all business logic to pure functions in 'Processing'.
module IOHandler
  ( runLoanCalculator
  ) where

import DataTypes
import Processing
import Utils

-- | Main interactive flow:
--   1. Ask user for loan parameters.
--   2. Build a 'LoanConfig'.
--   3. Compute amortization schedule (in parallel-capable pure code).
--   4. Print summary and first few payment rows.
runLoanCalculator :: IO ()
runLoanCalculator = do
  putStrLn "===== Functional Loan & Interest Calculator ====="

  -- User inputs
  putStrLn "Enter principal amount (e.g. 1000000):"
  pStr <- getLine

  putStrLn "Enter annual interest rate in % (e.g. 12):"
  rStr <- getLine

  putStrLn "Enter duration in years (e.g. 10):"
  yStr <- getLine

  putStrLn "Loan type: 1 = Annuity, 2 = Interest-only"
  tStr <- getLine

  -- Build configuration (pure data)
  let p  :: Double
      p  = readDouble pStr

      r  :: Double
      r  = readDouble rStr

      y  :: Int
      y  = readInt yStr

      lt :: LoanType
      lt = if tStr == "2" then InterestOnly else Annuity

      cfg :: LoanConfig
      cfg = LoanConfig
              { principal  = p
              , annualRate = r
              , years      = y
              , loanType   = lt
              }

      -- Use the parallel-capable pure schedule function.
      schedule :: [Payment]
      schedule = amortizationScheduleParallel cfg

      monthlyPmt :: Double
      monthlyPmt =
        case loanType cfg of
          Annuity      -> annuityPayment cfg
          InterestOnly -> principal cfg * monthlyRate cfg

      totalInt :: Double
      totalInt = totalInterestPaidParallel schedule

  -- Output summary
  putStrLn "\n===== Summary ====="
  putStrLn $ "Principal          : " ++ show (round2 p)
  putStrLn $ "Annual Rate (%)    : " ++ show (round2 r)
  putStrLn $ "Years              : " ++ show y
  putStrLn $ "Loan Type          : " ++ show lt
  putStrLn $ "Monthly Payment    : " ++ show (round2 monthlyPmt)
  putStrLn $ "Total Interest Paid: " ++ show (round2 totalInt)

  putStrLn "\nShowing first 12 periods of amortization schedule:\n"
  mapM_ printPayment (take 12 schedule)

-- | Nicely format a single 'Payment' row for the console.
printPayment :: Payment -> IO ()
printPayment pay =
  putStrLn $
    "Month " ++ show (period pay) ++
    " | Open: "      ++ show (round2 (openingBalance pay)) ++
    " | Interest: "  ++ show (round2 (interestComponent pay)) ++
    " | Principal: " ++ show (round2 (principalComponent pay)) ++
    " | Close: "     ++ show (round2 (closingBalance pay))
