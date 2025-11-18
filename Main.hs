-- | Main entry point of the Loan Calculator application.
--   Keeps IO at the very edge and delegates to IOHandler.
module Main where

import IOHandler (runLoanCalculator)

-- | main: starts the interactive loan calculator.
main :: IO ()
main = runLoanCalculator
