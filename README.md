# LoanCalculator_haskell

📘 **Loan Calculator – Haskell Functional Programming Mini Project**

A fully functional **loan and interest calculator** built using **pure functional programming principles in Haskell**.  
The system computes **monthly payments**, **total interest**, and generates a full **amortization schedule** for two loan types:

- **Annuity Loan** (Equal Monthly Payment)  
- **Interest-Only Loan**

This project demonstrates strong functional concepts including **pure functions**, **recursion**, **ADTs**, **immutability**, **higher-order functions**, and **modular design**.

---

## 📌 Project Structure

```text
LoanCalculator/
│
├── Main.hs
├── IOHandler.hs
├── Processing.hs
├── DataTypes.hs
└── Utils.hs
Each module handles a specific responsibility:

File	Responsibility
Main.hs	Entry point of the application
IOHandler.hs	Handles user input and formatted output
Processing.hs	Pure financial formulas & amortization logic
DataTypes.hs	Custom ADTs: LoanType, LoanConfig, Payment
Utils.hs	Helper functions (rounding, parsing, etc.)

🎯 Project Goal
Build a pure functional and auditable financial computation engine that:

Calculates monthly loan payment

Builds a full amortization table

Computes interest breakdown

Supports multiple loan types

Avoids all mutation and side-effects (except in the IO module)

This aligns with real-world fintech & banking needs where transparency and reliability are essential.

🧮 Features
✔ Monthly Payment Calculation
Uses the standard annuity formula:

text
Copy code
MonthlyPayment = principal * r * (1 + r)^n / ((1 + r)^n - 1)

where:
  principal = loan amount
  r         = monthly interest rate (annualRate / 12 in decimal)
  n         = total number of monthly payments
✔ Amortization Schedule
For each month, the schedule includes:

Opening balance

Interest portion

Principal repaid

Closing balance

✔ Total Interest Paid
Automatically computed by summing all interest components over the full schedule.

✔ Two Loan Types
Annuity – equal monthly payment

InterestOnly – interest each period, principal repaid at the end

✔ Pure Functional Processing
All financial calculations are:

Pure

Deterministic

Easily testable

🛠️ How to Run
1. Navigate to the project folder
bash
Copy code
cd LoanCalculator
2. Load in GHCi
bash
Copy code
ghci Main.hs
3. Run the program
haskell
Copy code
main
📥 Sample Input
text
Copy code
===== Functional Loan & Interest Calculator =====
Enter principal amount (e.g. 1000000):
1000000
Enter annual interest rate in % (e.g. 12):
12
Enter duration in years (e.g. 10):
10
Loan type: 1 = Annuity, 2 = Interest-only
1
📤 Sample Output
text
Copy code
===== Summary =====
Principal          : 1000000.0
Annual Rate (%)    : 12.0
Years              : 10
Loan Type          : Annuity
Monthly Payment    : 14321.51
Total Interest Paid: 718582.0

Show first 12 periods of amortization schedule:

Month 1 | Open: 1000000.0 | Interest: 10000.0 | Principal: 4321.51 | Close: 995678.49
Month 2 | Open: 995678.49 | Interest: 9956.78 | Principal: 4364.73 | Close: 991313.76
Month 3 | Open: 991313.76 | Interest: 9913.13 | Principal: 4408.38 | Close: 986905.38
...
🧱 Functional Programming Concepts Used
🟦 Pure Functions
All financial formulas in Processing.hs contain no side effects and depend only on their inputs.

🟩 Recursion
The amortization schedule uses a recursive helper function, e.g.:

haskell
Copy code
go :: Int -> Double -> [Payment]
go k balance = ...
🟨 Algebraic Data Types (ADTs)
Example from DataTypes.hs:

h
Copy code
data LoanType = Annuity | InterestOnly
These ADTs model real-world concepts in a type-safe way.

🟧 Immutability
Each monthly Payment is a new immutable value, never modified after creation.

🟪 Higher-Order Functions
Used for processing lists of payments, such as:

map

sum . map ...

take

filter / zipWith (where needed)

🟫 Modularity
Each module handles one concern:

Logic is clean

Code is testable

Design is maintainable

🧩 Possible Extensions
Add balloon payments

Add variable interest rates (stepwise rate changes)

Export amortization schedule to CSV

Add ASCII charts showing balance over time

Build a web UI using Yesod or Scotty frameworks

Add property-based tests (QuickCheck) for financial invariants