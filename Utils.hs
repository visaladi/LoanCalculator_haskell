-- | Utility helper functions for parsing and rounding.
--   These are small, reusable building blocks.
module Utils
  ( readDouble
  , readInt
  , round2
  ) where

-- | Read a 'Double' from a string, returning 0.0 on invalid input.
readDouble :: String -> Double
readDouble s =
  case reads s of
    [(x, "")] -> x
    _         -> 0.0

-- | Read an 'Int' from a string, returning 0 on invalid input.
readInt :: String -> Int
readInt s =
  case reads s of
    [(x, "")] -> x
    _         -> 0

-- | Round a Double to 2 decimal places – useful for monetary values.
round2 :: Double -> Double
round2 x =
  let y :: Integer
      y = round (x * 100)
  in fromIntegral y / 100.0

