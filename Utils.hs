module Utils
  ( readDouble
  , readInt
  , round2
  ) where

readDouble :: String -> Double
readDouble s =
  case reads s of
    [(x, "")] -> x
    _         -> 0.0

readInt :: String -> Int
readInt s =
  case reads s of
    [(x, "")] -> x
    _         -> 0

round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100)) / 100
