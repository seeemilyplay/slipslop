module Distribution.Core (
  Distribution(..),
  ParsableDistribution(..)) where

class Distribution d e where
  generateElements :: d -> Int -> IO [e]

class ParsableDistribution d where
  parseDistribution :: String -> d
