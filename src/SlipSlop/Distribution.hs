module SlipSlop.Distribution (
  Distribution(..),
  ParsableDistribution(..),
  Count) where

type Count = Integer

class Distribution d e where
  generateElements :: d -> Int -> IO [e]

class ParsableDistribution d where
  parseDistribution :: String -> d
