module SlipSlop.Distribution (
  Distribution(..),
  ParsableDistribution(..),
  Count,
  Chunks,
  randomNumbers) where

import Data.Ord
import Data.List
import System.Random

type Count = Integer
type Chunks = Integer

randomNumbers :: Count -> Chunks -> Integer -> IO [Integer]
randomNumbers n c mx = do
  rnd <- getStdGen
  return . map snd . sortBy (comparing fst) $ zip (repeat [1..c]) (take (fromIntegral n) $ randomRs (1, mx) rnd)

class Distribution d e where
  generateElements :: d -> Count -> Chunks -> IO [e]

class ParsableDistribution d where
  parseDistribution :: String -> d
