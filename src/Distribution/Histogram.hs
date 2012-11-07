module Distribution.Histogram (
  generateElements,
  Count,
  Histogram(..),
  parseDistribution) where

import Data.Map (Map, fromList, lookup)
import Data.Maybe
import System.Random

import Distribution.Core
import Element.Core


type Count = Integer

data Histogram = Histogram [(NamedElement, Count)]
  deriving Show

instance Distribution Histogram NamedElement where
  generateElements dist n = do
    let elMap = elementMap dist
        total = totalCount dist
    rnd <- getStdGen
    let numbers = take n $ randomRs (1, total) rnd
    return $ map (fromJust . flip Data.Map.lookup elMap) numbers
    where
      totalCount :: Histogram -> Integer
      totalCount (Histogram dist') = sum $ map snd dist'

      elementMap :: Histogram -> Map Integer NamedElement
      elementMap (Histogram dist') = fromList $ f 0 dist'
        where
          f :: Integer -> [(NamedElement, Count)] -> [(Integer, NamedElement)]
          f _ [] = []
          f cc ((e, c):xs) =
            zip (map (+cc) [1..])
                (replicate (fromIntegral c) e)
              ++ f (cc + c) xs

instance ParsableDistribution Histogram where
  parseDistribution = Histogram . map ((\ws -> (NamedElement $ head ws, read $ ws!!1)) . words) . lines . map (\e -> if e `elem` "\" (,) " then ' ' else e)