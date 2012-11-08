module SlipSlop.Distribution.Histogram (
  generateElements,
  Histogram(..),
  parseDistribution) where

import Data.Map (Map, fromList, lookup)
import Data.Maybe

import SlipSlop.Distribution
import SlipSlop.Element


data Histogram = Histogram [(NamedElement, Count)]
  deriving Show

instance Distribution Histogram NamedElement where
  generateElements dist n ch = do
    let elMap = elementMap dist
        total = totalCount dist
    numbers <- randomNumbers n ch total
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
