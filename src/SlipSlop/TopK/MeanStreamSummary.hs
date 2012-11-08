module SlipSlop.TopK.MeanStreamSummary (
  MeanStreamSummary(..),
  MsSlots,
  topK) where

import Control.Arrow
import Data.List (minimumBy, sortBy)
import Data.Map (Map, assocs, delete, empty, lookup, insert, size)
import Data.Ord (comparing)

import SlipSlop.TopK


type MsSlots = Integer

data MeanStreamSummary = MeanStreamSummary MsSlots

instance (Ord e) => TopK MeanStreamSummary e where
  topK (MeanStreamSummary slots) k =
    map (second estimate) . take (fromIntegral k) . reverse . sortBy (comparing snd) . assocs . foldr (addElement slots) empty

addElement :: (Ord e) => MsSlots -> e -> Map e Tally -> Map e Tally
addElement slots el tallies =
  case (Data.Map.lookup el tallies, size tallies < fromIntegral slots) of
    (Just (Tally c t s), _) -> insert el (Tally (c+1) (t+1) s) tallies
    (Nothing, True) -> insert el (Tally 1 1 1) tallies
    (Nothing, False) ->
      let (k, leastTally) = minimumBy (comparing (stickiness . snd)) $ assocs tallies
      in insert el (replace leastTally) $ delete k tallies

type Count = Integer
type Total = Integer
type Swaps = Integer

data Tally = Tally Count Total Swaps
  deriving (Eq, Show)

instance Ord Tally where
  compare = comparing estimate

replace :: Tally -> Tally
replace (Tally _c t s) = Tally (t `div` s) (t+1) (s+1)

stickiness :: Tally -> Integer
stickiness (Tally c t _s) = c * t

estimate :: Tally -> Integer
estimate (Tally c _t _s) = c
