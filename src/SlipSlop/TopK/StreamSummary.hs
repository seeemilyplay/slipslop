module SlipSlop.TopK.StreamSummary (
  StreamSummary(..),
  SSlots,
  topK) where

import Control.Arrow
import Data.List (minimumBy, sortBy)
import Data.Map (Map, assocs, delete, empty, lookup, insert, size)
import Data.Ord (comparing)

import SlipSlop.TopK


type SSlots = Integer

data StreamSummary = StreamSummary SSlots

instance (Ord e) => TopK StreamSummary e where
  topK (StreamSummary slots) k =
    map (second estimate) . take (fromIntegral k) . reverse . sortBy (comparing snd) . assocs . foldr (addElement slots) empty

addElement :: (Ord e) => SSlots -> e -> Map e Tally -> Map e Tally
addElement slots el tallies =
  case (Data.Map.lookup el tallies, size tallies < fromIntegral slots) of
    (Just (Tally c e), _) -> insert el (Tally (c+1) (e+1)) tallies
    (Nothing, True) -> insert el (Tally 1 1) tallies
    (Nothing, False) ->
      let (k, Tally c _) = minimumBy (comparing snd) $ assocs tallies
      in insert el (Tally (c+1) c) $ delete k tallies

type Count = Integer
type Error = Integer

data Tally = Tally Count Error
  deriving (Eq, Show)

instance Ord Tally where
  compare = comparing estimate

estimate :: Tally -> Integer
estimate (Tally c _) = c
