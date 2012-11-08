module SlipSlop.TopK.Accurate (
  Accurate(..),
  topK) where

import Data.List
import Data.Ord

import SlipSlop.TopK


data Accurate = Accurate
  deriving Show

instance (Eq e, Ord e) => TopK Accurate e where
  topK _ k =
    take (fromIntegral k) . reverse . sortBy (comparing snd) . map (\x -> (head x, fromIntegral $ length x)) . group . sort
