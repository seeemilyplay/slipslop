module SlipSlop.TopK.BufferedStreamSummary (
  BufferedStreamSummary(..),
  BSlots,
  topK) where

import Data.List (find, sortBy)
import Data.Ord (comparing)

import SlipSlop.TopK

type BSlots = Integer

data BufferedStreamSummary = BufferedStreamSummary BSlots BSlots

instance (Eq e, Ord e) => TopK BufferedStreamSummary e where
  topK (BufferedStreamSummary s1 s2) k =
    take (fromIntegral k) . reverse . sortBy (comparing snd) . map (\(Aggregate e c _) -> (e, c)) . snd . foldr (addElement s1 s2) ([], [])

addElement :: (Eq e, Ord e) => BSlots -> BSlots -> e -> ([Aggregate e], [Aggregate e]) -> ([Aggregate e], [Aggregate e])
addElement s1 s2 el (xs, ys) = shuntAndCull s1 (increment el xs) s2 ys

shuntAndCull :: (Eq e) => BSlots -> [Aggregate e] -> BSlots -> [Aggregate e] -> ([Aggregate e], [Aggregate e])
shuntAndCull s1 xs s2 ys =
  let ys' = foldr shunt ys xs
      xs' = foldr washBack xs ys'
  in (cullOldest s1 xs', cullLeast s2 ys')

shunt :: (Eq e) => Aggregate e -> [Aggregate e] -> [Aggregate e]
shunt (Aggregate el c _) xs =
  case find ((== el) . element) xs of
    Nothing -> (Aggregate el c (fakeNow xs)) : xs
    Just (Aggregate _ c' _) -> (Aggregate el (max c c') (fakeNow xs)) : filter ((/= el) . element) xs

washBack :: (Eq e) => Aggregate e -> [Aggregate e] -> [Aggregate e]
washBack (Aggregate el c _) xs =
  case find ((== el) . element) xs of
    Just (Aggregate _ c' _) | c > c' -> incrementBy c el xs
    _ -> xs

cullOldest :: BSlots -> [Aggregate e] -> [Aggregate e]
cullOldest s xs = take (fromIntegral s) . reverse $ sortBy (comparing time) xs

cullLeast :: BSlots -> [Aggregate e] -> [Aggregate e]
cullLeast s xs = take (fromIntegral s) . reverse $ sortBy (comparing count) xs

type Count = Integer
type Time = Integer

data Aggregate e = Aggregate e Count Time

element :: Aggregate e -> e
element (Aggregate e _ _) = e

count :: Aggregate e -> Count
count (Aggregate _ c _) = c

time :: Aggregate e -> Time
time (Aggregate _ _ t) = t

increment :: (Eq e) => e -> [Aggregate e] -> [Aggregate e]
increment = incrementBy 1

incrementBy :: (Eq e) => Integer -> e ->  [Aggregate e] -> [Aggregate e]
incrementBy n el xs =
  case find ((== el) . element) xs of
    Nothing -> (Aggregate el n (fakeNow xs)) : xs
    Just (Aggregate _ c _) -> (Aggregate el (c+n) (fakeNow xs)) : filter ((/= el) . element) xs

fakeNow :: [Aggregate e] -> Time
fakeNow [] = 1
fakeNow xs = (+1) . minimum $ map time xs