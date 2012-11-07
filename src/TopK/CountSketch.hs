module TopK.CountSketch (
  CountSketch(..),
  Method(..),
  Hash,
  HashSize,
  NoOfHashes,
  BigPrime,
  createHashes,
  Hashable(..),
  topK) where

import Control.Monad
import Data.List (sort, sortBy)
import Data.Ord
import Data.Set (Set, elems, empty, insert)
import System.Random

import Element.Core
import TopK.Core


type HashSize = Int
type NoOfHashes = Int
type BigPrime = Int
type RandomInteger = Int

data Method = Min | MeanMin

data CountSketch = CountSketch Method [Hash]

instance (Hashable e, Ord e) => TopK CountSketch e where
  topK (CountSketch m hs) k es =
    let sk@(Sketch _ _ heap _) = foldr addElement (Sketch m hs empty 0) es in
    take (fromIntegral k) . reverse . sortBy (comparing snd)
      . map (\e -> (e, estimateFrequency e sk)) $ elems heap

data Sketch a = Sketch Method [Hash] (Set a) Integer

addElement :: (Hashable a, Ord a) => a -> Sketch a -> Sketch a
addElement e (Sketch m hashes heap c) =
  Sketch m
         (map (add e) hashes)
         (insert e heap)
         (c + 1)

estimateFrequency :: (Hashable a) => a -> Sketch a -> Integer
estimateFrequency e (Sketch Min hashes _ _) = minimum $ map (hashLookup e) hashes
estimateFrequency e (Sketch MeanMin hashes _ c) =
  let estimates = sort $ map meanEstimate hashes
  in estimates !! (length estimates `div` 2)
  where
    meanEstimate :: Hash -> Integer
    meanEstimate h =
      let sketchCounter = hashLookup e h
          noiseEstimation = (c - sketchCounter) `div` (width h - 1)
      in sketchCounter - noiseEstimation

createHashes :: BigPrime -> HashSize -> NoOfHashes -> IO [Hash]
createHashes p w d = replicateM (fromIntegral d) createHash
  where
    createHash = do
      a <- getStdRandom (randomR (1, p))
      b <- getStdRandom (randomR (1, p))
      return $ Hash a b p (replicate w 0)

data Hash = Hash RandomInteger RandomInteger BigPrime [Integer]

class Hashable a where
  toHash :: a -> Int

instance Hashable NumberedElement where
  toHash (NumberedElement n) = fromIntegral n

hashLookup :: (Hashable a) => a -> Hash -> Integer
hashLookup value h@(Hash _ _ _ xs) =
  head . snd $ splitAt (hash value h) xs

add :: (Hashable a) => a -> Hash -> Hash
add value h@(Hash a b p xs) =
  let (xs', xs'') = splitAt (hash value h) xs
  in Hash a b p (xs' ++ [head xs'' + 1] ++ tail xs'')

hash :: (Hashable a) => a -> Hash -> Int
hash value (Hash a b p xs) = 
  ((a * toHash value + b) `mod` p) `mod` length xs

width :: Hash -> Integer
width (Hash _ _ _ xs) = fromIntegral $ length xs
