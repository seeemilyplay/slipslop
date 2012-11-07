module TopK.Core (
  TopK(..)) where


class TopK a e where
  topK :: a -> Integer -> [e] -> [(e, Integer)]
