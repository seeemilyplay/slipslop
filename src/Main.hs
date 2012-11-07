module Main (main) where

import Control.Applicative
import System.Environment
import System.Exit

import Distribution.Core
import Distribution.CountHistogram
import Distribution.Histogram
import TopK.Accurate
import TopK.CountSketch
import TopK.MeanStreamSummary
import TopK.StreamSummary
import Element.Core

main :: IO ()
main = do
  args <- getArgs
  let n = argNumber args "-n" 100
      minput = argFile args
  case minput of
    Just input | "histogram" `elem` args -> do
      (dist :: Histogram) <- parseDistribution <$> readFile input
      (els :: [NamedElement]) <- generateElements dist (fromIntegral n)
      runTopK els
    Just input | "count-histogram" `elem` args -> do
      (dist :: CountHistogram) <- parseDistribution <$> readFile input
      (els :: [NumberedElement]) <- generateElements dist (fromIntegral n)
      runTopK els
    _ -> do
      c <- getContents
      runTopK (map NamedElement $ lines c)
  where
    runTopK :: (Hashable e, Ord e, Show e) => [e] -> IO ()
    runTopK els = do
      args <- getArgs
      let k = argNumber args "-k" 10
          s = argNumber args "-s" (k + (k `div` 2))
          w = argNumber args "-w" 100
          d = argNumber args "-d" (k `div` 2)
          p = argNumber args "-p" 2147483647
      result <- case args of
        ("count-min-sketch":_) -> do
          hashes <- createHashes (fromIntegral p) (fromIntegral w) (fromIntegral d)
          return $ topK (CountSketch Min hashes) k els
        ("count-mean-min-sketch":_) -> do
          hashes <- createHashes (fromIntegral p) (fromIntegral w) (fromIntegral d)
          return $ topK (CountSketch MeanMin hashes) k els
        ("stream-summary":_) -> return $ topK (StreamSummary s) k els
        ("mean-stream-summary":_) -> return $ topK (MeanStreamSummary s) k els
        _ -> badArgExit
      let accurateResult = topK Accurate k els
      putStrLn "Accurate Result"
      putStrLn $ show accurateResult
      putStrLn "Our Result"
      putStrLn $ show result
    
    argNumber :: [String] -> String -> Integer -> Integer
    argNumber [] _ d = d
    argNumber (x:(y:_)) f _ | x==f = read y
    argNumber (_:xs) f d = argNumber xs f d

    argFile :: [String] -> Maybe FilePath
    argFile [] = Nothing
    argFile xs = Just (head $ reverse xs)

    usage :: String
    usage = "Usage: slipslop [count-min-sketch|count-mean-min-sketch|stream-summary|mean-stream-summary] [Options...]? [[histogram|count-histogram] file]?\n\n"
            ++ "  options:\n"
            ++ "    -k INT   top k\n"
            ++ "    -n INT   number of elements to try\n\n"
            ++ "  count-min-sketch options:\n"
            ++ "    -w INT   width of hash arrays\n"
            ++ "    -d INT   number of hash arrays\n"
            ++ "    -p INT   some large prime number (defaults to 2147483647)\n\n"
            ++ "  count-min-mean-sketch options:\n"
            ++ "    -w INT   width of hash arrays\n"
            ++ "    -d INT   number of hash arrays\n"
            ++ "    -p INT   some large prime number (defaults to 2147483647)\n\n"
            ++ "  stream-summary options:\n"
            ++ "    -s INT   number of slots\n\n"
            ++ "  mean-stream-summary options:\n"
            ++ "    -s INT   number of slots\n\n"

    badArgExit :: IO a
    badArgExit = do
      putStrLn usage
      exitFailure