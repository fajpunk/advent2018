{-# LANGUAGE CPP #-}
module Day01 where

import System.FilePath
import qualified Data.Set as S

inputFilePath :: FilePath -> FilePath
inputFilePath file = (takeDirectory $ takeDirectory __FILE__) </> "input" </> file

parseInt :: String -> Integer
parseInt ('+':base) = read base
parseInt num = read num

day1a :: IO ()
day1a = do
  let input = inputFilePath "day1.txt"
  content <- readFile input
  let unparsed = lines content
  let parsed = parseInt <$> unparsed
  let total = sum parsed
  putStrLn $ show total


firstRepeat :: [Integer] -> Integer
firstRepeat changes = 
  let initial = S.empty
      signals = scanl1 (+) (cycle changes)
  in  go initial signals
  where 
    go seen (x:xs) =
      if S.member x seen
         then x
         else go (S.insert x seen) xs
      
day1b :: IO ()
day1b = do
  let input = inputFilePath "day1.txt"
  content <- readFile input
  let unparsed = lines content
  let parsed = parseInt <$> unparsed
  print $ firstRepeat parsed
