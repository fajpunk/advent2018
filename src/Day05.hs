{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import           AdventUtil                     ( inputFilePath )
import           Data.Char                      ( isUpper
                                                , isLower
                                                , toUpper
                                                , toLower
                                                )
import           Data.Sequence                  ( Seq(..)
                                                , singleton
                                                )
import           Debug.Trace


reacts :: Char -> Char -> Bool
reacts c1 c2 =
  ((isUpper c1 && isLower c2) || (isLower c1 && isUpper c2))
    && ((toUpper c1) == (toUpper c2))

react :: [Char] -> Seq Char
react input = go Empty input
 where
  go stack []         = stack
  go Empty (c : rest) = go (singleton c) rest
  go stack@(seen :|> c1) (c2 : rest) =
    if reacts c1 c2 then go seen rest else go (stack :|> c2) rest

replaceAndReact :: [Char] -> Char-> Seq Char
replaceAndReact input char =
  let chars = [toUpper char, toLower char]
      replaced = filter (\char -> not (char `elem` chars)) input
   in react replaced

day5a :: IO ()
day5a = do
  let input = inputFilePath "day5.txt"
  content <- readFile input
  print $ length (react content) -- Why is this off by one?

day5b :: IO ()
day5b = do
  let input = inputFilePath "day5.txt"
  content <- readFile input
  let replacements = (replaceAndReact content) <$> ['a'..'z']
  print $ minimum (length <$> replacements)
