module Day02 where

import           Data.List
import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           AdventUtil                     ( inputFilePath )


type LetterCounts = M.Map Char Integer

allCounts :: String -> LetterCounts
allCounts boxId = foldr getCount M.empty boxId
 where
  getCount letter acc =
    let curr = M.findWithDefault 0 letter acc in M.insert letter (curr + 1) acc

groupCounts :: LetterCounts -> (Integer, Integer)
groupCounts letterCounts =
  let vals     = M.elems letterCounts
      exactly2 = if 2 `elem` vals then 1 else 0
      exactly3 = if 3 `elem` vals then 1 else 0
  in  (exactly2, exactly3)

sumGroups :: [(Integer, Integer)] -> (Integer, Integer)
sumGroups = foldr1 add where add (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)


-- counts :: String -> M.Map Char Integer
-- counts boxId =
--   foldr count M.empty boxId
--     where
--       let curr = M.findWithDefault letter acc
--       count letter acc = M.insert letter 0 acc

day2a :: IO ()
day2a = do
  let input = inputFilePath "day2.txt"
  content <- readFile input
  let ids      = lines content
  let sorted   = allCounts <$> ids
  let groups   = groupCounts <$> sorted
  let (v1, v2) = sumGroups groups
  print $ v1 * v2

genPairs :: [String] -> [(String, String)]
genPairs myList = do
  (x : rest) <- tails myList
  y          <- rest
  return (x, y)

data PairInfo = PairInfo { numDifferent :: Integer, commonLetters :: [Char] }
  deriving Show

analyzePair :: (String, String) -> PairInfo
analyzePair (id1, id2) = analyze (zip id1 id2) (PairInfo 0 [])
 where
  analyze [] info = info
  analyze (letters : rest) info =
    let (char1, char2) = letters
        PairInfo { numDifferent = different, commonLetters = common } = info
        newInfo        = if char1 == char2
          then PairInfo different (common <> [char1])
          else PairInfo (different + 1) common
    in  analyze rest newInfo

findMatchingBoxes :: [String] -> Maybe PairInfo
findMatchingBoxes ids =
  let pairs = genPairs ids
      infos = analyzePair <$> pairs
  in  find (\pair -> (numDifferent pair) == 1) infos


day2b :: IO ()
day2b = do
  let input = inputFilePath "day2.txt"
  content <- readFile input
  let ids     = lines content
  let boxInfo = findMatchingBoxes (ids)
  print boxInfo
