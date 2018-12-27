{-# LANGUAGE OverloadedStrings #-}
module Day06 where

import           AdventUtil                     ( inputFilePath )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Text.Read                 ( decimal )
import           Debug.Trace
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( catMaybes )
import           Data.List                      ( sort
                                                , group
                                                , filter
                                                )

data Point = Point { x :: Int, y :: Int }
  deriving (Eq, Ord, Show)

data Box = Box { bottomLeft :: Point, topRight :: Point }
  deriving (Eq, Show)

type PointStats = M.Map Point Int

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

boundingBox :: [Point] -> Box
boundingBox points = foldr
  go
  (Box (Point maxBound maxBound) (Point minBound minBound))
  points
 where
  go (Point x3 y3) (Box (Point x1 y1) (Point x2 y2)) =
    (Box (Point (min x1 x3) (min y1 y3)) (Point (max x2 x3) (max y2 y3)))

borderPoints :: Box -> [Point]
borderPoints (Box (Point x1 y1) (Point x2 y2)) =
  let bottom = (flip Point y1) <$> [x1 .. x2]
      top    = (flip Point y2) <$> [x1 .. x2]
      left   = (Point x1) <$> [(y1 + 1) .. (y2 - 1)]
      right  = (Point x2) <$> [(y1 + 1) .. (y2 - 1)]
  in  bottom ++ top ++ left ++ right

interiorPoints :: Box -> [Point]
interiorPoints (Box (Point x1 y1) (Point x2 y2)) = do
  x <- [(x1 + 1) .. (x2 - 1)]
  y <- [(y1 + 1) .. (y2 - 1)]
  return (Point x y)

closest :: Point -> [Point] -> Point
closest point candidates = foldr1 go candidates
 where
  go cand1 cand2 =
    if (distance cand1 point) < (distance cand2 point) then cand1 else cand2

sumDistances :: [Point] -> Point -> Int
sumDistances points location = sum $ (distance location) <$> points

closestNoTies :: [Point] -> Point -> Maybe Point
closestNoTies (c : cs) point =
  let initialDist = distance point c in go (initialDist, False, c) cs
 where
  go (dist, True , candidate) [] = Nothing
  go (dist, False, candidate) [] = Just candidate
  go (dist, seen, candidate) (x : xs) =
    let newDist = distance point x
    in  case compare newDist dist of
          EQ -> go (dist, True, candidate) xs
          LT -> go (newDist, False, x) xs
          _  -> go (dist, seen, candidate) xs

infinitePoints :: [Point] -> [Point] -> [Point]
infinitePoints points border = (S.toList . S.fromList) $ foldr go [] border
  where go borderPoint acc = acc ++ [(closest borderPoint points)]

parsePoint :: T.Text -> Point
parsePoint input =
  let parts        = T.splitOn "," input
      Right (x, _) = decimal $ T.strip $ parts !! 0
      Right (y, _) = decimal $ T.strip $ parts !! 1
  in  Point x y

day6a :: IO ()
day6a = do
  let input = inputFilePath "day6.txt"
  content <- TIO.readFile input
  let inputPoints  = parsePoint <$> (T.lines content)
  let box          = boundingBox inputPoints
  let interior     = interiorPoints box
  let border       = borderPoints box
  let infinite     = infinitePoints inputPoints border
  let allClosest = catMaybes $ (closestNoTies inputPoints) <$> interior
  let sansInfinite = filter (\x -> not (elem x infinite)) allClosest
  let blah         = (fmap length) . group . sort $ sansInfinite
  print $ length allClosest
  print $ length sansInfinite
  print $ length blah
  print $ maximum blah

day6b = do
  let input = inputFilePath "day6.txt"
  content <- TIO.readFile input
  let inputPoints  = parsePoint <$> (T.lines content)
  let box          = boundingBox inputPoints
  let interior     = interiorPoints box
  let border       = borderPoints box
  let allPoints = border ++ interior
  let distances = (sumDistances inputPoints) <$> allPoints
  let smallDistances = filter (< 10000) distances
  let size = length smallDistances
  print size
