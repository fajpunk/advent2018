{-# LANGUAGE RecordWildCards #-}
module Day03 where

import           AdventUtil                     ( inputFilePath )
import qualified Data.Set                      as S
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Either


data Point = Point Integer Integer
  deriving (Show, Eq, Ord)

data Square = Square { start :: Point, width :: Integer, height :: Integer, sqid :: Integer }
  deriving (Show)

pointsFrom :: Square -> S.Set Point
pointsFrom Square {..} =
  let (Point startx starty) = start
      points =
        [ Point x y
        | x <- [startx .. startx + width - 1]
        , y <- [starty .. starty + height - 1]
        ]
  in  S.fromList points

type Parser = Parsec String String

squareP :: Parser Square
squareP = do
  _      <- single '#'
  id     <- some numberChar
  _      <- string " @ "
  startx <- some numberChar
  _      <- single ','
  starty <- some numberChar
  _      <- string ": "
  width  <- some numberChar
  _      <- single 'x'
  height <- some numberChar
  return $ Square (Point (read startx) (read starty)) (read width) (read height) (read id)

parseSpec :: String -> Square
parseSpec spec =
  fromRight (Square (Point 0 0) 0 0 0) (parse squareP "whatever" spec)

data Coverage = Coverage { covered :: S.Set Point, coveredMultiple :: S.Set Point }
  deriving (Show)

cover :: Square -> Coverage -> Coverage
cover square Coverage {..} =
  let points     = pointsFrom square
      duplicates = S.intersection covered points
  in  Coverage (S.union covered points) (S.union coveredMultiple duplicates)


isUncovered :: Coverage -> (Square -> Bool)
isUncovered Coverage{coveredMultiple=coveredMultiple} =
  \square -> (S.intersection (pointsFrom square) coveredMultiple) == S.empty

day3a :: IO ()
day3a = do
  let input = inputFilePath "day3.txt"
  content <- readFile input
  let specs    = lines content
  let squares  = parseSpec <$> specs
  let coverage = foldr cover (Coverage S.empty S.empty) squares
  print $ length (coveredMultiple coverage) -- 3a
  let findfunc = isUncovered coverage
  let uncovered = find findfunc squares
  print uncovered -- 3b
  

  -- print coverage
