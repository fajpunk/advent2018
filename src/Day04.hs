{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day04 where

import           Data.List                      ( sort
                                                , sortOn
                                                )
import           AdventUtil                     ( inputFilePath )
import           Data.Void                      ( Void )
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           Data.Text.Prettyprint.Doc      ( pretty )
import           Data.Foldable                  ( foldl' )
import qualified Data.Map.Strict               as M
import           Debug.Trace

----------------
-- State Machine
----------------
data Timestamp = Timestamp
  { year :: Integer
  , month :: Integer
  , day :: Integer
  , hour :: Integer
  , minute :: Integer
  } deriving (Show)

type GuardID = Integer

data Event
  = StartShift Timestamp GuardID
  | FallAsleep Timestamp
  | Wakeup Timestamp
  deriving (Show)

type Minute = Integer
type Count = Integer

data SleepStats = SleepStats
  { guardID:: GuardID, total :: Integer, byMinute :: M.Map Minute Count }
  deriving (Show, Eq)

instance Ord SleepStats where
  compare SleepStats{total=total1} SleepStats{total=total2} = compare total1 total2

mkSleepStats :: GuardID -> SleepStats
mkSleepStats guardID = SleepStats guardID 0 (M.empty)

data Accumulator = Accumulator
  { guardID :: Integer
  , fellAsleep :: Minute
  , stats :: M.Map GuardID SleepStats
  } deriving (Show)

updateSleepStats :: Minute -> Minute -> SleepStats -> SleepStats
updateSleepStats fellAsleep wokeUp stats@SleepStats { total, byMinute } =
  let asleepFor   = wokeUp - fellAsleep
      newTotal    = total + asleepFor
      minutes     = [fellAsleep .. (wokeUp - 1)]
      newByMinute = foldr (\k m -> M.insertWith (+) k 1 m) byMinute minutes
  in  stats { total = newTotal, byMinute = newByMinute }

processEvent :: Event -> Accumulator -> Accumulator
processEvent (StartShift _ someID) acc = acc { guardID = someID }
processEvent (FallAsleep ts@Timestamp { minute }) acc =
  acc { fellAsleep = minute }
processEvent (Wakeup Timestamp { minute }) acc@Accumulator { guardID, fellAsleep, stats }
  = let guardStats    = M.findWithDefault (mkSleepStats guardID) guardID stats
        newSleepStats = updateSleepStats fellAsleep minute guardStats
        newStats      = M.insert guardID newSleepStats stats
    in  acc { stats = newStats }


----------
-- Parsing
----------
type ParsecM = Parsec Void String

timestampP :: ParsecM Timestamp
timestampP = do
  single '['
  year <- many digitChar
  single '-'
  month <- many digitChar
  single '-'
  day <- many digitChar
  space
  hour <- many digitChar
  single ':'
  minute <- many digitChar
  string "]"
  space
  return
    $ Timestamp (read year) (read month) (read day) (read hour) (read minute)

startshiftP :: ParsecM Event
startshiftP = do
  timestamp <- timestampP
  string "Guard #"
  guardID <- many digitChar
  return $ StartShift timestamp (read guardID)

fallasleepP :: ParsecM Event
fallasleepP = do
  timestamp <- timestampP
  string "falls asleep"
  return $ FallAsleep timestamp

wakesupP :: ParsecM Event
wakesupP = do
  timestamp <- timestampP
  string "wakes up"
  return $ Wakeup timestamp

eventP :: ParsecM Event
eventP = do
  event <- (try startshiftP) <|> (try fallasleepP) <|> (try wakesupP)
  return event

-- Yeah, I know this isn't total and I don't care
parseEvent :: String -> Event
parseEvent line = let (Right event) = parse eventP "" line in event

keyWithMaxValue :: Ord v => M.Map k v -> k
keyWithMaxValue = fst . head . reverse . (sortOn snd) . M.toList

maxValue :: Ord v => M.Map k v -> (k, v)
maxValue = head . reverse . (sortOn snd) . M.toList

day4a :: IO ()
day4a = do
  let input = inputFilePath "day4.txt"
  content <- readFile input
  let unordered = lines content
  let ordered   = sort unordered
  let events    = parseEvent <$> ordered
  let Accumulator { stats } =
        foldl' (flip processEvent) (Accumulator 0 0 M.empty) events
  let list    = M.elems stats
  let guard   = maximum list
  let gid = guardID (guard :: SleepStats)
  let maxMin = keyWithMaxValue (byMinute guard)
  print $ "Guard id: " ++ show gid
  print $ "Minute with the most sleep: " ++ show maxMin
  print $ "Answer: " ++ show (gid * maxMin) -- Part A

  print "------"
  let byLargestMinute = (\SleepStats{guardID, byMinute} -> (guardID, maxValue byMinute)) `fmap` list
  let largest = head . reverse . (sortOn (snd . snd)) $ byLargestMinute
  print largest
  print $ "Guard id: " ++ show (fst largest)
  print $ "Most slept minute: " ++ show ((snd . snd) largest)
  print $ "Answer: " ++ show ((fst largest) * ((fst . snd) largest))
