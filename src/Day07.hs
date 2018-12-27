{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Day07 where

import           AdventUtil                     ( inputFilePath )
import qualified Data.Map.Strict               as M
import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified Data.Char                     as C
import           Debug.Trace

type Node = Char
type Edge = (Node, Node)
type Dependencies = M.Map Node (S.Set Node)

data Graph = Graph
  { children :: M.Map Node (S.Set Node)
  , parents :: M.Map Node (S.Set Node)
  , nodes :: S.Set Node
  }
  deriving (Show)

fromEdges :: [Edge] -> Graph
fromEdges = foldr absorb (Graph M.empty M.empty S.empty)
 where
  absorb (src, dst) Graph { children, parents, nodes } = Graph
    { children = M.insertWith S.union src (S.singleton dst) children
    , parents  = M.insertWith
      S.union
      src
      S.empty
      (M.insertWith S.union dst (S.singleton src) parents)
    , nodes    = (S.insert dst (S.insert src nodes))
    }

getReady :: Dependencies -> S.Set Node -> [Node]
getReady parents processed =
  let notProcessed = M.withoutKeys parents processed
  in  M.keys $ M.filter (`S.isSubsetOf` processed) notProcessed

process :: Graph -> [Node]
process graph@(Graph children parents nodes) = findPath
  (getReady parents S.empty)
  S.empty
  []
  (Graph children parents)
 where
  findPath [] _ path _ = path
  findPath ready processed path graph =
    let (x : xs)     = ready
        newProcessed = S.insert x processed
        newPath      = path ++ [x]
        newReady     = getReady parents newProcessed
    in  findPath newReady newProcessed newPath graph


-- Workers and pools
duration :: Node -> Int
duration node = (C.ord node) - 64 + 60

data Worker = Worker
  { node :: Node
  , timeRemaining :: Int
  } deriving (Show)

data Pool = Pool
  { free :: [Worker]
  , busy :: [Worker]
  , inProgress :: S.Set Node
  } deriving (Show)

mkPool :: Int -> Pool
mkPool numWorkers = Pool
  { free       = (take numWorkers $ repeat (Worker '-' 0))
  , busy       = []
  , inProgress = S.empty
  }

takeJob :: Node -> Worker
takeJob node = Worker node (duration node)

work :: Pool -> (Pool, [Node])
work Pool { free, busy, inProgress } =
  let worked            = doWork <$> busy
      (done, stillBusy) = L.partition isDone worked
      newFree           = free ++ done
      processed         = node <$> done
      newInProgress     = S.difference inProgress (S.fromList processed)
  in  ((Pool newFree stillBusy newInProgress), processed)
 where
  doWork Worker { node, timeRemaining } = Worker node (timeRemaining - 1)
  isDone Worker { timeRemaining } = timeRemaining == 0

takeJobs :: Pool -> [Node] -> Pool
takeJobs pool@Pool { free = [], busy } ready    = pool
takeJobs pool                          []       = pool
takeJobs Pool { free = (w : ws), busy, inProgress } (n : ns) = takeJobs
  Pool
    { free       = ws
    , busy       = (busy ++ [takeJob n])
    , inProgress = (S.insert n inProgress)
    }
  ns

count :: Pool -> Int
count Pool { free, busy } = length free + length busy

processWithWorkers :: Graph -> Int -> (Int, [Node])
processWithWorkers graph@Graph { children, parents, nodes = allNodes } numWorkers
  = let time        = 0
        path        = []
        processed   = S.empty
        ready       = getReady parents processed
        initialPool = mkPool numWorkers
        pool        = takeJobs initialPool ready
        remaining   = allNodes
    in  tick time path pool processed remaining
 where
  tick time path _ _ (S.null -> True) = (time, path)
  tick time path pool processed remaining =
    let newTime                     = (traceShow time time) + 1
        (workedPool, finishedNodes) = work pool
        newPath                     = path ++ (L.sort finishedNodes)
        newProcessed = S.union processed (S.fromList finishedNodes)
        newRemaining                = S.difference remaining newProcessed
        newAvailable                = S.toAscList $ S.difference
          (S.fromList $ getReady parents newProcessed)
          (inProgress workedPool)
        newPool = takeJobs workedPool newAvailable
    in  tick newTime newPath newPool newProcessed newRemaining


parseEdge :: String -> Edge
parseEdge input = (input !! 5, input !! 36)

day7a :: IO ()
day7a = do
  let input = inputFilePath "day7.txt"
  content <- readFile input
  let rawEdges = lines content
  let edges    = parseEdge <$> rawEdges
  let graph    = fromEdges edges
  print graph
  let path = process graph
  print path

day7b :: IO ()
day7b = do
  let input = inputFilePath "day7.txt"
  content <- readFile input
  let rawEdges = lines content
  let edges    = parseEdge <$> rawEdges
  let graph    = fromEdges edges
  -- let pool     = mkPool 5
  -- let newPool  = takeJobs pool "ABCD"
  -- print newPool
  let blah     = processWithWorkers graph 5
  print blah
  print graph

