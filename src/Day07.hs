{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  { graphChildren :: M.Map Node (S.Set Node)
  , graphParents :: M.Map Node (S.Set Node)
  }
  deriving (Show)

fromEdges :: [Edge] -> Graph
fromEdges = foldr absorb (Graph M.empty M.empty)
 where
  absorb (src, dst) (Graph children parents) = Graph
    { graphChildren = M.insertWith S.union src (S.singleton dst) children
    , graphParents  = M.insertWith
      S.union
      src
      S.empty
      (M.insertWith S.union dst (S.singleton src) parents)
    }

getReady :: Dependencies -> S.Set Node -> [Node]
getReady parents processed =
  let notProcessed = M.withoutKeys parents processed
  in  M.keys $ M.filter (`S.isSubsetOf` processed) notProcessed

process :: Graph -> [Node]
process graph@(Graph children parents) = findPath (getReady parents S.empty)
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
  } deriving (Show)

mkPool :: Int -> Pool
mkPool numWorkers =
  Pool {free = (take numWorkers $ repeat (Worker '-' 0)), busy = []}

takeJob :: Node -> Worker
takeJob node = Worker node (duration (trace ("Node: " ++ show node ++ "Duration: " ++ show (duration node)) node))

inProgress :: Pool -> S.Set Node
inProgress Pool { free, busy } = S.fromList $ node <$> busy

work :: Pool -> (Pool, [Node])
work Pool { free, busy } =
  let worked            = doWork <$> busy
      (done, stillBusy) = L.partition isDone worked
  in  ((Pool (free ++ done) stillBusy), (node <$> done))
 where
  doWork Worker { node, timeRemaining } = Worker node (timeRemaining - 1)
  isDone Worker { timeRemaining } = timeRemaining == 0

available :: Graph -> S.Set Node -> Pool -> [Node]
available (Graph children parents) processed pool =
  let allReady = getReady parents processed
      working  = inProgress pool
  in  S.toAscList ((S.fromList allReady) S.\\ working)

takeJobs :: Pool -> [Node] -> Pool
takeJobs pool@Pool { free = [], busy } ready = pool
takeJobs pool                          []    = pool
takeJobs Pool { free = (w : ws), busy } (r : rs) =
  Pool ws (busy ++ [takeJob r])

processWithWorkers :: Graph -> Int -> (Int, [Node])
processWithWorkers graph@(Graph children parents) numWorkers =
  let initialPool      = mkPool numWorkers
      initialReady     = (getReady parents S.empty)
      initialInProgress = []
      initialProcessed = S.empty
      initialTime      = 0
      initialPath      = []
  in  tick initialPool initialReady initialInProgress initialProcessed initialTime initialPath
 where
  tick _ [] [] _ time path = (time, path)
  tick pool ready progress processed time path =
    let (workedPool, doneNodes) = work pool
        newProcessed            = S.union processed (S.fromList doneNodes)
        newPool                 = takeJobs workedPool ready
        newPath                 = path ++ doneNodes
        newReady                = available graph newProcessed newPool
        newTime                 = time + 1
        newProgress = S.toList $ inProgress newPool
    in  tick newPool newReady newProgress newProcessed newTime newPath

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
  let blah     = processWithWorkers graph 1
  print blah

