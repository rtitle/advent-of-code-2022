import qualified Data.Heap as H
import Data.Heap (MinPrioHeap)
import qualified Data.Map as M
import Data.Map (Map, keys)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Data.Char (ord)
import Data.List (isPrefixOf)

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Eq k, Ord k) => Map k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (M.lookup key distanceMap)

newtype Graph = Graph { 
    edges :: Map String [(String, Int)] 
} deriving (Show)

data DijkstraState = DijkstraState { 
    visitedSet :: Set String, 
    distanceMap :: Map String (Distance Int),
    nodeQueue :: MinPrioHeap (Distance Int) String
}

findShortestDistance :: Graph -> String -> String -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = S.empty
    initialDistances = M.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> Map String (Distance Int)
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Nothing -> d0
      Just ((minDist, node), q1) -> if node == dest then d0
        else if S.member node v0 then processQueue (ds {nodeQueue = q1})
        else
          -- Update the visited set
          let v1 = S.insert node v0
          -- Get all unvisited neighbors of our current node
              allNeighbors = fromMaybe [] (M.lookup node (edges graph))
              unvisitedNeighbors = filter (\(n, _) -> not (S.member n v1)) allNeighbors
          -- Fold each neighbor and recursively process the queue
          in  processQueue $ foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
      let altDistance = addDist (d0 !?? current) (Dist cost)
      in  if altDistance < d0 !?? neighborNode
            then DijkstraState v1 (M.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1)
            else ds

graph1 :: Graph
graph1 = Graph $ M.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]

buildGraph :: String -> Graph
buildGraph s = Graph $ M.fromList edges where
    s' = filter (/= '\n') s
    xlen = length . head . lines $ s
    ylen = length . lines $ s
    len = length s'
    edges :: [(String, [(String, Int)])]
    edges = fmap (\(c, i) -> (c : show i, adjacent i)) (zip s' [0..])
    adjacent :: Int -> [(String, Int)]
    adjacent i = concat [
        if i `mod` xlen > 0 && cmp (s'!!i) (s'!!(i-1)) then [(s'!!(i-1):(show (i-1)), 1)] else [],
        if i `mod` xlen < xlen-1 && cmp (s'!!i) (s'!!(i+1)) then [(s'!!(i+1):(show (i+1)), 1)] else [],
        if i >= xlen && cmp (s'!!i) (s'!!(i-xlen)) then [(s'!!(i-xlen):(show (i-xlen)), 1)] else [],
        if i < len-xlen && cmp (s'!!i) (s'!!(i+xlen)) then [(s'!!(i+xlen):(show (i+xlen)), 1)] else []]
    cmp c1 c2 = ord' c2 - ord' c1 <= 1
    ord' 'S' = ord 'a'
    ord' 'E' = ord 'z'
    ord' c = ord c

findS :: Graph -> String
findS g = head . filter (isPrefixOf "S") . keys . edges $ g

findE :: Graph -> String
findE g = head . filter (isPrefixOf "E") . keys . edges $ g

findAs :: Graph -> [String]
findAs g = filter (isPrefixOf "a") . keys . edges $ g

main :: IO ()
main = do
    input <- readFile "day12-input.txt"
    let graph = buildGraph input
    let part1 = findShortestDistance graph (findS graph) (findE graph)
    let part2 = minimum $ fmap (\a -> findShortestDistance graph a (findE graph)) (findAs graph)
    print $ part1
    print $ part2


