import Data.Map (Map)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Room = (Int, [String])
type Graph = Map String Room
type Distances = Map (String, String) Int

parseLine :: String -> (String, Room)
parseLine s = (name, (flow, conns)) where
    sp = splitOn " " s'
    s' = filter (not . (`elem` ",;")) s
    name = sp !! 1
    flow = read @Int $ (splitOn "=" (sp !! 4)) !! 1
    conns = drop 9 sp

parseGraph :: [String] -> Graph
parseGraph s = M.fromList $ fmap parseLine s

keyRooms :: Graph -> Set String
keyRooms = M.keysSet . (M.filterWithKey f) where
    f k v = k == "AA" || (fst v) > 0

-- bfs to find distances between each room
findDistances :: Graph -> Set String -> Distances
findDistances g keyRooms = M.unions $ fmap (\s -> fn (zeroDistances s) 0 s [s]) (S.toList keyRooms) where
    fn :: Distances -> Int -> String -> [String] -> Distances
    fn distances depth startRoom [] = M.empty 
    fn distances depth startRoom curRooms = 
        let (newDistances, next) = foldl f (distances, S.empty) (concat . fmap connections $ curRooms) in
            M.union newDistances (fn newDistances (depth+1) startRoom (S.toList next))
        where
            f (r, next) c = if M.notMember (startRoom, c) r then (M.insert (startRoom, c) (depth+1) r, S.insert c next) else (r, next)
    zeroDistances a = M.singleton (a,a) 0
    connections a = snd (g M.! a)

-- find the best total flow
findBestTotalFlow :: Graph -> Distances -> String -> Int -> Set String -> Set String -> Int
findBestTotalFlow g distances cur time seen targets = 
    let newSeen = S.insert cur seen
        newTargets = S.difference targets newSeen in 
    maximum' $ fmap (\s -> if (timeLeft s) > 0 then ((rate s) * (timeLeft s)) + findBestTotalFlow g distances s (timeLeft s) newSeen newTargets else 0) (S.toList newTargets)
      where
        timeLeft target = time - (distances M.! (cur, target)) - 1
        rate target = fst (g M.! target)
        maximum' [] = 0
        maximum' xs = maximum xs

-- part 2 todo:
-- a. find 26 minute flow rates, return mapping of Set -> Int
-- b. find the best flow rates for missing paths
-- c. find the best assignment to human or elephant


main :: IO ()
main = do
    input <- readFile "day16-input.txt"
    let graph = parseGraph $ lines input
    let keys = keyRooms graph
    let distances = findDistances graph keys
    let part1 = findBestTotalFlow graph distances "AA" 30 S.empty keys
    print $ part1