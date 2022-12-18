import Data.Map (Map)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.RWS (RWS, execRWS, get, modify, rws)

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
    maximum' $ fmap (\s -> if (timeLeft s) > 0 then (curFlow s) + findBestTotalFlow g distances s (timeLeft s) newSeen newTargets else 0) (S.toList newTargets)
      where
        timeLeft target = time - (distances M.! (cur, target)) - 1
        rate target = fst (g M.! target)
        maximum' [] = 0
        maximum' xs = maximum xs
        curFlow target = (rate target) * (timeLeft target)

-- part 2:

rwsPure :: a -> RWS () () (Map (Set String) Int) a
rwsPure a = rws (\r s -> (a, s, ()))

-- a. find 26 minute flow rates, return mapping of Set -> Int
findBestFlowRates :: Graph -> Distances -> String -> Int -> Int -> Set String -> Set String -> RWS () () (Map (Set String) Int) Int
findBestFlowRates g distances cur curFlow time seen targets = 
    let newSeen = S.insert cur seen
        newTargets = S.difference targets newSeen in do
            modify (M.insertWith max (S.delete "AA" newSeen) curFlow)
            mx <- traverse (\s -> if (timeLeft s) > 0 then fmap (+ (nextFlow s)) (findBestFlowRates g distances s (curFlow + (nextFlow s)) (timeLeft s) newSeen newTargets) else rwsPure 0) (S.toList newTargets)
            return $ maximum' mx
      where
        timeLeft target = time - (distances M.! (cur, target)) - 1
        rate target = fst (g M.! target)
        maximum' [] = 0
        maximum' xs = maximum xs
        nextFlow target = (rate target) * (timeLeft target)

-- b. find the best flow rates for missing paths
fillInMissing :: Set String -> RWS () () (Map (Set String) Int) Int
fillInMissing cur = do
    s <- get
    ret <- if M.member cur s then pure (s M.! cur) else do
        mx <- traverse (\x -> fillInMissing (S.delete x cur)) (S.toList cur)
        modify (M.insertWith max cur (maximum mx))
        return $ maximum mx
    return ret

-- c. find the best assignment to human or elephant
findBestHumanAndElephantFlow :: Set String -> Map (Set String) Int -> Int
findBestHumanAndElephantFlow k m = maximum (fmap (\x -> (flow x) + (flow (elephantFlow x))) (M.keys m)) where
    elephantFlow x = S.difference k x
    flow x = m M.! x

main :: IO ()
main = do
    input <- readFile "day16-input.txt"
    let graph = parseGraph $ lines input
    let keys = keyRooms graph
    let distances = findDistances graph keys
    let part1 = findBestTotalFlow graph distances "AA" 30 S.empty keys
    print $ part1
    let flows = fst $ execRWS ((findBestFlowRates graph distances "AA" 0 26 S.empty keys) >>= (\x -> fillInMissing (S.delete "AA" keys))) () M.empty
    let part2 = findBestHumanAndElephantFlow (S.delete "AA" keys) flows
    print $ part2