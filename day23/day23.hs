import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

type Coord = (Int, Int)
data Direction = N | S | E | W deriving (Eq, Show)

data State = State {
    rnd :: Int,
    elves :: Set Coord,
    dirs :: [Direction],
    steady :: Bool
} deriving (Show)

allDirections :: [[Direction]]
allDirections = [[N], [S], [E], [W], [N, E], [N, W], [S, E], [S, W]]

move :: Coord -> [Direction] -> Coord
move pos [] = pos
move (x,y) [N] = (x, y-1)
move (x,y) [S] = (x, y+1)
move (x,y) [E] = (x+1, y)
move (x,y) [W] = (x-1, y)
move pos [a, b] = move (move pos [a]) [b]

initialState :: [String] -> State
initialState s = State 0 (snd elves) [N, S, W, E] False where
    elves = foldl f (0, S.empty) s
    f (y, r) c = (y+1, snd (inner r y c))
    inner cur y line = foldl (innerF y) (0, cur) line
    innerF y (x, r) c = if c == '#' then (x+1, S.insert (x,y) r) else (x+1, r)

findEmptyAdjacent :: State -> Coord -> [[Direction]]
findEmptyAdjacent (State _ elves _ _) pos = filter (\d -> (move pos d) `S.notMember` elves) allDirections

makeProposal :: State -> Coord -> Maybe Coord
makeProposal s pos = fmap (\d -> move pos [d]) maybeProposal where
    adj = findEmptyAdjacent s pos
    maybeProposal = if length adj == 8 then Nothing else find allEmpty (dirs s)
    allEmpty d = length (filter (== d) (concat adj)) == 3

doStep :: State -> State
doStep s@(State rnd elves (d:ds) _) = State (rnd+1) applyProposals (ds ++ [d]) (applyProposals == elves) where
    elfProposals = fmap (\e -> (e, (makeProposal s e))) (S.toList elves)
    proposalCounts = foldr fn M.empty elfProposals
    fn (_, Just p) r = M.insertWith (+) p 1 r
    fn (_, Nothing) r = r
    applyProposal (e, Just p) = if proposalCounts M.! p == 1 then p else e
    applyProposal (e, Nothing) = e
    applyProposals = S.fromList $ fmap applyProposal elfProposals

doPart1 :: State -> Int 
doPart1 s = calc where
    calc = ((maxX - minX + 1) * (maxY - minY + 1)) - (S.size sim10)
    minX = minimum (S.map fst sim10)
    maxX = maximum (S.map fst sim10)
    minY = minimum (S.map snd sim10)
    maxY = maximum (S.map snd sim10)
    sim10 = elves (foldr (\_ r -> if (steady r) then r else doStep r) s (replicate 10 0))

-- ugly but gives correct answer
doPart2 :: State -> Int 
doPart2 s = rnd sim10k where 
    sim10k = foldr (\_ r -> if (steady r) then r else doStep r) s (replicate 10000 0)

main :: IO ()
main = do
    input <- readFile "day23-input.txt"
    let initial = initialState $ lines input
    let part1 = doPart1 initial
    print $ part1
    let part2 = doPart2 initial
    print $ part2