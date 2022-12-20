import Control.Monad.Loops (iterateWhile)
import Control.Monad.RWS (RWS, evalRWS, execRWS, get, put, tell)
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M

data RockType = Dash | Plus | Ell | Line | Square deriving (Eq, Show)
type Pos = (Int, Int)

data Rock = Rock {
    rockType :: RockType, 
    pos :: Pos
} deriving (Eq, Show)

data Direction = L | R | D

type Grid = Vector Bool
type State = (Grid, Rock, Int)

getDirection :: Char -> Direction
getDirection '>' = R
getDirection '<' = L
getDirection _ = D

move :: Grid -> Rock -> Direction -> Rock
move g r@(Rock Dash (x,y)) L = if x == 0 || g V.! (7*y+x-1) then r else Rock Dash (x-1,y)
move g r@(Rock Dash (x,y)) R = if x >= 3 || g V.! (7*y+x+4) then r else Rock Dash (x+1,y)
move g r@(Rock Dash (x,y)) D = if any (\(a, b) -> g V.! (7*b+a)) [(x,y-1), (x+1,y-1), (x+2,y-1), (x+3,y-1)] then r else Rock Dash (x,y-1)
move g r@(Rock Ell (x,y)) L = if x == 0 || g V.! (7*y+x+1) || g V.! (7*(y-1)+x+1) || g V.! (7*(y-2)+x-1) then r else Rock Ell (x-1,y)
move g r@(Rock Ell (x,y)) R = if x >= 4 || g V.! (7*y+x+3) || g V.! (7*(y-1)+x+3) || g V.! (7*(y-2)+x+3) then r else Rock Ell (x+1,y)
move g r@(Rock Ell (x,y)) D = if any (\(a, b) -> g V.! (7*b+a)) [(x,y-3), (x+1,y-3), (x+2,y-3)] then r else Rock Ell (x,y-1)
move g r@(Rock Plus (x,y)) L = if x == 0 || g V.! (7*y+x) || g V.! (7*(y-1)+x-1) || g V.! (7*(y-2)+x)  then r else Rock Plus (x-1,y)
move g r@(Rock Plus (x,y)) R = if x >= 4 || g V.! (7*y+x+2) || g V.! (7*(y-1)+x+3) || g V.! (7*(y-2)+x+2) then r else Rock Plus (x+1,y)
move g r@(Rock Plus (x,y)) D = if any (\(a, b) -> g V.! (7*b+a)) [(x,y-2), (x+1,y-3), (x+2,y-2)] then r else Rock Plus (x,y-1)
move g r@(Rock Line (x,y)) L = if x == 0 || g V.! (7*y+x-1) || g V.! (7*(y-1)+x-1) || g V.! (7*(y-2)+x-1) || g V.! (7*(y-3)+x-1) then r else Rock Line (x-1,y)
move g r@(Rock Line (x,y)) R = if x >= 6 || g V.! (7*y+x+1) || g V.! (7*(y-1)+x+1) || g V.! (7*(y-2)+x+1) || g V.! (7*(y-3)+x+1) then r else Rock Line (x+1,y)
move g r@(Rock Line (x,y)) D = if any (\(a, b) -> g V.! (7*b+a)) [(x,y-4)] then r else Rock Line (x,y-1)
move g r@(Rock Square (x,y)) L = if x == 0 || g V.! (7*y+x-1) || g V.! (7*(y-1)+x-1) then r else Rock Square (x-1,y)
move g r@(Rock Square (x,y)) R = if x >= 5 || g V.! (7*y+x+2) || g V.! (7*(y-1)+x+2) then r else Rock Square (x+1,y)
move g r@(Rock Square (x,y)) D = if any (\(a, b) -> g V.! (7*b+a)) [(x,y-2), (x+1,y-2)] then r else Rock Square (x,y-1)

addToGrid :: Grid -> Rock -> Grid
addToGrid g (Rock Dash (x,y)) = V.update g (V.fromList (fmap (\i -> (7*y+i, True)) [x..x+3]))
addToGrid g (Rock Ell (x,y)) = V.update g (V.fromList [((y-2)*7+x, True), ((y-2)*7+x+1, True), ((y-2)*7+x+2, True), ((y-1)*7+x+2, True), (y*7+x+2, True)])
addToGrid g (Rock Plus (x,y)) = V.update g (V.fromList [((y-1)*7+x, True), (y*7+x+1, True), ((y-1)*7+x+1, True), ((y-2)*7+x+1, True), ((y-1)*7+x+2, True)])
addToGrid g (Rock Line (x,y)) = V.update g (V.fromList [(y*7+x, True), ((y-1)*7+x, True), ((y-2)*7+x, True), ((y-3)*7+x, True)])
addToGrid g (Rock Square (x,y)) = V.update g (V.fromList [(y*7+x, True), ((y-1)*7+x, True), (y*7+x+1, True), ((y-1)*7+x+1, True)])

initial :: Grid -> RockType -> Rock
initial g Dash = Rock Dash (2, highest g + 3)
initial g Ell = Rock Ell (2, highest g + 5)
initial g Plus  = Rock Plus (2, highest g + 5)
initial g Line  = Rock Line (2, highest g + 6)
initial g Square  = Rock Square (2, highest g + 4)

nextRockType :: RockType -> RockType
nextRockType Dash = Plus
nextRockType Plus = Ell
nextRockType Ell = Line
nextRockType Line = Square
nextRockType Square = Dash

initialGrid :: Grid
initialGrid = (V.replicate 7 True) V.++ (V.replicate 100000 False)

initialState :: State
initialState = let g = initialGrid in (g, (initial g Dash), 0)

highest :: Grid -> Int
highest g = length $ takeWhile (\xs -> any id xs) . fmap (take 7) . iterate (drop 7) $ (V.toList g)

topRow :: Grid -> [Int]
topRow g = fmap ((7 `subtract`) . last . takeWhile (not . (g V.!))) sequences where
    sequences = fmap (\i -> [i,i-7..]) topRow
    topRow = let h = highest g in [(h*7+7)..(h*7+13)]

normalize :: [Int] -> [Int]
normalize is = let m = minimum is in fmap (m `subtract`) is

groupRows :: [[Int]] -> Map [Int] [Int]
groupRows is = foldl fn M.empty (zip is [0..]) where
    fn r (c, i) = M.insertWith (++) c [i] r

isPeriodic :: [Int] -> Bool
isPeriodic is = length (nub deltas) == 1 where
    deltas = zipWith (-) is (tail is)

getPeriodic :: Map [Int] [Int] -> Map [Int] [Int]
getPeriodic m = M.filter (\a -> length a > 2 && isPeriodic a) m

firstPeriodicIndex :: Map [Int] [Int] -> Int
firstPeriodicIndex m = M.foldr fn (maxBound :: Int) m where
    fn c r = let mn = minimum c in if mn < r then mn else r

doPart2 :: String -> Int
doPart2 s = res where
    p = head . M.toList . getPeriodic . groupRows $ snd (simulate 4000 s)
    (_, (endIdx:startIdx:_)) = p 
    startHeight = getHighest s startIdx
    endHeight = getHighest s endIdx
    rise = endHeight - startHeight
    run = endIdx - startIdx
    middleHeight = ((1000000000000-endIdx) `div` run) * rise
    remainder = (1000000000000-endIdx) `mod` run
    remainderHeight = (getHighest s (startIdx+remainder)) - startHeight
    res = endHeight + middleHeight + remainderHeight

getHighest :: String -> Int -> Int
getHighest s max = let (grid, _, _) = fst (simulate max s) in highest grid - 1

simulate :: Int -> String -> (State, [[Int]])
simulate max s = execRWS (traverse inner s) () initialState where
    inner :: Char -> (RWS () [[Int]] State Bool)
    inner c = do
        (grid, rock, count) <- get
        let newRock1 = move grid rock (getDirection c)
        let newRock2 = move grid newRock1 D
        res <- if count >= max then do
            return False
        else if newRock1 == newRock2 then do
            let newGrid = addToGrid grid newRock2
            put (newGrid, initial newGrid (nextRockType (rockType rock)), count+1)
            tell [normalize . topRow $ newGrid]
            return True
        else do
            put (grid, newRock2, count)
            return True
        return res

main :: IO ()
main = do
    input <- readFile "day17-input.txt"
    let repeated = (concat (replicate 100 input))
    let part1 = getHighest repeated 2022
    print $ part1
    let part2 = doPart2 repeated
    print $ part2
    