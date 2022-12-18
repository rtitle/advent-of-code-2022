import Control.Monad.Loops (iterateWhile)
import Control.Monad.RWS (RWS, execRWS, get, put)
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V

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
move g r@(Rock Dash (x,y)) L = if x > 0 then Rock Dash (x-1,y) else r
move g r@(Rock Dash (x,y)) R = if x < 3 then Rock Dash (x+1,y) else r
move g r@(Rock Dash (x,y)) D = if any (\(a, b) -> g V.! (7*b+a)) (fmap (\i -> (i, y-1)) [x..x+3]) then r else Rock Dash (x,y-1)
move g r@(Rock Ell (x,y)) L = if x > 0 then Rock Ell (x-1,y) else r
move g r@(Rock Ell (x,y)) R = if x < 4 then Rock Ell (x+1,y) else r
move g r@(Rock Ell (x,y)) D = if any (\(a, b) -> g V.! (7*b+a)) (fmap (\i -> (i, y-3)) [x..x+2]) then r else Rock Ell (x,y-1)
move g r@(Rock Plus (x,y)) L = if x > 0 then Rock Plus (x-1,y) else r
move g r@(Rock Plus (x,y)) R = if x < 4 then Rock Plus (x+1,y) else r
move g r@(Rock Plus (x,y)) D = if g V.! ((y*7)+x-14) || g V.! 
    
    
    any (\(a, b) -> g V.! (7*b+a)) (fmap (\i -> (i, y-3)) [x..x+2]) then r else Rock Plus (x,y-1)
-- todo other rock types

addToGrid :: Grid -> Rock -> Grid
addToGrid g (Rock Dash (x,y)) = V.update g (V.fromList (fmap (\i -> (7*y+i, True)) [x..x+3]))
addToGrid g (Rock Ell (x,y)) = V.update g (V.fromList [((y-2)*7+x, True), ((y-2)*7+x+1, True), ((y-2)*7+x+2, True), ((y-1)*7+x+2, True), (y*7+x+2, True)])
-- todo other rock types

initial :: Grid -> RockType -> Rock
initial g Dash = Rock Dash (2, highest g + 3)
initial g Ell = Rock Ell (2, highest g + 5)
initial g _  = Rock Dash (2, highest g + 3)
-- todo other rock types

nextRockType :: RockType -> RockType
nextRockType Dash = Plus
nextRockType Plus = Ell
nextRockType Ell = Plus
-- nextRockType Ell = Line
-- nextRockType Line = Square
-- nextRockType Square = Dash

initialGrid :: Grid
initialGrid = (V.replicate 7 True) V.++ (V.replicate 32000 False)

initialState :: State
initialState = let g = initialGrid in (g, (initial g Dash), 0)

highest :: Grid -> Int
highest g = length $ takeWhile (\xs -> any id xs) . fmap (take 7) . iterate (drop 7) $ (V.toList g)

simulate :: String -> State
simulate s = fst $ execRWS (iterateWhile (\x -> (maximum x) < 2022) (traverse inner s)) () initialState where
    inner :: Char -> (RWS () () State Int)
    inner c = do
        (grid, rock, count) <- get
        let newRock1 = move grid rock (getDirection c)
        let newRock2 = move grid newRock1 D
        res <- if newRock1 == newRock2 then do
            let newGrid = addToGrid grid newRock2
            put (newGrid, initial newGrid (nextRockType (rockType rock)), count+1)
            return (count+1)
        else do
            put (grid, newRock2, count)
            return count
        return res

main :: IO ()
main = do
    input <- readFile "day17-input.txt"
    let (grid, _, _) = simulate input
    let part1 = highest grid
    print $ part1
    -- let (grid, rock, _) = initialState
    -- print $ initialState
    -- -- print $ move grid rock D
    -- print $ addToGrid grid (move grid rock D)