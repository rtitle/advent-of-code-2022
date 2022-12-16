import Control.Monad.Loops (iterateWhile)
import Control.Monad.RWS (RWS, execRWS, get, put, tell)
import Data.List
import Data.List.Split
import qualified Data.Set as S

type Coord = (Int, Int)
type Grid = S.Set Coord
type State = (Grid, Coord)

parseCoord :: String -> Coord
parseCoord s = let [x,y] = splitOn "," s in
    (read @Int x, read @Int y)

parseLine :: String -> [Coord]
parseLine s = foldr fn [] coords where 
    coords = fmap parseCoord $ splitOn " -> " s
    fn (cx, cy) [] = [(cx, cy)]
    fn (cx, cy) r@(h:t) =
        let (rx, ry) = h in
            (if rx < cx then reverse $  fmap (\x -> (x, cy)) [rx..cx]
            else if rx > cx then fmap (\x -> (x, cy)) [cx..rx]
            else if ry < cy then reverse $ fmap (\y -> (cx, y)) [ry..cy]
            else if ry > cy then fmap (\y -> (cx, y)) [cy..ry]
            else [(cx, cy)]) ++ r

bottomRow :: Grid -> Int
bottomRow g = maximum (S.map snd g)

calcSand :: Grid -> Coord -> Coord
calcSand g (x, y) = 
    let down = (x,y+1)
        downLeft = (x-1, y+1)
        downRight = (x+1, y+1) in
            if down `S.member` g then
                if downLeft `S.member` g then
                    if downRight `S.member` g then (x, y)
                    else downRight
                else downLeft
            else down

doStepPart1 :: Int -> (RWS () [Int] State Bool)
doStepPart1 gridMax = do
    (grid, sand) <- get
    let newSand = calcSand grid sand
    if newSand == sand then do 
        put (S.insert newSand grid, (500,0))
        tell [1]
        return True
    else if (snd newSand) > gridMax then do
        return False
    else do
        put (grid, newSand)
        return True

doStepPart2 :: Int -> (RWS () [Int] State Bool)
doStepPart2 gridMax = do
    (grid, sand) <- get
    let newSand = calcSand grid sand
    if newSand == sand then do 
        put (S.insert newSand grid, (500,0))
        tell [1]
        return (newSand /= (500, 0))
    else if (snd newSand) > gridMax then do
        put (S.insert newSand grid, (500,0))
        tell [1]
        return True
    else do
        put (grid, newSand)
        return True

simulate :: Bool -> Grid -> Int
simulate part2 g = sum . snd $ execRWS (iterateWhile id mon) () (g, (500,0)) where
    mon = (if part2 then doStepPart2 else doStepPart1) (bottomRow g)

simulate1 :: Bool -> Grid -> State
simulate1 part2 g = fst $ execRWS mon () (g, (500,0)) where
    mon = (if part2 then doStepPart2 else doStepPart1) (bottomRow g)

main :: IO ()
main = do
    input <- readFile "day14-input.txt"
    let grid = S.fromList . concat . fmap parseLine $ lines input
    let part1 = simulate False grid
    let part2 = simulate True grid
    print $ part1
    print $ part2