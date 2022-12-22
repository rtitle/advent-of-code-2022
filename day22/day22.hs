import Data.List 
import Data.List.Split (splitWhen)

type Coord = (Int, Int)
type Walls = [Coord]
type Range = (Int, Int)

data Map = Map {
    rows :: [Range],
    cols :: [Range],
    walls :: Walls
} deriving (Show)

data Direction = N | S | E | W

parseRow :: Int -> String -> (Range, Walls)
parseRow idx s = (range, walls) where
    offset = length . (takeWhile space) $ s
    s' = takeWhile (not . space) . dropWhile space $ s
    range = (offset, offset + length s')
    walls = foldr fn [] (zip s' [offset..])
    fn (c, i) r = if c == '#' then ((i,idx):r) else r
    space c = c == ' '

parse :: [String] -> Map
parse xs = Map (fmap fst rows) cols (concat . fmap snd $ rows) where
    rows = fmap (\(r, i) -> parseRow i r) (zip xs [0..])
    cols = fmap (\(c, i) -> fst (parseRow i c)) (zip (transpose xs) (reverse [0..maxWidth]))
    maxWidth = maximum (fmap length xs)

move :: Map -> Coord -> Direction -> Int -> Coord
move m (x,y) dir n = (rowStart + calcX, colStart + calcY) where
    (rowStart, rowEnd) = (rows m) !! y
    (colStart, colEnd) = (cols m) !! x
    lenX = rowEnd - rowStart
    lenY = colEnd - colStart
    calcX = if nx >= 0 then (x - rowStart + (min nx (minWallDistX - 1))) `mod` lenX else (x - rowStart + (max nx (-(lenX - maxWallDistX - 1)))) `mod` lenX 
    calcY = if ny >= 0 then (y - colStart + (min ny (minWallDistY - 1))) `mod` lenY else (y - colStart + (max ny (-(lenY - maxWallDistY - 1)))) `mod` lenY
    (minWallDistX, maxWallDistX, minWallDistY, maxWallDistY) = wallDist m (x,y)
    (nx,ny) = case (dir) of 
        N -> (0,-n)
        E -> (n,0)
        S -> (0,n)
        W -> (-n,0)

wallDist :: Map -> Coord -> (Int, Int, Int, Int)
wallDist m (x,y) = (minWallDistX, maxWallDistX, minWallDistY, maxWallDistY) where
    (rowStart, rowEnd) = (rows m) !! y
    (colStart, colEnd) = (cols m) !! x
    lenX = rowEnd - rowStart
    lenY = colEnd - colStart
    rowWalls = filter (\(wx,wy) -> wy == y) (walls m)
    colWalls = filter (\(wx,wy) -> wx == x) (walls m)
    minWallDistX = myMin . fmap (\(wx, _) -> (wx - x) `mod` lenX) $ rowWalls
    maxWallDistX = myMax . fmap (\(wx, _) -> (wx - x) `mod` lenX) $ rowWalls
    minWallDistY = myMin . fmap (\(_, wy) -> (wy - y) `mod` lenY) $ colWalls
    maxWallDistY = myMax . fmap (\(_, wy) -> (wy - y) `mod` lenY) $ colWalls
    myMin [] = 0
    myMin xs = minimum xs
    myMax [] = 0
    myMax xs = maximum xs

rotateR :: Direction -> Direction
rotateR N = E
rotateR E = S
rotateR S = W
rotateR W = N

rotateL :: Direction -> Direction
rotateL N = W
rotateL E = N
rotateL S = E
rotateL W = S

initialPosition :: Map -> Coord
initialPosition m = (fst . head . rows $ m, 0)

doPart1 :: Map -> String -> Int
doPart1 m s = 
    let (_, (x,y), finalDir) = mv in 
        (4*(x+1)) + (1000*(y+1)) + (facing finalDir)
    where
        mv = foldl fn ("", initialPosition m, E) (s++"Z")
        fn (cur, coord, dir) c = case (c) of
            'R' -> let newCoord = move m coord dir (read @Int cur) in ("", newCoord, rotateR dir)
            'L' -> let newCoord = move m coord dir (read @Int cur) in ("", newCoord, rotateL dir)
            'Z' -> let newCoord = move m coord dir (read @Int cur) in ("", newCoord, dir)
            otherwise -> (cur++[c], coord, dir)
        facing E = 0
        facing S = 1
        facing W = 2
        facing N = 3

main :: IO ()
main = do
    input <- readFile "day22-input.txt"
    let [mapStr, [instructions]] = splitWhen null $ lines input
    let map = parse mapStr
    let part1 = doPart1 map instructions
    print $ part1

    -- 22R34R41L30L
    -- let a = move map (initialPosition map) E 22
    -- print $ a
    -- let b = move map a S 34
    -- print $ b
    -- let c = move map b W 41
    -- print $ c 

    -- -- 10R5L5R10L4R5L5
    -- print $ initialPosition map
    -- let a = move map (initialPosition map) E 10
    -- print $ a
    -- let b = move map a S 5
    -- print $ b
    -- let c = move map b E 5
    -- print $ c
    -- let d = move map c S 10
    -- print $ d
    -- let e = move map d E 4
    -- print $ e
    -- let f = move map e S 5
    -- print $ f 
    -- let g = move map f E 5
    -- print $ g

