import Data.List 
import Data.List.Split (splitWhen)

type Coord = (Int, Int)
type Walls = [Coord]
type Range = (Int, Int)

type Coord3 = (Int, Int, Int)
type Position = (Int, Int, Int, Direction)
type Walls3 = [Coord3]

data Map = Map {
    rows :: [Range],
    cols :: [Range],
    walls :: Walls
} deriving (Show)

data Map3 = Map3 {
    len :: Int,
    walls3 :: Walls3
} deriving (Show)

data Direction = N | S | E | W deriving (Show)

-- part 1
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

-- part 2
parseRow3Test :: Int -> String -> Walls3
parseRow3Test idx s = 
    if idx <= 3 then face 1 s'
    else if idx <= 7 then face 2 (take 4 s') ++ face 3 (take 4 . drop 4 $ s') ++ face 4 (drop 8 s')
    else face 5 (take 4 s') ++ face 6 (drop 4 s')
    where
        s' = takeWhile (not . space) . dropWhile space $ s
        space c = c == ' '
        face f xs = foldr (\(c, i) r -> if c == '#' then ((i,idx `mod` 4,f):r) else r) [] (zip xs [0..])

parseRow3 :: Int -> String -> Walls3
parseRow3 idx s = 
    if idx < 50 then face 1 (take 50 s') ++ face 2 (drop 50 s')
    else if idx < 100 then face 3 s'
    else if idx < 150 then face 4 (take 50 s') ++ face 5 (drop 50 s')
    else face 6 s'
    where
        s' = takeWhile (not . space) . dropWhile space $ s
        space c = c == ' '
        face f xs = foldr (\(c, i) r -> if c == '#' then ((i,idx `mod` 50,f):r) else r) [] (zip xs [0..])

parse3 :: Bool -> [String] -> Map3
parse3 test s = Map3 (if test then 4 else 50) (concat $ fmap (\(r, i) -> if test then parseRow3Test i r else parseRow3 i r) (zip s [0..]))

-- part 1
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

rotateR3 :: Position -> Position
rotateR3 (x, y, f, d) = (x, y, f, rotateR d)

rotateL3 :: Position -> Position
rotateL3 (x, y, f, d) = (x, y, f, rotateL d)

-- part 2
move1 :: Map3 -> Position -> Position
move1 m (x,y,f,dir) = 
    let res@(newX, newY, newF, _) = calc in
        if (newX, newY, newF) `elem` (walls3 m) then (x,y,f,dir) else res
    where
    calc = let x' = x + nx
               y' = y + ny in
                   if x' < 0 || x' >= ln || y' < 0 || y' >= ln then nextFace else (x', y', f, dir)
    nextFace = case (f, dir) of 
        -- fake
        -- (1, E) -> (ln-y-1, 0, 4, S)
        -- (1, W) -> (y, 0, 3, S)
        -- (1, N) -> (x, ln-1, 6, N)
        -- (1, S) -> (x, 0, 2, S)
        -- (2, E) -> (0, y, 4, E)
        -- (2, W) -> (ln-1, y, 3, W)
        -- (2, N) -> (x, ln-1, 1, N)
        -- (2, S) -> (x, 0, 5, S)
        -- (3, E) -> (0, y, 2, E)
        -- (3, W) -> (0, (ln-y-1), 6, E)
        -- (3, N) -> (0, x, 1, E)
        -- (3, S) -> (0, ln-x-1, 5, E)
        -- (4, E) -> (ln-1, ln-y-1, 6, W)
        -- (4, W) -> (ln-1, y, 2, W)
        -- (4, N) -> (ln-1, ln-x-1, 1, W)
        -- (4, S) -> (ln-1, x, 5, W)
        -- (5, E) -> (y, ln-1, 4, N)
        -- (5, W) -> (ln-y-1, ln-1, 3, N)
        -- (5, N) -> (x, ln-2, 2, N)
        -- (5, S) -> (x, 0, 6, S)
        -- (6, E) -> (ln-1, ln-y-1, 4, W)
        -- (6, W) -> (0, ln-y-1, 3, E)
        -- (6, N) -> (x, ln-1, 5, N)
        -- (6, S) -> (x, 0, 1, S)

        -- test input
        -- (1, E) -> (ln-1, ln-y-1, 6, W)
        -- (1, W) -> (y, 0, 3, S)
        -- (1, N) -> (ln-x-1, 0, 2, S)
        -- (1, S) -> (x, 0, 4, S)
        -- (2, E) -> (0, y, 3, E)
        -- (2, W) -> (ln-y-1, ln-1, 6, N)
        -- (2, N) -> (ln-x-1, 0, 1, S)
        -- (2, S) -> (ln-x-1, ln-1, 5, N)
        -- (3, E) -> (0, y, 4, E)
        -- (3, W) -> (ln-1, y, 2, W)
        -- (3, N) -> (0, x, 1, E)
        -- (3, S) -> (0, ln-x-1, 5, E)
        -- (4, E) -> (ln-y-1, 0, 6, S)
        -- (4, W) -> (ln-1, y, 3, W)
        -- (4, N) -> (x, ln-1, 1, N)
        -- (4, S) -> (x, 0, 5, S)
        -- (5, E) -> (0, y, 6, E)
        -- (5, W) -> (ln-y-1, ln-1, 3, N)
        -- (5, N) -> (x, ln-1, 4, N)
        -- (5, S) -> (ln-x-1, ln-1, 2, N)
        -- (6, E) -> (ln-1, ln-y-1, 1, W)
        -- (6, W) -> (ln-1, y, 5, W)
        -- (6, N) -> (ln-1, ln-x-1, 4, W)
        -- (6, S) -> (0, ln-x-1, 2, E)

        -- real input
        (1, E) -> (0, y, 2, E)
        (1, W) -> (0, ln-y-1, 4, E)
        (1, N) -> (0, x, 6, E)
        (1, S) -> (x, 0, 3, S)
        (2, E) -> (ln-1, ln-y-1, 5, W)
        (2, W) -> (ln-1, y, 1, W)
        (2, N) -> (x, ln-1, 6, N)
        (2, S) -> (ln-1, x, 3, W)
        (3, E) -> (y, ln-1, 2, N)
        (3, W) -> (y, 0, 4, S)
        (3, N) -> (x, ln-1, 1, N)
        (3, S) -> (x, 0, 5, S)
        (4, E) -> (0, y, 5, E)
        (4, W) -> (0, ln-y-1, 1, E)
        (4, N) -> (0, x, 3, E)
        (4, S) -> (x, 0, 6, S)
        (5, E) -> (ln-1, ln-y-1, 2, W)
        (5, W) -> (ln-1, y, 4, W)
        (5, N) -> (x, ln-1, 3, N)
        (5, S) -> (ln-1, x, 6, W)
        (6, E) -> (y, ln-1, 5, N)
        (6, W) -> (y, 0, 1, S)
        (6, N) -> (x, ln-1, 4, N)
        (6, S) -> (x, 0, 2, S)

    ln = len m
    (nx,ny) = case (dir) of 
        N -> (0,-1)
        E -> (1,0)
        S -> (0,1)
        W -> (-1,0)

initialPosition :: Map -> Coord
initialPosition m = (fst . head . rows $ m, 0)

initialPosition3 :: Position
initialPosition3 = (0,0,1,E)

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

doPart2 :: Map3 -> String -> Int
doPart2 m s = 
    let (_, p@(x,y,f,d)) = mv 
        (offsetX, offsetY) = offset f in
        (4*(x+1+offsetX)) + (1000*(y+1+offsetY)) + (facing d)
    where
        mv = foldl fn ("", initialPosition3) (s++"Z")
        fn (cur, pos) c = case (c) of
            'R' -> let newPos = foldr (\_ r -> move1 m r) pos (replicate (read @Int cur) 0) in ("", rotateR3 newPos)
            'L' -> let newPos = foldr (\_ r -> move1 m r) pos (replicate (read @Int cur) 0) in ("", rotateL3 newPos)
            'Z' -> let newPos = foldr (\_ r -> move1 m r) pos (replicate (read @Int cur) 0) in ("", newPos)
            otherwise -> (cur++[c], pos)
        facing E = 0
        facing S = 1
        facing W = 2
        facing N = 3
        ln = len m
        -- test
        -- offset 1 = (0,0)
        -- offset 2 = (0, ln)
        -- offset 3 = (ln, ln)
        -- offset 4 = (ln*2, ln)
        -- offset 5 = (0, ln*2)
        -- offset 6 = (ln, ln*2)
        -- real
        offset 1 = (0,0)
        offset 2 = (ln, 0)
        offset 3 = (0,ln)
        offset 4 = (0,ln*2)
        offset 5 = (ln,ln*2)
        offset 6 = (0,ln*3)

main :: IO ()
main = do
    input <- readFile "day22-input.txt"
    let [mapStr, [instructions]] = splitWhen null $ lines input
    let map = parse mapStr
    let part1 = doPart1 map instructions
    print $ part1
    let map3 = parse3 False mapStr
    let part2 = doPart2 map3 (instructions)
    print $ part2


