import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Coord = (Int, Int, Int)
data Direction = N | S | E | W | U | D deriving (Eq, Show, Ord)

parseLine :: String -> [Int]
parseLine s = fmap (read @Int) $ splitOn "," s

build2dMaps :: [[Int]] -> [Map (Int, Int) [Int]]
build2dMaps is = foldr fn (replicate 3 M.empty) is where
    fn [x,y,z] [xy, xz, yz] = [M.insertWith (++) (x,y) [z] xy, M.insertWith (++) (x,z) [y] xz, M.insertWith (++) (y,z) [x] yz]

build3dMap :: [[Int]] -> Set Coord
build3dMap is = foldr fn (S.empty) is where
    fn [x,y,z] s = S.insert (x,y,z) s

getSurfaceAreaPart1 :: [Map (Int, Int) [Int]] -> [[Int]] -> Int
getSurfaceAreaPart1 maps is = foldr fn 0 is where
    fn [x,y,z] total = total + (6 - (check z (xy M.! (x,y))) - (check y (xz M.! (x,z))) - (check x (yz M.! (y,z))))
    xy = maps !! 0
    xz = maps !! 1
    yz = maps !! 2
    check a as = count (\i -> abs (i - a) == 1) as

count :: (a -> Bool) -> [a] -> Int
count f as = length $ filter f as

-- BFS over whole space - exterior surface area is all reached nodes
getSurfaceAreaPart2 :: Set Coord -> Int
getSurfaceAreaPart2 all = foldl total 0 (M.toList $ inner [(myMin, myMin, myMin)] S.empty M.empty) where
    total r (k, v) = r + (S.size v)
    myMin = -2
    myMax = 25
    inner :: [Coord] -> Set Coord -> Map Coord (Set Direction) -> Map Coord (Set Direction)
    inner [] _ exterior = exterior
    inner cur visited exterior =
        let (newExterior, next) = foldl fn (exterior, []) (nub . filter (alive . snd) . concat . fmap genNext $ cur) in
            M.unionWith (S.union) newExterior (inner next newVisited newExterior)
        where
            fn (ext, next) (dir, c) =
                if S.member c all then (M.insertWith (S.union) c (S.singleton dir) ext, next)
                else (ext, c:next)
            genNext (x,y,z) = [(W, (x-1,y,z)), (E, (x+1,y,z)), (N,(x,y-1,z)), (S,(x,y+1,z)), (U,(x,y,z-1)), (D,(x,y,z+1))]
            newVisited = S.union visited (S.fromList cur)
            alive c@(x,y,z) = (S.notMember c newVisited) && x <= myMax && y <= myMax && z <= myMax && x >= myMin && y >= myMin && z >= myMin

main :: IO ()
main = do
    input <- readFile "day18-input.txt"
    let parsed = fmap parseLine $ lines input
    let maps2d = build2dMaps parsed
    let part1 = getSurfaceAreaPart1 maps2d parsed
    print $ part1
    let maps3d = build3dMap parsed
    let part2 = getSurfaceAreaPart2 maps3d
    print $ part2