import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

type Coord = (Int, Int)
type Range = (Int, Int)

parseLine :: String -> (Coord, Coord)
parseLine s = ((sensorX, sensorY), (beaconX, beaconY)) where
    sp = splitOn " " s'
    s' = filter (not . (`elem` ",:")) s
    getVal expr = read @Int $ (splitOn "=" expr) !! 1
    sensorX = getVal (sp !! 2)
    sensorY = getVal (sp !! 3)
    beaconX = getVal (sp !! 8)
    beaconY = getVal (sp !! 9)

getCoverage :: Int -> Coord -> Coord -> [Int]
getCoverage y s@(sx, sy) b@(bx, by) = if by == y then filter (/=bx) area else area where
    height = abs (sy - y)
    m = md s b
    area = [sx-m+height..sx+m-height]

getRanges :: Coord -> Coord -> IntMap [Range]
getRanges s@(sx, sy) b@(bx, by) = M.fromList $ fmap (\y -> (y, [inner y])) [max 0 (sy-m)..min 4000000 (sy+m)] where
    m = md s b
    inner y = let height = abs (sy - y) in
        (max 0 (sx-m+height), min 4000000 (sx+m-height))

combineRanges :: [Range] -> [Range] -> [Range]
combineRanges rs1 rs2 = foldl fn [] (sort (rs1 ++ rs2)) where
    fn [] c = [c]
    fn acc c@(x,y) = let (a,b) = head . reverse $ acc in
        if rangeFullyContained c (a,b) || rangePartiallyContained c (a,b) then (init acc) ++ [((min x a), (max y b))] else acc ++ [c]
    rangePartiallyContained (a1, a2) (b1, b2) = if a1 <= b1 then b1 <= a2-1 else a1 <= b2+1
    rangeFullyContained (a1, a2) (b1, b2) = a1 <= b1 && a2 >= b2 || b1 <= a1 && a2 <= b2

doPart1 :: [(Coord, Coord)] -> Int
doPart1 cs = length . S.unions $ fmap (S.fromList . uncurry (getCoverage 2000000)) cs

doPart2 :: [(Coord, Coord)] -> Int
doPart2 cs = let [(y, ((_, x):t))] = M.toList entry in x*4000000 + y where
    entry = M.filter (/=[(0, 4000000)]) fullMap
    fullMap = M.unionsWith combineRanges $ fmap (uncurry getRanges) cs where

md :: Coord -> Coord -> Int
md (w,x) (y,z) = abs (w-y) + abs (x-z)

main :: IO ()
main = do
    input <- readFile "day15-input.txt"
    let parsed = fmap parseLine $ lines input
    let part1 = doPart1 parsed
    let part2 = doPart2 parsed
    print $ part1
    print $ part2