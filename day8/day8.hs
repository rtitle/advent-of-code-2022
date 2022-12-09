import Data.Char
import Data.List

type Grid = [[(Int, Int, Char)]]

visibleFromLeft :: [(Int, Int)] -> [(Int, Int, Char)] -> (Int, Int, [(Int, Int)])
visibleFromLeft indexesAlreadySeen row = foldl fn (minBound :: Int, 0, indexesAlreadySeen) row where
    fn (max, count, indexes) (x, y, c) = 
        let i = getValue c in
            if i > max then 
                if (x, y) `elem` indexes then
                    (i, count, indexes)
                else 
                    (i, count + 1, (x, y) : indexes)
            else
                (max, count, indexes)

annotateScenicScore :: Bool -> [(Int, Int, Char)] -> [(Int, Int, Int, Char)]
annotateScenicScore isHoriz row = fmap fn row where
    fn :: (Int, Int, Char) -> (Int, Int, Int, Char)
    fn (x, y, c) = ((fst3 $ scoreRight x y c) * (fst3 $ scoreLeft x y c), x, y, c)
    scoreRight x y c = foldl scoreRightFn (0, getValue c, False) (reverse . take (length row - (if isHoriz then x else y) - 1) . reverse $ row)
    scoreRightFn (cnt, prev, done) (x, _, ch) = if done then (cnt, prev, done) else if (getValue ch) < prev then (cnt + 1, prev, done) else (cnt + 1, prev, True)
    scoreLeft x y c = foldl scoreLeftFn (0, getValue c, False) (reverse (take (if isHoriz then x else y) row))
    scoreLeftFn (cnt, prev, done) (x, _, ch) = if done then (cnt, prev, done) else if (getValue ch) < prev then (cnt + 1, prev, done) else (cnt + 1, prev, True)

getValue :: Char -> Int
getValue c = ord c - ord '0'

zipWithIndex :: [String] -> Grid
zipWithIndex s = fmap (\(i, s) -> fmap (\(j, c) -> (j, i, c)) (zip [0..] s)) (zip [0..] s)

combine :: [(Int, Int, [(Int, Int)])] -> (Int, [(Int, Int)])
combine = foldl fn (0, []) where
    fn (total, arr) (_, curTotal, curArr) = (total + curTotal, nub (arr ++ curArr))

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

test :: [String]
test = [
    ['3','0','3','7','3'],
    ['2','5','5','1','2'],
    ['6','5','3','3','2'],
    ['3','3','5','4','9'],
    ['3','5','3','9','0']]

main :: IO ()
main = do
    input <- readFile "day8-input.txt"
    let grid = zipWithIndex $ lines input
    let (left, seen) = combine . fmap (visibleFromLeft []) $ grid
    let (right, seen2) = combine . fmap ((visibleFromLeft seen) . reverse) $ grid
    let (top, seen3) = combine . fmap (visibleFromLeft seen2) $ transpose grid
    let (bottom, seen4) = combine . fmap ((visibleFromLeft seen3) . reverse) $ transpose grid
    let part1 = left + right + top + bottom
    print $ part1
    let horizontalScore = fmap (annotateScenicScore True) grid
    let verticalScore = transpose (fmap (annotateScenicScore False) (transpose grid))
    let part2 = maximum (fmap (maximum . fmap (\((a, b, c, d), (w, x, y, z)) -> a * w)) (zipWith zip horizontalScore verticalScore))
    print $ part2