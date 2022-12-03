import Data.Char
import Data.List
import Data.List.Split

compartments :: String -> (String, String)
compartments l = splitAt ((length l + 1) `div` 2) l

score :: Char -> Int
score c = if isUpper c then ord c - 38 else ord c - 96

main :: IO ()
main = do
    input <- readFile "day3-input.txt"
    let part1 = sum . fmap (score . head . uncurry intersect . compartments) $ lines input
    let part2 = sum . fmap (score . head . foldr1 intersect) $ chunksOf 3 $ lines input
    print $ part1
    print $ part2