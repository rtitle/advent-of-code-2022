import Data.List

process :: Int -> String -> [String]
process n s = scanl fn [] s where
    fn xs a =
        if length xs < n then a : xs
        else a : init xs

firstDistinct :: Int -> String -> Maybe Int
firstDistinct n = fmap (+n) . findIndex unique . drop n . process n where
    unique a = nub a == a

main :: IO ()
main = do
    input <- readFile "day6-input.txt"
    let part1 = firstDistinct 4 input
    let part2 = firstDistinct 14 input
    print $ part1
    print $ part2