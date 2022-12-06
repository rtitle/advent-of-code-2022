import Data.List

process :: Int -> String -> [String]
process n s = scanl fn [] s where
    fn xs a =
        if length xs < n then a : xs
        else a : (reverse . (drop 1) . reverse) xs

main :: IO ()
main = do
    input <- readFile "day6-input.txt"
    let part1 = findIndex (\x -> length x == 4 && (nub x == x)) $ process 4 input
    let part2 = findIndex (\x -> length x == 14 && (nub x == x)) $ process 14 input
    print $ part1
    print $ part2