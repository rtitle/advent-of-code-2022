import Data.List
import Data.List.Split

main :: IO ()
main = 
  do input <- readFile "day1-input.txt"
     let grouped = fmap (fmap str2int) $ splitWhen null $ lines input
     let part1 = maximum . fmap sum $ grouped
     let part2 = sum . take 3 . reverse . sort . fmap sum $ grouped
     print $ part1
     print $ part2

str2int :: String -> Int
str2int n = read n :: Int