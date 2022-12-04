import Data.List.Split

data Range = Range {
    start :: Int,
    end :: Int
} deriving Show

contains :: Range -> Range -> Bool
contains (Range a b) (Range x y) = a <= x && b >= y

overlaps :: Range -> Range -> Bool
overlaps (Range a b) (Range x y) = (a >= x && a <= y)

pairIsContained :: [Range] -> Bool
pairIsContained [r1, r2] = r1 `contains` r2 || r2 `contains` r1
pairIsContained _ = False

pairOverlaps :: [Range] -> Bool
pairOverlaps [r1, r2] = r1 `overlaps` r2 || r2 `overlaps` r1
pairOverlaps _ = False

parseRange :: String -> Range
parseRange s = Range a b where
    r = fmap (read @Int) (splitOn "-" s)
    a = r !! 0
    b = r !! 1

count :: (a -> Bool) -> [a] -> Int
count fn a = foldr (\x -> if fn(x) then (+1) else id) 0 a

main :: IO ()
main = do
    input <- readFile "day4-input.txt"
    let grouped = fmap (fmap parseRange . (splitOn ",")) $ lines input
    let part1 = count pairIsContained grouped
    let part2 = count pairOverlaps grouped
    print $ part1
    print $ part2