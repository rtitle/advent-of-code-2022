import Data.Char
import Data.List

type SNAFU = String

snafu2Int :: SNAFU -> Int
snafu2Int s = fst $ foldr fn (0, 1) s where
    fn c (r, place) = (r + place * digit c, place * 5)
    digit '-' = -1
    digit '=' = -2
    digit c = ord c - ord '0'

int2Snafu :: Int -> SNAFU
int2Snafu i = reverse $ unfoldr fn i where
    fn c = if c == 0 then Nothing else 
        let r = remainder (c `mod` 5) 
            next = c `div` 5 in 
                Just (r, if r == '-' || r == '=' then next+1 else next)
    remainder 3 = '='
    remainder 4 = '-'
    remainder n = chr (ord '0' + n)


main :: IO ()
main = do
    input <- readFile "day25-input.txt"
    let part1 = int2Snafu . sum . fmap snafu2Int $ lines input
    print $ part1