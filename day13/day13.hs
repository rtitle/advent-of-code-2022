import Data.Either
import Data.List
import Data.List.Split (splitWhen)
import Text.Parsec
import Text.Parsec.String (Parser)

data IntOrList = I Int | L [IntOrList] deriving (Eq, Show)

instance Ord IntOrList where
    compare (I a) (I b) = compare a b
    compare (L as) (L bs) = mconcat (zipWith compare as bs) <> compare (length as) (length bs)
    compare (I a) (L bs) = compare (L [I a]) (L bs)
    compare (L as) (I b) = compare (L as) (L [I b])

parseInput :: String -> IntOrList
parseInput s = fromRight (I 0) . fmap head $ parse (many1 value) "" s where
    value = list <|> int
    int = I <$> number
    list = L <$> between (char '[') (char ']') (value `sepBy` (char ','))

number :: Parser Int
number = do
    n <- many1 digit
    return (read n)

part1 :: [[IntOrList]] -> Int
part1 is = sum . fmap snd . filter correct $ zip is [1..] where
    correct ([a,b], i) = a<b

part2 :: [[IntOrList]] -> Int
part2 is = product . fmap snd . filter divider $ zip is' [1..] where
    d1 = parseInput "[[2]]"
    d2 = parseInput "[[6]]"
    is' = sort (d1 : d2 : concat is)
    divider (a, i) = a == d1 || a == d2

main :: IO ()
main = do
    input <- readFile "day13-input.txt"
    let grouped = fmap (fmap parseInput) . splitWhen null $ lines input
    print $ part1 grouped
    print $ part2 grouped