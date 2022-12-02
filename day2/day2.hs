import Data.List
import Data.List.Split

data Hand = Rock | Paper | Scissors deriving Eq

instance Ord Hand where
    compare Rock Scissors = GT
    compare Scissors Paper = GT
    compare Paper Rock = GT
    compare Scissors Rock = LT 
    compare Rock Paper = LT 
    compare Paper Scissors = LT
    compare _ _ = EQ

score :: Bool -> [String] -> Int
score part1 [opponent, me] = hand + outcome where
    opponent' = parseOpponent opponent
    me' = if part1 then parseMePart1 me
          else opponent' >>= parseMePart2 me
    hand = case (me') of
        Just Rock -> 1
        Just Paper -> 2
        Just Scissors -> 3
        _ -> 0
    outcome = case (compare me' opponent') of
        GT -> 6
        EQ -> 3
        LT -> 0

parseMe :: String -> Maybe Hand
parseMe "X" = Just Rock
parseMe "Y" = Just Paper
parseMe "Z" = Just Scissors
parseMe _ = Nothing

parseOpponent :: String -> Maybe Hand
parseOpponent "A" = Just Rock
parseOpponent "B" = Just Paper
parseOpponent "C" = Just Scissors
parseOpponent _ = Nothing

parseMePart1 :: String -> Maybe Hand
parseMePart1 "X" = Just Rock
parseMePart1 "Y" = Just Paper
parseMePart1 "Z" = Just Scissors
parseMePart1 _ = Nothing

parseMePart2 :: String -> Hand -> Maybe Hand
parseMePart2 "X" h = find (< h)  [Rock, Paper, Scissors] 
parseMePart2 "Y" h = find (== h) [Rock, Paper, Scissors]
parseMePart2 "Z" h = find (> h)  [Rock, Paper, Scissors]
parseMePart2 _ _ = Nothing

main :: IO ()
main = do 
    input <- readFile "day2-input.txt"
    let grouped = fmap (splitOn " ") $ lines input
    let part1 = sum . fmap (score True) $ grouped
    let part2 = sum . fmap (score False) $ grouped
    print $ part1
    print $ part2
     
