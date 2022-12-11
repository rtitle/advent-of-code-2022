import Data.List
import Data.List.Split
import Control.Monad.RWS (RWS, evalRWS, get, modify, tell)

data Monkey = Monkey {
    mid :: Int,
    items :: [Int],
    operation :: Int -> Int,
    divisor :: Int,
    trueMonkey :: Int,
    falseMonkey :: Int
}

instance Show Monkey where
  show (Monkey mid items _ _ _ _) = show mid ++ ":" ++ show items

instance Eq Monkey where
    a == b = (mid a) == (mid b)

parseMonkey :: [String] -> Monkey
parseMonkey s = Monkey mid items operation test (throwMonkey 4) (throwMonkey 5) where
    mid = read @Int . drop 7 . init $ s !! 0
    items = read @[Int] ('[' : drop 18 (s !! 1) ++ "]")
    operation = parseOperation . drop 5 . splitOn " " $ s !! 2
    test = read @Int . last . (splitOn " ") $ s !! 3
    throwMonkey a = read @Int . last . (splitOn " ") $ s !! a

parseOperation :: [String] -> Int -> Int
parseOperation ["old", "*", "old"] = (^2)
parseOperation ["old", "+", n] = (+ (read @Int n))
parseOperation ["old", "*", n] = (* (read @Int n))

throwToMonkey :: Int -> Int -> Int -> [Monkey] -> [Monkey]
throwToMonkey n source target = fmap buildMonkey where
    buildMonkey m = Monkey (mid m) (updateItems m) (operation m) (divisor m) (trueMonkey m) (falseMonkey m)
    updateItems (Monkey mid items _ _ _ _) = if mid == target then items ++ [n] else if mid == source then drop 1 items else items

processMonkey :: Bool -> Int -> Int -> RWS () [Int] [Monkey] [()]
processMonkey part2 modulo mi = do
    monkeys <- get
    let m = head . filter (\x -> (mid x) == mi) $ monkeys
    traverse (inner m) (items m) where
        inner (Monkey mid items operation divisor trueMonkey falseMonkey) item = do
            let op = (operation item) `mod` modulo
            let newWorry = if part2 then op else op `div` 3
            let targetMonkey = if newWorry `mod` divisor == 0 then trueMonkey else falseMonkey
            monkeys <- modify (throwToMonkey newWorry mid targetMonkey)
            tell [mid]

monkeyBusiness :: Bool -> Int -> [Monkey] -> Int
monkeyBusiness part2 rounds ms = business . snd $ evalRWS (traverse (processMonkey part2 modulo) r) () ms where
    business = product . take 2 . reverse . sort . fmap (length) . group . sort
    modulo = product . fmap divisor $ ms
    r = concat . replicate rounds $ [0..length ms - 1]

main :: IO ()
main = do
    input <- readFile "day11-input.txt"
    let monkeys = (fmap parseMonkey) . (splitWhen null) $ lines input
    let part1 = monkeyBusiness False 20 monkeys
    let part2 = monkeyBusiness True 10000 monkeys
    print $ part1
    print $ part2