import Data.Function
import Data.List
import Data.List.Split
import Control.Monad.RWS (RWS, evalRWS, execRWS, get, modify, put, tell)

data Monkey = Monkey {
    mid :: Int,
    items :: [Int],
    operation :: Int -> Int,
    testDivBy :: Int,
    trueMonkey :: Int,
    falseMonkey :: Int
}

instance Show Monkey where
  show (Monkey mid items _ _ _ _) = show mid ++ ":" ++ show items

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
throwToMonkey n source target ms = fmap addItem ms where
    addItem m = Monkey (mid m) (items' m) (operation m) (testDivBy m) (trueMonkey m) (falseMonkey m)  
    items' m = if (mid m) == target then ((items m) ++ [n]) else if (mid m) == source then drop 1 (items m) else (items m)

processMonkey :: Monkey -> RWS () [Int] [Monkey] [()]
processMonkey m = do
    monkeys <- get
    let m' = head . filter (\x -> (mid x) == (mid m)) $ monkeys
    traverse (inner m') (items m') where
        inner m' item = do
            let op = ((operation m') item)
            -- let newWorry = if op `mod` (testDivBy m') == 0 then (testDivBy m') else op
            let newWorry = (op `mod` 9699690)
            let targetMonkey = if newWorry `mod` (testDivBy m') == 0 then (trueMonkey m') else (falseMonkey m')
            -- let newWorry = ((operation m') item) `div` 3
            -- let targetMonkey = if newWorry `mod` (testDivBy m') == 0 then (trueMonkey m') else (falseMonkey m')
            monkeys <- modify (throwToMonkey newWorry (mid m') targetMonkey)
            tell [(mid m')]

monkeyBusiness :: Int -> [Monkey] -> Int
monkeyBusiness rounds ms = product . take 2 . reverse . sort . fmap (length) . group . sort . snd $ evalRWS (traverse processMonkey (concat . replicate rounds $ ms)) () ms

-- monkeyBusiness :: Int -> [Monkey] -> [Int]
-- monkeyBusiness rounds ms = fmap (length) . group . sort . snd $ evalRWS (traverse processMonkey (concat . replicate rounds $ ms)) () ms

-- newWorry :: Int -> Int -> Int
-- newWorry 0 n = if n `mod` 23 == 0 then 23 else n
-- newWorry 1 n = if n `mod` 19 == 0 then 19 else n

-- monkeyBusiness :: Int -> [Monkey] -> [Monkey]
-- monkeyBusiness rounds ms = fst $ execRWS (traverse processMonkey (concat . replicate rounds $ ms)) () ms


main :: IO ()
main = do
    input <- readFile "day11-input.txt"
    let monkeys = (fmap parseMonkey) . (splitWhen null) $ lines input
    print $ monkeys
    let part1 = monkeyBusiness 1 monkeys
    let part2 = monkeyBusiness 10000 monkeys
    print $ part1
    print $ part2