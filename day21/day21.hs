import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.RWS (RWS, evalRWS, get, modify, tell)

data Op = Plus | Minus | Div | Mul deriving (Eq, Show)
data Monkey = Value Int | Expr Op String String deriving (Eq, Show)
type Monkeys = Map String Monkey

data HumanOrNumber = Human | Number Int deriving (Eq, Show)

parseOp :: String -> Op
parseOp "+" = Plus
parseOp "-" = Minus
parseOp "*" = Mul
parseOp "/" = Div

applyOp :: Op -> Int -> Int -> Int
applyOp Plus = (+)
applyOp Minus = (-)
applyOp Div = div
applyOp Mul = (*)

-- n = a+human  ==>  human = n-a
-- n = a*human  ==>  human = n/a
-- n = a-human  ==>  human = -n+a
-- n = a/human  ==>  human = a/n
invertOp1 :: Op -> Int -> Int -> Int
invertOp1 Plus a = (\x -> (-) x a)
invertOp1 Mul a = (\x -> (div) x a)
invertOp1 Minus a = (\x -> (-x) + a)
invertOp1 Div a = (\x -> a `div` x)

-- n = human+b  ==>  human = n-b
-- n = human*b  ==>  human = n/b
-- n = human-b  ==>  human = n+b
-- n = human/b  ==>  human = n*b
invertOp2 :: Op -> Int -> Int -> Int
invertOp2 Plus b = (\x -> (-) x b)
invertOp2 Mul b = (\x -> (div) x b)
invertOp2 Minus b = (\x -> (+) x b)
invertOp2 Div b = (\x -> (*) x b)

parseMonkey :: String -> (String, Monkey)
parseMonkey s = if length sp == 2 then (name, val) else (name, expr) where
    sp = splitOn " " (filter (/= ':') s)
    name = sp !! 0
    val = Value (read @Int (sp !! 1))
    expr = Expr (parseOp (sp !! 2)) (sp !! 1) (sp !! 3)

groupMonkeys :: [String] -> Monkeys
groupMonkeys s = M.fromList $ fmap parseMonkey s

children :: Monkeys -> String -> [String]
children ms name = case (ms M.! name) of
    (Value i) -> []
    (Expr _ l r) -> [l, r]

-- memoize - is it necessary?
eval :: Monkeys -> String -> Int
eval ms s = fst $ evalRWS (inner s) () M.empty where
    inner :: String -> RWS () () (Map String Int) Int
    inner name = case (ms M.! name) of
        (Value i) -> do
            modify (M.insert name i)
            return i
        (Expr op l r) -> do
            memo <- get
            case (M.lookup name memo) of
                Just i -> return i
                Nothing -> do
                    left <- inner l
                    right <- inner r
                    let res = applyOp op left right
                    modify (M.insert name res)
                    return res

-- evaulate in the non-human case; log inverted operations in the human case
eval2 :: Monkeys -> String -> (HumanOrNumber, [Int -> Int])
eval2 ms s = evalRWS (inner s) () () where
    inner :: String -> RWS () [Int -> Int] () HumanOrNumber
    inner "humn" = return Human
    inner name = case (ms M.! name) of
        (Value i) -> return (Number i)
        (Expr op l r) -> do
            left <- inner l
            right <- inner r
            case (left, right) of 
                (Number a, Number b) -> 
                    return (Number (applyOp op a b))
                (Number a, Human) -> do
                    tell [invertOp1 op a]
                    return Human
                (Human, Number b) -> do
                    tell [invertOp2 op b]
                    return Human

-- apply the inverted operations from the human branch to the value from the non-human branch
doPart2 :: Monkeys -> Int
doPart2 ms = let (n, log) = getLog in foldr (\c r -> c r) n log where 
    getChildren = fmap (eval2 ms) (children ms "root")
    getLog = case (getChildren) of
        [(Human, log), ((Number n), _)] -> (n, log)
        [((Number n), _), (Human, log)] -> (n, log)

main :: IO ()
main = do
    input <- readFile "day21-input.txt"
    let monkeys = groupMonkeys $ lines input
    let part1 = eval monkeys "root"
    print $ part1
    let part2 = doPart2 monkeys
    print $ part2
