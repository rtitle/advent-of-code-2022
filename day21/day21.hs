import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.RWS (RWS, evalRWS, get, modify)

data Op = Plus | Minus | Div | Mul deriving (Eq, Show)
data Monkey = Value Int | Expr Op String String deriving (Eq, Show)
type Monkeys = Map String Monkey

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

parseMonkey :: String -> (String, Monkey)
parseMonkey s = if length sp == 2 then (name, val) else (name, expr) where
    sp = splitOn " " (filter (/= ':') s)
    name = sp !! 0
    val = Value (read @Int (sp !! 1))
    expr = Expr (parseOp (sp !! 2)) (sp !! 1) (sp !! 3)

groupMonkeys :: [String] -> Monkeys
groupMonkeys s = M.fromList $ fmap parseMonkey s

eval :: Monkeys -> Int
eval ms = fst $ evalRWS (inner "root") () M.empty where
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

main :: IO ()
main = do
    input <- readFile "day21-input.txt"
    let parsed = groupMonkeys $ lines input
    let part1 = eval parsed
    print $ part1