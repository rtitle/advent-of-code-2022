import Data.List
import Data.List.Split
import Control.Monad.RWS (RWS, evalRWS, get, put, tell)

data Direction = U | D | L | R deriving (Eq, Show)
type Coordinate = (Int, Int)

zero :: Coordinate
zero = (0, 0)

parseDirection :: String -> Direction
parseDirection "U" = U
parseDirection "D" = D
parseDirection "L" = L
parseDirection "R" = R

move :: Direction -> Coordinate -> Coordinate
move U (x, y) = (x, (y+1))
move D (x, y) = (x, (y-1))
move L (x, y) = ((x-1), y)
move R (x, y) = ((x+1), y)

cmp :: Int -> Int -> Int
cmp a b = if a < b then -1 else if a > b then 1 else 0 

computeTail :: Coordinate -> Coordinate -> Coordinate
computeTail (hx, hy) (tx, ty) = 
    if abs (hx - tx) <= 1 && abs (hy - ty) <= 1 then (tx, ty)
    else ((tx + hx `cmp` tx), (ty + hy `cmp` ty))

parseInstruction :: String -> [Direction]
parseInstruction s = replicate dist dir where
    sp = splitOn " " s
    dir = parseDirection (sp !! 0)
    dist = read @Int (sp !! 1)

applyInstructions :: Int -> [Direction] -> Int
applyInstructions n is = length . nub . snd $ evalRWS (traverse inner is) () (replicate n zero) where
    inner :: Direction -> RWS () [Coordinate] [Coordinate] ()
    inner dir = do
        s <- get
        let h = head . reverse . take 1 . reverse $ s
        let t = init s
        let newHead = move dir h
        let newTails = foldr (\t' hs -> (computeTail (head hs) t') : hs) [newHead] t
        put newTails
        tell [head newTails]

test = [
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"]

main :: IO ()
main = do
    input <- readFile "day9-input.txt"
    let instructions =  concat . (fmap parseInstruction) $ lines input
    let part1 = applyInstructions 2 $ instructions
    let part2 = applyInstructions 10 $ instructions
    print $ part1
    print $ part2