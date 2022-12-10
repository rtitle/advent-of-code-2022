import Data.List
import Data.List.Split
import Control.Monad.RWS (RWS, evalRWS, get, modify, put, tell)

data Instruction = Noop | Addx Int

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop
parseInstruction s = Addx x where
    s' = splitOn " " s
    x = read @Int (s' !! 1)

-- todo: factor part1 and part2 into common function

applyInstructions :: [Instruction] -> Int
applyInstructions is = sum . take 6 . snd $ evalRWS (traverse inner is) () (1, 1) where
    inner :: Instruction -> RWS () [Int] (Int, Int) ()
    inner Noop = do
        (i, x) <- get
        if i `mod` 40 == 20 then tell [x*i] else tell []
        put (i+1, x)      
    inner (Addx x') = do
        (i, x) <- get
        if i `mod` 40 == 20 then tell [x*i] else if (i+1) `mod` 40 == 20 then tell [x*(i+1)] else tell []
        put (i+2, x+x')

applyInstructions2 :: [Instruction] -> [Int]
applyInstructions2 is = snd $ evalRWS (traverse inner is) () (0, 1) where
    inner :: Instruction -> RWS () [Int] (Int, Int) ()
    inner Noop = do
        (p, x) <- get
        let h = p `mod` 40
        if h >= x-1 && h <= x+1 then tell [p] else tell []
        put ((p+1) `mod` 240, x)
    inner (Addx x') = do
        (p, x) <- get
        let h = p `mod` 40
        let h' = (p+1) `mod` 40
        if h >= x-1 && h <= x+1 then tell [p] else if h' >= x-1 && h' <= x+1 then tell [p+1] else tell []
        put ((p+2) `mod` 240, x+x')

drawCrt :: [Int] -> String
drawCrt is = insertAt 40 '\n' chars where
    chars = (foldl fn "" is) ++ "#"
    fn s i = s ++ (replicate (i - length s) '#') ++ "."

insertAt :: Int -> Char -> String -> String
insertAt 0 c s = s
insertAt n c [] = []
insertAt n c s
  | length s < n = s
  | otherwise = take n s ++ [c] ++ insertAt n c (drop n s)

main :: IO ()
main = do 
    input <- readFile "day10-input.txt"
    let instructions = fmap parseInstruction $ lines input
    let part1 = applyInstructions instructions
    let part2 = drawCrt . applyInstructions2 $ instructions
    print $ part1
    putStr $ part2