import Data.List
import Data.List.Split
import qualified Data.Sequence as S

type Ship = S.Seq String

data Instruction = Instruction {
    src :: Int,
    dest :: Int,
    quantity :: Int
} deriving Show

parseShip :: [String] -> Ship
parseShip s = S.fromList . fmap (filter (/= ' ')) $ transpose cols where
    s' = reverse . (drop 1) . reverse $ s
    cols = fmap (fmap (!! 1) . (chunksOf 4)) $ s'

parseInstruction :: String -> Instruction
parseInstruction s = Instruction (src - 1) (dest - 1) q where
    split = splitOn " " s 
    src = read @Int $ split !! 3
    dest = read @Int $ split !! 5
    q = read @Int $ split !! 1

applyInstruction :: Bool -> Instruction -> Ship -> Ship
applyInstruction p2 i s = (S.update (dest i) newDest) . (S.update (src i) newSrc) $ s where
    moved = take (quantity i) (s `S.index` (src i))
    newSrc = drop (quantity i) (s `S.index` (src i))
    newDest = (if p2 then moved else reverse moved) ++ (s `S.index` (dest i))

main :: IO ()
main = do
    input <- readFile "day5-input.txt"
    let [shipLines, instLines] = splitWhen null $ lines input
    let initialShip = parseShip shipLines
    let instructions = fmap parseInstruction instLines
    let part1 = fmap head $ foldr (applyInstruction False) initialShip (reverse instructions)
    let part2 = fmap head $ foldr (applyInstruction True) initialShip (reverse instructions)
    print $ part1
    print $ part2