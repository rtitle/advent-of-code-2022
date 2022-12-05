import Data.List
import Data.List.Split
import qualified Data.Sequence as S

type Ship = S.Seq [Char]

data Instruction = Instruction {
    src :: Int,
    dest :: Int,
    quantity :: Int
} deriving Show

initialShip :: Ship
initialShip = S.fromList [
    "TQVCDSN",
    "VFM",
    "MHNPDWQF",
    "FTRQD",
    "BVHQNMFR",
    "QWPNGFC",
    "TCLRFW",
    "SNZT",
    "NHQRJDSM"]

parseInstruction :: String -> Instruction
parseInstruction s = Instruction src dest q where
    split = splitOn " " s 
    src = read @Int $ split !! 3
    dest = read @Int $ split !! 5
    q = read @Int $ split !! 1

applyInstruction :: Bool -> Instruction -> Ship -> Ship
applyInstruction p2 i s = (S.update (dest i - 1) newDest) . (S.update (src i - 1) newSrc) $ s where
    moved = take (quantity i) (s `S.index` (src i - 1))
    newSrc = drop (quantity i) (s `S.index` (src i - 1))
    newDest = (if p2 then moved else reverse moved) ++ (s `S.index` (dest i - 1))

main :: IO ()
main = do
    input <- readFile "day5-input.txt"
    let instructions = fmap parseInstruction . filter (isPrefixOf "move") $ lines input
    let part1 = fmap head $ foldr (applyInstruction False) initialShip (reverse instructions)
    let part2 = fmap head $ foldr (applyInstruction True) initialShip (reverse instructions)
    print $ part1
    print $ part2