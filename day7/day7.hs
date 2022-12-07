import Data.List
import Data.List.Split
import Data.Maybe

data FSItem = File String Int | Folder String [FSItem] deriving (Show)

data Line = Ls | CdRoot | CdParent | Cd String | Dir String | F String Int deriving (Show)

instance Eq FSItem where
    (==) (File name1 _) (File name2 _) = name1 == name2
    (==) (Folder name1 _) (Folder name2 _) = name1 == name2
    (==) _ _ = False 

parseLine :: String -> Line
parseLine s = case (splitOn " " s) of 
    ["$", "cd", "/"] -> CdRoot
    ["$", "cd", ".."] -> CdParent
    ["$", "ls"] -> Ls
    ["$", "cd", d] -> Cd d
    ["dir", d] -> Dir d
    [s, f] -> F f (read @Int s)

replaceChild :: FSItem -> FSItem -> FSItem
replaceChild child (Folder n children) = Folder n (child : (filter (/= child) children))
replaceChild _ f = f

name :: FSItem -> String
name (Folder n _) = n
name (File n _) = n

getChild :: String -> FSItem -> Maybe FSItem
getChild s (Folder _ children) = find (\c -> name c == s) children
getChild _ _ = Nothing

fixParents :: FSItem -> [FSItem] -> [FSItem]
fixParents newChild (x : xs) =
    let fixed = replaceChild newChild x in
        fixed : fixParents fixed xs
fixParents _ [] = []

process :: [Line] -> ([FSItem], FSItem)
process s = foldl fn zero s where
    zero = ([], Folder "/" [])
    fn (parents, node) s = case (s) of 
        CdRoot -> zero
        Ls -> (parents, node)
        CdParent -> (drop 1 parents, head parents)
        Cd d -> 
            let newChild = maybe (Folder d []) id (getChild d node) in
                (fixParents newChild (node : parents), newChild)
        Dir d -> 
            let newChild = maybe (Folder d []) id (getChild d node)
                newNode = replaceChild newChild node in
                    (fixParents newNode parents, newNode)
        F n sz -> 
            let newChild = maybe (File n sz) id (getChild n node)
                newNode = replaceChild newChild node in
                    (fixParents newNode parents, newNode)

sumNodes :: Int -> (Int, Int) -> FSItem -> (Int, Int)
sumNodes tt (n, total) (Folder _ children) = 
    let (a, b) = tupleSum . fmap (sumNodes tt (0, 0)) $ children in
        (n + a, if a <= tt then a + b else b)
    where
        tupleSum = foldl (\(r1, r2) (c1, c2) -> (r1 + c1, r2 + c2)) (0,0)
sumNodes _ (_, total) (File _ sz) = (sz, total)

findDir :: Int -> (Int, Int) -> FSItem -> (Int, Int)
findDir mn (n, curMin) (Folder _ children) =
    let (a, b) = tupleMin . fmap (findDir mn (0, curMin)) $ children in
        (n + a, if a >= mn then min b a else b)
    where
        tupleMin = foldl (\(r1, r2) (c1, c2) -> (r1 + c1, min r2 c2)) (0, maxBound :: Int)
findDir _ (_, curMin) (File _ sz) = (sz, maxBound :: Int)

test :: [String]
test = [
  "$ cd /",
  "$ ls",
  "1 a",
  "2 b",
  "dir c",
  "$ cd c",
  "$ ls",
  "1000000 d",
  "$ cd e",
  "$ ls",
  "2 z",
  "$ cd ..",
  "$ cd ..",
  "$ cd c"]

main :: IO ()
main = do
    input <- readFile "day7-input.txt"
    let (parents, _) = process . (fmap parseLine) $ lines input -- test
    let root = head . reverse $ parents
    let (total, part1) = sumNodes 100000 (0, 0) root
    let (_, part2) = findDir (30000000 - (70000000 - total)) (0, maxBound :: Int) root
    print $ part1
    print $ part2