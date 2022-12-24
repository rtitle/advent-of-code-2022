import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

type Coord = (Int, Int)
data Direction = N | S | E | W deriving (Eq, Show, Ord)

data Blizzard = Blizzard {
    pos :: Coord,
    direction :: Direction
} deriving (Eq, Show, Ord)

data Grid = Grid {
    blizzards :: [Blizzard],
    covered :: Set Coord,
    xlen :: Int,
    ylen :: Int
} deriving (Eq, Show, Ord)

parseInitial :: [String] -> Grid
parseInitial s = let bs = blizzards in Grid bs (getCovered bs) (length . head $ s) (length s) where
    blizzards = foldl fn [] (zip s [0..]) 
    fn r (c, y) = r ++ parseLine y c
    parseLine y l = foldl fn [] (zip l [0..]) where
        fn r (c, x) = case c of
            '>' -> (Blizzard (x,y) E):r
            '<' -> (Blizzard (x,y) W):r
            '^' -> (Blizzard (x,y) N):r
            'v' -> (Blizzard (x,y) S):r
            otherwise -> r

getCovered :: [Blizzard] -> Set Coord
getCovered bs = S.fromList $ fmap pos bs

doStep :: Grid -> Grid
doStep (Grid blizzards _ xlen ylen) = let bs = fmap moveBlizzard blizzards in Grid bs (getCovered bs) xlen ylen where
    moveBlizzard (Blizzard pos dir) = Blizzard (wrap pos dir) dir
    wrap (x, y) dir = let (mvx, mvy) = mv dir in (wrapX x mvx, wrapY y mvy)
    wrapX x mv = let x' = x+mv in if x' == xlen-1 then 1 else if x' == 0 then xlen-2 else x'
    wrapY y mv = let y' = y+mv in if y' == ylen-1 then 1 else if y' == 0 then ylen-2 else y'
    mv N = (0, -1)
    mv S = (0, 1)
    mv E = (1, 0)
    mv W = (-1, 0)

allSteps :: Grid -> IntMap Grid
allSteps g = inner g S.empty 0 where
    inner cur seen n = if cur `S.member` seen then M.empty else
        M.insert n cur (inner (doStep cur) (S.insert cur seen) (n+1))

bfs :: IntMap Grid -> Int -> Coord -> Coord -> Int
bfs a startIdx start end = inner (S.singleton start) startIdx S.empty where
    xl = xlen (a M.! 0)
    yl = ylen (a M.! 0)
    inner cs depth seen = 
        if cs == S.empty || end `S.member` cs then depth else
            let idx = depth `mod` length a
                nextIdx = (depth+1) `mod` length a
                nextGrid = a M.! nextIdx
                nextCs = S.unions (S.map (S.fromList . (genNext nextGrid)) cs)
                nextCs' = S.filter (`S.notMember` seen) (S.map (,nextIdx) nextCs) in
                    inner (S.map fst nextCs') (depth+1) (S.union (S.map (,idx) cs) seen)
    genNext g (x,y) = filter (\(i,j) -> i>=1 && i <= xl-2 && (j >= 1 || (j == 0 && i == 1)) && (j <= yl-2 || (j == yl-1 && i == xl-2)) && (i,j) `S.notMember` (covered g)) [(x, y), (x-1, y), (x+1, y), (x, y-1), (x, y+1)]

main :: IO ()
main = do 
    input <- readFile "day24-input.txt"
    let initial = parseInitial $ lines input
    let all = allSteps initial
    let period = length all
    let start = (1, 0)
    let target = ((xlen initial)-2, (ylen initial)-1)
    let part1 = bfs all 0 start target
    print $ part1
    let b = bfs all (part1 `mod` period) target start
    let c = bfs all (b `mod` period) start target
    let part2 = part1 + b + c - (part1 `mod` period) - (b `mod` period)
    print $ part2