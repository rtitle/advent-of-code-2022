import Data.List.Split (splitOn)
import Control.Monad.RWS (RWS, evalRWS, get, put, tell)

data Blueprint = Blueprint {
    bpId :: Int,
    costOre :: Int,
    costClay :: Int,
    costObsidianOre :: Int,
    costObsidianClay :: Int,
    costGeodeOre :: Int, 
    costGeodeObsidian :: Int
}

data State = State {
    numOre :: Int,
    numClay :: Int,
    numObsidian :: Int,
    numGeode :: Int,
    numOreRobots :: Int,
    numClayRobots :: Int,
    numObsidianRobots :: Int,
    numGeodeRobots :: Int,
    shouldBuyOre :: Bool,
    shouldBuyClay :: Bool,
    shouldBuyObsidian :: Bool
} deriving (Eq, Show)

parseBlueprint :: String -> Blueprint
parseBlueprint s = Blueprint bpId oc cc oco occ gco gcob where
    s' = filter (not . (`elem` ".:")) s
    sp = splitOn " " s'
    bpId = read @Int (sp !! 1)
    oc = read @Int (sp !! 6)
    cc = read @Int (sp !! 12)
    oco = read @Int (sp !! 18)
    occ = read @Int (sp !! 21)
    gco = read @Int (sp !! 27) 
    gcob = read @Int (sp !! 30)

initialState :: State
initialState = State 0 0 0 0 1 0 0 0 True True True

canAffordGeode :: Blueprint -> State -> Bool
canAffordGeode bp s = (numOre s) >= (costGeodeOre bp) && (numObsidian s) >= (costGeodeObsidian bp)

canAffordObsidian :: Blueprint -> State -> Bool
canAffordObsidian bp s = (numOre s) >= (costObsidianOre bp) && (numClay s) >= (costObsidianClay bp)

canAffordClay :: Blueprint -> State -> Bool
canAffordClay bp s = (numOre s) >= (costClay bp)

canAffordOre :: Blueprint -> State -> Bool
canAffordOre bp s = (numOre s) >= (costOre bp)

doNothing :: Blueprint -> State -> State
doNothing bp s = State (numOre s + numOreRobots s) (numClay s + numClayRobots s) (numObsidian s + numObsidianRobots s) (numGeode s + numGeodeRobots s) (numOreRobots s) (numClayRobots s) (numObsidianRobots s) (numGeodeRobots s) (not (canAffordOre bp s)) (not (canAffordClay bp s)) (not (canAffordObsidian bp s))

buyGeode :: Blueprint -> State -> State
buyGeode bp s = State (numOre s - costGeodeOre bp + numOreRobots s) (numClay s + numClayRobots s) (numObsidian s - costGeodeObsidian bp + numObsidianRobots s) (numGeode s + numGeodeRobots s) (numOreRobots s) (numClayRobots s) (numObsidianRobots s) (numGeodeRobots s + 1) True True True

buyObsidian :: Blueprint -> State -> State
buyObsidian bp s = State (numOre s - costObsidianOre bp + numOreRobots s) (numClay s - costObsidianClay bp + numClayRobots s) (numObsidian s + numObsidianRobots s) (numGeode s + numGeodeRobots s) (numOreRobots s) (numClayRobots s) (numObsidianRobots s + 1) (numGeodeRobots s) True True True

buyClay :: Blueprint -> State -> State
buyClay bp s = State (numOre s + numOreRobots s - costClay bp) (numClay s + numClayRobots s) (numObsidian s + numObsidianRobots s) (numGeode s + numGeodeRobots s) (numOreRobots s) (numClayRobots s + 1) (numObsidianRobots s) (numGeodeRobots s) True True True

buyOre :: Blueprint -> State -> State
buyOre bp s = State (numOre s - costOre bp + numOreRobots s) (numClay s + numClayRobots s) (numObsidian s + numObsidianRobots s) (numGeode s + numGeodeRobots s) (numOreRobots s + 1) (numClayRobots s) (numObsidianRobots s) (numGeodeRobots s) True True True

shouldBuyOreRobot :: Blueprint -> State -> Bool
shouldBuyOreRobot bp s = (shouldBuyOre s)  && (numOreRobots s) < maximum [(costOre bp), (costClay bp), (costObsidianOre bp), (costGeodeOre bp)]

shouldBuyClayRobot :: Blueprint -> State -> Bool
shouldBuyClayRobot bp s = (shouldBuyClay s) && (numClayRobots s) < (costObsidianClay bp)

shouldBuyObsidianRobot :: Blueprint -> State -> Bool
shouldBuyObsidianRobot bp s = (shouldBuyObsidian s) && (numObsidianRobots s) < (costGeodeObsidian bp)

-- apply some heuristics
genNextState :: Blueprint -> State -> [State]
genNextState bp s = concat [
    if canAffordGeode bp s then [buyGeode bp s]
    else 
        let 
            ob = if canAffordObsidian bp s && shouldBuyObsidianRobot bp s then [buyObsidian bp s] else []
            cl = if canAffordClay bp s && shouldBuyClayRobot bp s then [buyClay bp s] else []
            or = if canAffordOre bp s && shouldBuyOreRobot bp s then [buyOre bp s] else []
            no = if canAffordOre bp s && canAffordClay bp s && canAffordObsidian bp s then [] else [doNothing bp s]
        in ob ++ cl ++ or ++ no]

upperBound :: Int -> Blueprint -> State -> Int
upperBound depth bp s = (numGeode s) + x where
    x = sum $ fmap (\i -> numGeodeRobots s + i) (take (24 - depth) [1,2..])

-- dfs
findQuality :: Int -> Blueprint -> Int
findQuality maxDepth bp = fst (evalRWS (inner 0 initialState) () 0) where
    inner :: Int -> State -> RWS () [Int] Int Int
    inner depth s = if depth == maxDepth then baseCase s else do
        best <- get
        res <- if False then return 0 --TODO: why this not working? if upperBound depth bp s < best then return 0
                else do
                    next <- traverse (inner (depth+1)) (genNextState bp s)
                    return $ myMax next
        return res
    baseCase s = do
        best <- get
        let newBest = if (numGeode s) > best then (numGeode s) else best
        -- tell [newBest]
        put newBest
        return (numGeode s)
    myMax [] = 0
    myMax xs = maximum xs

main :: IO ()
main = do
    input <- readFile "day19-input.txt"
    let parsed = fmap parseBlueprint $ lines input
    let part1 = sum . fmap (\bp -> (bpId bp) * (findQuality 24 bp)) $ parsed
    print $ part1
    let part2 = product . fmap (findQuality 32) . take 3 $ parsed
    print $ part2