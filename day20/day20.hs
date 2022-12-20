import Data.CircularList (CList)
import qualified Data.CircularList as CL
import Data.Maybe (fromJust)

shift :: (Int, Int) -> CList (Int, Int) -> CList (Int, Int)
shift i@(n, _) clist = (CL.insertR i) . (CL.rotNR n') . CL.removeR  $ clist' where
    clist' = unsafeRotateTo i clist
    n' = n `mod` (CL.size clist - 1)

unsafeFocus :: CList (Int, Int) -> (Int, Int)
unsafeFocus = fromJust . CL.focus

unsafeRotateTo :: (Int, Int) -> CList (Int, Int) -> CList (Int, Int)
unsafeRotateTo n clist = fromJust $ CL.rotateTo n clist

unsafeRotateToZero :: CList (Int, Int) -> CList (Int, Int)
unsafeRotateToZero clist = fromJust $ CL.findRotateTo (\a -> fst a == 0) clist

mix :: Int -> [(Int, Int)] -> CList (Int, Int) -> CList (Int, Int)
mix n indices clist = foldl fn clist (concat $ replicate n indices) where
    fn r c = shift c r

getCoords :: CList (Int, Int) -> Int
getCoords clist = rot 1000 + rot 2000 + rot 3000 where
    rot n = fst . unsafeFocus $ (CL.rotNR n (unsafeRotateToZero clist))

main :: IO ()
main = do
    input <- readFile "day20-input.txt"
    let parsed = fmap (read @Int) $ lines input
    let indices = zip parsed [0..]
    let clist = CL.fromList indices
    let part1 = getCoords (mix 1 indices clist)
    print $ part1
    let parsed2 = fmap (*811589153) parsed
    let indices2 = zip parsed2 [0..]
    let clist2 = CL.fromList indices2
    let part2 = getCoords (mix 10 indices2 clist2)
    print $ part2
