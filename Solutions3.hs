module Solutions3
(insertAt,
 range,
 rndSelect,
 diffSelect,
 rndPermu,
) where

import System.Random
import Solutions1
import Solutions2

--problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n = (slice ys 0 (n-1)) ++ [x] ++ (slice ys (n-1) len)
    where len = (countElements ys)

--problem 22
range :: Int -> Int -> [Int]
range x y = [x..y]

--problem 23
rndSelect :: [a] -> Int -> [a]
rndSelect xs n = rndSelectHelper xs rndIdxs
    where rndIdxs = take n $ getRndIdxs $ countElements xs

rndSelectHelper :: [a] -> [Int] -> [a]
rndSelectHelper _ [] = []
rndSelectHelper xs (y:ys) = (getKth xs y):(rndSelectHelper xs ys)

getRndIdxs :: Int -> [Int]
getRndIdxs n = randomRs (1,n) (mkStdGen 94) :: [Int]

--problem 24
diffSelect :: Int -> Int -> [Int]
diffSelect n m = diffSelectHelper [1..m] n (mkStdGen 94)

diffSelectHelper :: [a] -> Int -> StdGen -> [a]
diffSelectHelper [] _ _ = []
diffSelectHelper _ 0 _ = []
diffSelectHelper xs n gen = 
    let rndIdx = getRndIdx (countElements xs) gen
        selected = getKth xs (fst rndIdx)
        remaining = removeAt xs (fst rndIdx)
    in selected:diffSelectHelper remaining (n-1) (snd rndIdx)

getRndIdx :: Int -> StdGen -> (Int, StdGen)
getRndIdx n gen = randomR (1, n) gen

--problem 25
rndPermu :: [a] -> [a]
rndPermu xs = diffSelectHelper xs (countElements xs) (mkStdGen 94)

