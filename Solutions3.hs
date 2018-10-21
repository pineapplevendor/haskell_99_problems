module Solutions3
(insertAt,
 range,
 rndSelect,
 diffSelect,
 rndPermu,
 combinations,
 lsort,
 lfsort,
) where

import System.Random
import Solutions1
import Solutions2

--problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n = slice ys 0 (n-1) ++ [x] ++ slice ys (n-1) len
    where len = countElements ys

--problem 22
range :: Int -> Int -> [Int]
range x y = [x..y]

--problem 23
rndSelect :: [a] -> Int -> [a]
rndSelect xs n = rndSelectHelper xs rndIdxs
    where rndIdxs = take n $ getRndIdxs $ countElements xs

rndSelectHelper :: [a] -> [Int] -> [a]
rndSelectHelper _ [] = []
rndSelectHelper xs (y:ys) = getKth xs y:rndSelectHelper xs ys

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

--problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = 
    let startingWithCurrent = prependEltToLists x (combinations (n-1) xs)
        skippingCurrent = combinations n xs
    in startingWithCurrent ++ skippingCurrent

prependEltToLists :: a -> [[a]] -> [[a]]
prependEltToLists x [] = []
prependEltToLists x (y:ys) = (x:y) : (prependEltToLists x ys)

--problem 27
--skipping because I'm trying to learn a language, not mathematics

--problem 28
--problem 28 a
lsort :: [[a]] -> [[a]]
lsort xs = qsort compareListLen xs

compareListLen :: [a] -> [a] -> Bool
compareListLen xs ys = countElements xs < countElements ys

qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort _ [] = []
qsort f (x:xs) = qsort f (filter lT xs) ++ [x] ++ qsort f (filter (not . lT) xs)
    where lT y = f y x

--problem 28 b
lfsort :: (Eq a, Ord a) => [[a]] -> [[a]]
lfsort xs = 
    let freqTuples = encode $ qsort (<) xs
        sortedFreqTuples = qsort compareFreqTuples freqTuples
    in unpackFreqTuples sortedFreqTuples

compareFreqTuples :: (Int, a) -> (Int, a) -> Bool
compareFreqTuples x y = fst x < fst y

unpackFreqTuples :: [(Int, a)] -> [a]
unpackFreqTuples [] = []
unpackFreqTuples (x:xs) = replicate (fst x) (snd x) ++ (unpackFreqTuples xs)





